# Engine: Actions
# Segment-level action helpers: elasticity, adverse selection, assumption lookup
# get_segment_assumption extracted from R/02_global_utils.R
# compute_elasticity_drop and compute_adverse_selection_lift are NEW reusable wrappers

# ── Grade-weighted config lookup for elasticity / adverse selection ──
# `primary_var` (optional): when the segment is built around a single dominant
# variable (e.g., the WI segment "N_HARD_INQ_6M >= 3"), pass the column name
# here so the waterfall's Layer 1 can match it against direct V4 test/control
# evidence. If NA, Layer 1 is bypassed and the lookup goes straight to the
# observational model (Tier 2).
get_segment_assumption <- function(segment_grades, param_name,
                                   consultant_config = NULL,
                                   primary_var = NA_character_) {
  # Load consultant_config as before if not provided
  if (is.null(consultant_config)) {
    if (exists("consultant_config", envir = .GlobalEnv)) {
      consultant_config <- .GlobalEnv$consultant_config
    } else {
      consultant_config <- list(
        elasticity = c(High = 3, Med = 5, Low = 8),
        adverse_selection = c(High = 2, Med = 5, Low = 8),
        anchor_fico_floor = 760
      )
    }
  }

  valid_grades <- segment_grades[!is.na(segment_grades)]

  # Compute grade_mix for waterfall consumers
  grade_mix <- if (length(valid_grades) == 0) {
    c(High = 0, Med = 1, Low = 0)
  } else {
    tbl <- table(factor(valid_grades, levels = c("High", "Med", "Low")))
    setNames(as.numeric(prop.table(tbl)), c("High", "Med", "Low"))
  }

  # Route to new waterfall for elasticity; preserve existing path for other params
  if (param_name == "elasticity") {
    ccg <- if (exists("calibration_cache_global", envir = .GlobalEnv))
      .GlobalEnv$calibration_cache_global else NULL
    cc  <- if (!is.null(ccg)) ccg$elasticity else NULL
    oec <- if (!is.null(ccg)) ccg$obs_elasticity else NULL

    # Normalise primary_var: empty string or non-character → NA.
    # Layer 1 uses case-insensitive match (see .elasticity_layer1).
    pv <- if (is.null(primary_var) || length(primary_var) == 0) NA_character_
          else as.character(primary_var)[1]
    if (!is.na(pv) && !nzchar(pv)) pv <- NA_character_

    result <- tryCatch(
      get_segment_elasticity(primary_var = pv,
                              grade_mix = grade_mix,
                              calibration_cache = cc,
                              obs_elasticity_cache = oec,
                              consultant_config = consultant_config),
      error = function(e) NULL
    )
    if (is.null(result) || is.null(result$value)) {
      # Defensive fallback — should not happen since L4 always returns
      val <- unname(consultant_config[["elasticity"]]["Med"])
      stopifnot(is.numeric(val) && length(val) == 1)
      return(as.numeric(val))
    }
    stopifnot(is.numeric(result$value) && length(result$value) == 1)
    return(as.numeric(result$value))
  }

  # Legacy path for non-elasticity params (adverse_selection, etc.) — unchanged
  if (length(valid_grades) == 0) {
    if (exists("calibration_cache_global", envir = .GlobalEnv) &&
        !is.null(get("calibration_cache_global", envir = .GlobalEnv))) {
      cc <- get("calibration_cache_global", envir = .GlobalEnv)
      sub_cache <- cc[[param_name]]
      val <- tryCatch(
        get_calibrated_assumption(param_name, "Med", sub_cache, consultant_config),
        error = function(e) NULL
      )
      if (!is.null(val) && !is.na(val)) return(val)
    }
    return(unname(consultant_config[[param_name]]["Med"]))
  }

  if (exists("calibration_cache_global", envir = .GlobalEnv) &&
      !is.null(get("calibration_cache_global", envir = .GlobalEnv))) {
    cc         <- get("calibration_cache_global", envir = .GlobalEnv)
    sub_cache  <- cc[[param_name]]
    grade_table   <- table(valid_grades)
    primary_grade <- names(which.max(grade_table))

    val <- tryCatch(
      get_calibrated_assumption(param_name, primary_grade,
                                sub_cache, consultant_config),
      error = function(e) NULL
    )
    if (!is.null(val) && !is.na(val)) return(val)
  }

  grade_counts <- table(valid_grades)
  grade_values <- consultant_config[[param_name]]
  weights <- as.numeric(grade_counts[names(grade_values)])
  weights[is.na(weights)] <- 0
  if (all(weights == 0)) {
    return(unname(consultant_config[[param_name]]["Med"]))
  }
  weighted.mean(as.numeric(grade_values), weights)
}

# Compute price elasticity drop for a segment
# Returns: list(drop_rate, n_drop, drop_idx)
compute_elasticity_drop <- function(segment_grades, bps_val, fico_scores,
                                     issued_mask, consultant_config) {
  elast_pct <- get_segment_assumption(segment_grades, "elasticity",
                                       consultant_config) / 100
  drop_rate <- min((abs(bps_val) / 100) * elast_pct, 0.50)
  n_issued  <- sum(issued_mask)
  n_drop    <- round(n_issued * drop_rate)
  if (n_drop == 0) return(list(drop_rate = drop_rate, n_drop = 0L, drop_idx = integer(0)))
  # Highest FICO leave first
  fico_order <- order(fico_scores[issued_mask], decreasing = TRUE)
  drop_idx   <- which(issued_mask)[fico_order[seq_len(min(n_drop, length(fico_order)))]]
  list(drop_rate = drop_rate, n_drop = n_drop, drop_idx = drop_idx)
}

# Compute adverse selection lift for a segment
# Returns: list(adverse_adj, n_flip, flip_idx)
compute_adverse_selection_lift <- function(segment_grades, bps_val, fico_scores,
                                            issued_mask, good_mask, consultant_config) {
  adv_pct <- get_segment_assumption(segment_grades, "adverse_selection",
                                     consultant_config) / 100
  if (adv_pct <= 0 || bps_val <= 0) {
    return(list(adverse_adj = 0, n_flip = 0L, flip_idx = integer(0)))
  }
  adverse_adj  <- (bps_val / 100) * adv_pct
  good_in_seg  <- which(issued_mask & good_mask)
  n_flip       <- round(length(good_in_seg) * adverse_adj)
  if (n_flip == 0) return(list(adverse_adj = adverse_adj, n_flip = 0L, flip_idx = integer(0)))
  # Lowest FICO good borrowers flip to bad
  fico_ord <- order(fico_scores[good_in_seg])
  flip_idx <- good_in_seg[fico_ord[seq_len(min(n_flip, length(fico_ord)))]]
  list(adverse_adj = adverse_adj, n_flip = n_flip, flip_idx = flip_idx)
}
