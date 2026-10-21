# ============================================================
# R/services/discover_service.R — Week 3 implementation
#
# Risk discovery service - feeds Auto Scan, Guided Explorer,
# and Risk Drivers tabs. Pure functions, no Shiny imports.
#
# Composes these engine primitives:
#   engine/segments.R::extract_segments
#   engine/segments.R::compute_marginal_bands
#   engine/segments.R::build_boundary_population
#   engine/projection.R::compute_projected_bad_rate (optional enrichment)
# ============================================================

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# ---- SHAP availability check ------------------------------------------------
has_shap_packages <- function() {
  requireNamespace("xgboost", quietly = TRUE) &&
  requireNamespace("shapviz", quietly = TRUE)
}

# ---- driver_importance ------------------------------------------------------
#' Rank borrower attributes by predictive power for 2+ delinquency.
#'
#' @param df credit_data frame (must have performance_delinquency_flag + issued_flag)
#' @param mode "correlation" (fast, default) or "shap" (slower, needs xgboost)
#' @param filters Optional row filter predicates (not yet used - Week 4)
#' @return data.frame(variable, label, importance, closest_policy_var,
#'                    closest_overlap, status)
driver_importance <- function(df, mode = c("correlation", "shap"), filters = NULL) {
  mode <- match.arg(mode)
  if (!exists("analysis_vars", envir = globalenv())) {
    stop("analysis_vars not loaded")
  }
  av <- get("analysis_vars", envir = globalenv())

  # Only issued loans with perf data
  issued <- df[!is.na(df$issued_flag) & df$issued_flag == 1 &
               !is.na(df$performance_delinquency_flag), , drop = FALSE]
  if (nrow(issued) < 100) {
    return(.empty_drivers_df())
  }

  cols <- intersect(unname(av), names(issued))
  cols <- cols[vapply(cols, function(c) is.numeric(issued[[c]]), logical(1))]

  y <- issued$performance_delinquency_flag

  if (mode == "shap" && !has_shap_packages()) {
    # Fall back silently
    mode <- "correlation"
  }

  if (mode == "correlation") {
    imp <- vapply(cols, function(c) {
      x <- issued[[c]]
      if (sum(!is.na(x)) < 50 || sd(x, na.rm = TRUE) == 0) return(NA_real_)
      abs(suppressWarnings(cor(x, y, use = "pairwise.complete.obs")))
    }, numeric(1))
  } else {
    # SHAP mode — fit a lightweight xgboost and compute mean |SHAP|
    feat <- as.matrix(issued[, cols, drop = FALSE])
    feat[!is.finite(feat)] <- NA
    dtrain <- xgboost::xgb.DMatrix(feat, label = y, missing = NA)
    bst <- xgboost::xgboost(
      data = dtrain,
      nrounds = 50,
      objective = "binary:logistic",
      eval_metric = "logloss",
      max_depth = 4,
      eta = 0.1,
      verbose = 0
    )
    sv <- shapviz::shapviz(bst, X_pred = feat)
    imp <- colMeans(abs(sv$S))
  }

  if (all(is.na(imp))) return(.empty_drivers_df())

  max_imp <- max(imp, na.rm = TRUE)
  imp_norm <- if (max_imp > 0) imp / max_imp else imp

  # Closest policy var = the existing policy variable most correlated with
  # this factor. Used to classify as "Investigate" vs "Already captured".
  policy_vars <- c("fico_score", "credit_file_age_months",
                   "delinquent_accounts_count", "debt_to_income_ratio",
                   "N_HARD_INQ_6M")
  policy_vars <- intersect(policy_vars, names(issued))

  result <- data.frame(
    variable           = cols,
    label              = names(av)[match(cols, unname(av))],
    importance         = as.numeric(imp_norm),
    closest_policy_var = NA_character_,
    closest_overlap    = NA_real_,
    status             = "Investigate",
    stringsAsFactors   = FALSE
  )

  for (i in seq_len(nrow(result))) {
    v <- result$variable[i]
    if (v %in% policy_vars) {
      result$closest_policy_var[i] <- v
      result$closest_overlap[i]    <- 1.0
      result$status[i]             <- "Already captured"
      next
    }
    best_cor <- 0
    best_pv  <- NA_character_
    for (pv in policy_vars) {
      c <- tryCatch(
        abs(suppressWarnings(cor(issued[[v]], issued[[pv]],
                                 use = "pairwise.complete.obs"))),
        error = function(e) NA_real_
      )
      if (!is.na(c) && c > best_cor) { best_cor <- c; best_pv <- pv }
    }
    result$closest_policy_var[i] <- best_pv
    result$closest_overlap[i]    <- best_cor
    if (!is.na(best_cor) && best_cor > 0.8) {
      result$status[i] <- "Already captured"
    }
  }

  result <- result[order(result$importance, decreasing = TRUE,
                         na.last = TRUE), , drop = FALSE]
  result <- result[!is.na(result$importance), , drop = FALSE]
  rownames(result) <- NULL
  result
}

.empty_drivers_df <- function() {
  data.frame(variable = character(0), label = character(0),
             importance = numeric(0), closest_policy_var = character(0),
             closest_overlap = numeric(0), status = character(0),
             stringsAsFactors = FALSE)
}

# ---- driver_drilldown -------------------------------------------------------
#' Distribution of a single variable for good vs bad loans.
#' @return list(var_name, good, bad, n_good, n_bad)
driver_drilldown <- function(df, variable) {
  if (is.null(variable) || !(variable %in% names(df))) return(NULL)
  issued <- df[!is.na(df$issued_flag) & df$issued_flag == 1 &
               !is.na(df$performance_delinquency_flag), , drop = FALSE]
  good <- issued[[variable]][issued$performance_delinquency_flag == 0]
  bad  <- issued[[variable]][issued$performance_delinquency_flag == 1]
  good <- good[is.finite(good)]
  bad  <- bad[is.finite(bad)]
  list(
    var_name = variable,
    good = good,
    bad = bad,
    n_good = length(good),
    n_bad  = length(bad)
  )
}

# ---- auto_scan --------------------------------------------------------------
#' CART-based risk segment discovery across product slices.
#'
#' Delegates to engine/scanner.R::scan_portfolio() with cross_slices = FALSE
#' (1-way only for service-layer performance). Data filtering stays here;
#' product and objective are added to the result.
auto_scan <- function(credit_data, perf_tape = NULL,
                      product = "Personal Loan",
                      objective = c("delinquency", "irr", "blended"),
                      max_segments = 15) {
  objective <- match.arg(objective)

  df <- credit_data
  if (!identical(product, "all") && "product_type" %in% names(df)) {
    df <- df[df$product_type == product, , drop = FALSE]
  }
  df <- df[!is.na(df$issued_flag) & df$issued_flag == 1 &
           !is.na(df$performance_delinquency_flag), , drop = FALSE]
  if (nrow(df) < 200) {
    return(.empty_scan(product, objective))
  }

  av <- get("analysis_vars", envir = globalenv())
  avail_vars <- intersect(unname(av), names(df))
  avail_vars <- avail_vars[vapply(avail_vars, function(v) is.numeric(df[[v]]), logical(1))]

  result <- scan_portfolio(
    df_issued       = df,
    avail_vars      = avail_vars,
    perf_tape       = perf_tape,
    cross_slices    = FALSE,
    objective       = objective,
    max_segments    = max_segments
  )

  result$product   <- product
  result$objective <- objective
  result
}

.empty_scan <- function(product, objective) {
  list(segments = list(), portfolio_bad = NA_real_,
       portfolio_irr = NA_real_, elapsed_seconds = 0,
       n_slices_scanned = 0, product = product, objective = objective)
}

# ---- marginal_bands_for_segment ---------------------------------------------
#' Wraps engine::build_boundary_population + compute_marginal_bands.
marginal_bands_for_segment <- function(segment, credit_data) {
  if (is.null(segment) || is.null(segment$primary_var) ||
      is.na(segment$cutoff_val)) return(NULL)
  df <- build_boundary_population(credit_data, segment)
  if (is.null(df) || nrow(df) < 50) return(NULL)
  mb <- compute_marginal_bands(df, segment$primary_var,
                                segment$cutoff_val, segment$direction)
  if (is.null(mb)) return(NULL)
  list(bands = mb$bands, var_name = segment$primary_var)
}

# ---- to_whatif_payload ------------------------------------------------------
#' Build a cross-tab payload the What-If Lab will consume in Week 4.
to_whatif_payload <- function(segment) {
  if (is.null(segment)) return(NULL)
  list(
    filters          = list(),
    suggested_param  = list(
      name      = segment$primary_var,
      value     = segment$cutoff_val,
      direction = segment$direction
    ),
    segment_id       = segment$leaf_id,
    rule_text        = segment$display_rule %||% segment$rule,
    origin           = "auto_scan"
  )
}
