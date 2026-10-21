# Engine: Segments
# CART segmentation, boundary analysis, rule evaluation
# Extracted from R/tab_smart_recommender.R and R/02_global_utils.R

# ── Helper: translate raw column IDs to friendly labels in a rule string ──
# Keeps SMART Risk Scanner rule display consistent with Opportunity Explorer
# and What-If Simulator, which both read labels from `wi_rule_vars`.
# Falls back silently to raw IDs when the lookup table isn't sourced yet.

# PCRE-mode regex escape — used to make rule var IDs safe inside a pattern.
# All wi_rule_vars IDs today are alphanumeric/underscore, but escaping defensively
# keeps the helper safe against future additions (e.g. dotted bureau codes).
.regex_escape <- function(s) {
  gsub("([][{}()+*?.\\^$|\\\\/-])", "\\\\\\1", s, perl = TRUE)
}

translate_rule_var_labels <- function(rule_text) {
  if (is.null(rule_text) || is.na(rule_text) || rule_text == "") return(rule_text)
  vars <- tryCatch(
    if (exists("wi_rule_vars", envir = .GlobalEnv, inherits = FALSE))
      get("wi_rule_vars", envir = .GlobalEnv) else NULL,
    error = function(e) NULL)
  if (is.null(vars) || length(vars) == 0) return(rule_text)

  # Order longest-id-first so substrings don't clobber enclosing matches
  # (e.g. "risk_model_score_3" before "risk_model_score_1").
  ids    <- vapply(vars, function(v) v$id,    character(1))
  labels <- vapply(vars, function(v) v$label, character(1))
  ord    <- order(-nchar(ids))
  ids    <- ids[ord]; labels <- labels[ord]

  out <- rule_text
  for (k in seq_along(ids)) {
    # Word-boundary match so partial names aren't replaced.
    pat <- paste0("\\b", .regex_escape(ids[k]), "\\b")
    out <- gsub(pat, labels[k], out, perl = TRUE)
  }
  out
}

#' Extract the ordered list of rule-variable ids referenced in a CART rule string.
#' Uses wi_rule_vars (if available) to scope which identifiers count as variables.
#' Returns a character vector of ids in first-appearance order.
extract_rule_vars <- function(rule_text) {
  if (is.null(rule_text) || length(rule_text) == 0) return(character(0))
  if (is.na(rule_text) || !nzchar(rule_text)) return(character(0))
  vars <- tryCatch(
    if (exists("wi_rule_vars", envir = .GlobalEnv, inherits = FALSE))
      get("wi_rule_vars", envir = .GlobalEnv) else NULL,
    error = function(e) NULL)
  if (is.null(vars) || length(vars) == 0) return(character(0))
  ids <- vapply(vars, function(v) v$id, character(1))

  # Match longest ids first so risk_model_score_3 isn't matched as risk_model_score_1.
  ids_sorted <- ids[order(-nchar(ids))]
  positions  <- integer(0)
  names_out  <- character(0)
  for (id in ids_sorted) {
    pat <- paste0("\\b", .regex_escape(id), "\\b")
    pos <- regexpr(pat, rule_text, perl = TRUE)
    if (pos > 0) {
      positions <- c(positions, as.integer(pos))
      names_out <- c(names_out, id)
    }
  }
  if (length(positions) == 0) return(character(0))
  names_out[order(positions)]
}

# ── Helper: format CART rules for display ──
# Converts scientific notation to readable numbers and swaps raw column IDs
# for friendly labels so Scanner / Opportunity Explorer speak the same language.
# Defined here (not in 02_global_utils.R) because extract_segments() needs it
# and engine/ is sourced before utils, causing lookup failures under setTimeLimit.
format_rule_display <- function(rule_text) {
  if (is.null(rule_text) || is.na(rule_text) || rule_text == "") return(rule_text)
  # Number formatting first (works on raw IDs), then label translation so the
  # final string shows "Risk Score 3 >= 0.001713" instead of "risk_model_score_3>=".

  currency_fields <- c("annual_income", "monthly_debt_payment", "issued_amount",
                        "loan_amount", "income", "amount", "payment")
  result <- rule_text

  # Pass 1: Currency fields with scientific notation -> $X,XXX
  for (field in currency_fields) {
    pattern <- paste0("(", field, "\\s*[<>=!]+\\s*)([0-9]+\\.?[0-9]*[eE][+\\-]?[0-9]+)")
    while (grepl(pattern, result)) {
      m <- regmatches(result, regexec(pattern, result))[[1]]
      if (length(m) >= 3) {
        num_val <- as.numeric(m[3])
        formatted <- paste0("$", formatC(round(num_val), format = "f", digits = 0, big.mark = ","))
        result <- sub(pattern, paste0(m[2], formatted), result, fixed = FALSE)
      } else break
    }
  }

  # Pass 2: Any remaining scientific notation -> plain comma-separated number
  general_pattern <- "([<>=!]+\\s*)([0-9]+\\.?[0-9]*[eE][+\\-]?[0-9]+)"
  while (grepl(general_pattern, result)) {
    m <- regmatches(result, regexec(general_pattern, result))[[1]]
    if (length(m) >= 3) {
      num_val <- as.numeric(m[3])
      digits <- if (abs(num_val - round(num_val)) < 0.01) 0 else 2
      formatted <- formatC(round(num_val, digits), format = "f", digits = digits, big.mark = ",")
      result <- sub(general_pattern, paste0(m[2], formatted), result, fixed = FALSE)
    } else break
  }

  translate_rule_var_labels(result)
}

# ── Helper: simplify redundant CART conditions ──
# e.g., "fico_score< 809.5 & fico_score< 749.5" → "fico_score< 749.5"
# For same variable: keep tightest < (min), tightest >= (max)
simplify_rule <- function(rule_text) {
  parts <- trimws(strsplit(rule_text, " & ")[[1]])
  if (length(parts) <= 1) return(rule_text)

  # Parse each condition: variable, operator, value
  parsed <- lapply(parts, function(p) {
    m <- regmatches(p, regexec("^(.+?)\\s*(<|>=)\\s*(.+)$", p))[[1]]
    if (length(m) == 4) {
      list(var = trimws(m[2]), op = m[3], val = as.numeric(m[4]), raw = p)
    } else {
      list(var = NA, op = NA, val = NA, raw = p)
    }
  })

  # Group by (variable, operator)
  keep <- rep(TRUE, length(parsed))
  for (i in seq_along(parsed)) {
    if (!keep[i] || is.na(parsed[[i]]$var)) next
    for (j in seq_along(parsed)) {
      if (i == j || !keep[j] || is.na(parsed[[j]]$var)) next
      if (parsed[[i]]$var == parsed[[j]]$var && parsed[[i]]$op == parsed[[j]]$op) {
        # Same var, same op: keep the tighter bound
        if (parsed[[i]]$op == "<") {
          # For <, tighter = smaller value
          if (parsed[[i]]$val <= parsed[[j]]$val) {
            keep[j] <- FALSE
          } else {
            keep[i] <- FALSE; break
          }
        } else {
          # For >=, tighter = larger value
          if (parsed[[i]]$val >= parsed[[j]]$val) {
            keep[j] <- FALSE
          } else {
            keep[i] <- FALSE; break
          }
        }
      }
    }
  }

  kept <- vapply(parsed[keep], function(x) x$raw, character(1))
  paste(kept, collapse = " & ")
}


# ── Helper: extract actionable segments from a fitted rpart tree ──
extract_segments <- function(tree, df, tree_vars, source_type,
                             portfolio_bad, portfolio_irr) {

  leaf_ids <- as.integer(rownames(tree$frame[tree$frame$var == "<leaf>", ]))
  if (length(leaf_ids) == 0) return(list())

  # Assign each row to a leaf
  node_assign <- assign_leaf_nodes(tree, df, tree_vars)

  # Get human-readable paths
  paths <- rpart::path.rpart(tree, nodes = leaf_ids, print.it = FALSE)

  segments <- lapply(leaf_ids, function(lid) {
    in_leaf   <- which(node_assign == lid)
    leaf_df   <- df[in_leaf, ]
    n         <- nrow(leaf_df)
    if (n < 20) return(NULL)

    bad_rate  <- mean(leaf_df$performance_delinquency_flag, na.rm = TRUE)
    mean_irr  <- mean(leaf_df$loan_irr, na.rm = TRUE)

    # Actionability: how bad is this segment relative to portfolio?
    if (source_type == "delinquency") {
      delta <- bad_rate - portfolio_bad
      if (delta <= 0) return(NULL)  # better than average — skip
      action_score <- delta * sqrt(n)
    } else {
      delta <- portfolio_irr - mean_irr
      if (is.na(delta) || delta <= 0) return(NULL)
      action_score <- delta * sqrt(n)
    }

    # Parse the rule
    path_vec <- paths[[as.character(lid)]]
    rule     <- paste(path_vec[-1], collapse = " & ")

    # Identify primary split variable (first condition)
    primary_var <- sub("[<>=!]+.*", "", trimws(path_vec[2]))
    primary_var <- trimws(primary_var)

    # Extract cutoff value from first condition
    cutoff_match <- regmatches(path_vec[2],
                               regexpr("[0-9]+\\.?[0-9]*", path_vec[2]))
    cutoff_val   <- if (length(cutoff_match) > 0) as.numeric(cutoff_match) else NA_real_

    # Direction: < means segment is below cutoff, >= means above
    direction <- if (grepl("<", path_vec[2])) "below" else "above"

    # Simplify redundant conditions, then format for display
    rule          <- simplify_rule(rule)
    display_rule  <- format_rule_display(rule)

    list(
      leaf_id       = lid,
      rule          = rule,
      display_rule  = display_rule,
      source        = source_type,
      n             = n,
      bad_rate      = bad_rate,
      mean_irr      = mean_irr,
      lift          = if (portfolio_bad > 0) round(bad_rate / portfolio_bad, 2) else NA_real_,
      action_score  = action_score,
      primary_var   = primary_var,
      cutoff_val    = cutoff_val,
      direction     = direction,
      loan_ids      = df$loan_id[in_leaf],
      avg_apr       = mean(leaf_df$interest_rate_apr, na.rm = TRUE),
      tree          = tree,
      tree_vars     = tree_vars
    )
  })

  Filter(Negate(is.null), segments)
}


# ── Helper: compute marginal bands around a cutoff ──
# When df contains an `in_segment` flag (from build_boundary_population),
# bands are assigned based on actual segment membership + distance from cutoff.
compute_marginal_bands <- function(df, var_name, cutoff_val, direction) {
  vals <- df[[var_name]]
  vals <- vals[!is.na(vals)]
  if (length(vals) < 50) return(NULL)

  has_membership <- "in_segment" %in% names(df)

  if (has_membership) {
    # Use actual CART membership: in-segment loans split by distance from cutoff
    sd_val <- sd(vals, na.rm = TRUE)
    df$band <- NA_character_
    pv <- df[[var_name]]
    pv_ok <- !is.na(pv)
    in_s  <- df$in_segment
    out_s <- !df$in_segment

    if (direction == "below") {
      # In-segment: primary_var < cutoff (from CART)
      df$band[in_s  & pv_ok & pv <  (cutoff_val - sd_val)] <- "Deep in-segment"
      df$band[in_s  & pv_ok & pv >= (cutoff_val - sd_val)] <- "Marginal in-segment"
      df$band[out_s & pv_ok & pv <  (cutoff_val + sd_val)] <- "Marginal out-segment"
      df$band[out_s & pv_ok & pv >= (cutoff_val + sd_val)] <- "Safe zone"
    } else {
      # In-segment: primary_var >= cutoff (from CART)
      df$band[in_s  & pv_ok & pv >= (cutoff_val + sd_val)] <- "Deep in-segment"
      df$band[in_s  & pv_ok & pv <  (cutoff_val + sd_val)] <- "Marginal in-segment"
      df$band[out_s & pv_ok & pv >= (cutoff_val - sd_val)] <- "Marginal out-segment"
      df$band[out_s & pv_ok & pv <  (cutoff_val - sd_val)] <- "Safe zone"
    }
  } else {
    # Fallback: pure cutoff-based bands (single-condition segments)
    sd_val <- sd(vals, na.rm = TRUE)
    if (direction == "below") {
      breaks <- c(-Inf, cutoff_val - sd_val, cutoff_val, cutoff_val + sd_val, Inf)
      labels <- c("Deep in-segment", "Marginal in-segment",
                  "Marginal out-segment", "Safe zone")
    } else {
      breaks <- c(-Inf, cutoff_val - sd_val, cutoff_val, cutoff_val + sd_val, Inf)
      labels <- c("Safe zone", "Marginal out-segment",
                  "Marginal in-segment", "Deep in-segment")
    }
    breaks <- unique(breaks)
    if (length(breaks) - 1 != length(labels)) return(NULL)
    df$band <- as.character(cut(df[[var_name]], breaks = breaks, labels = labels,
                                include.lowest = TRUE, right = FALSE))
  }

  df$band <- factor(df$band,
    levels = c("Deep in-segment", "Marginal in-segment",
               "Marginal out-segment", "Safe zone"))

  band_stats <- df %>%
    dplyr::filter(!is.na(band)) %>%
    dplyr::group_by(band) %>%
    dplyr::summarise(
      count    = dplyr::n(),
      bad_rate = mean(performance_delinquency_flag, na.rm = TRUE),
      avg_irr  = mean(loan_irr, na.rm = TRUE),
      avg_var  = mean(.data[[var_name]], na.rm = TRUE),
      .groups  = "drop"
    ) %>%
    dplyr::arrange(match(band, c("Deep in-segment", "Marginal in-segment",
                                  "Marginal out-segment", "Safe zone")))

  list(bands = band_stats, breaks = NULL, labels = levels(df$band))
}


# ── Helper: build boundary analysis population from segment loan_ids ──
# Uses the segment's actual CART-assigned loans (immune to surrogate split
# mismatches) and adds contextual loans along the primary variable axis
# so out-of-segment bands are populated for comparison.
build_boundary_population <- function(df, seg) {
  seg_ids <- as.character(seg$loan_ids)
  df$loan_id_chr <- as.character(df$loan_id)
  df$in_segment  <- df$loan_id_chr %in% seg_ids

  # Segment loans define the in-segment range on the primary variable
  seg_vals <- df[[seg$primary_var]][df$in_segment]
  seg_vals <- seg_vals[!is.na(seg_vals)]
  if (length(seg_vals) == 0) return(df)

  # Expand to include out-of-segment context: 1 SD beyond segment range
  sd_val <- sd(df[[seg$primary_var]], na.rm = TRUE)
  seg_min <- min(seg_vals, na.rm = TRUE)
  seg_max <- max(seg_vals, na.rm = TRUE)
  range_lo <- seg_min - sd_val
  range_hi <- seg_max + sd_val

  pv <- df[[seg$primary_var]]
  df <- df[!is.na(pv) & pv >= range_lo & pv <= range_hi, ]
  df
}


# ── Assign rpart leaf node IDs to new data using the tree's splitting rules ──
# More robust than floating-point fingerprint matching.
assign_leaf_nodes <- function(tree, newdata, tree_vars) {
  frame_ids <- as.integer(rownames(tree$frame))
  nd <- newdata[, tree_vars, drop = FALSE]
  # Use rpart internal pred.rpart to send newdata down the tree
  tryCatch({
    where_new <- rpart:::pred.rpart(tree, rpart:::rpart.matrix(nd))
    frame_ids[where_new]
  }, error = function(e) {
    # Fallback: use predicted values to match leaves (with exact matching)
    node_assign <- frame_ids[tree$where]
    if (tree$method == "class") {
      pp <- predict(tree, nd, type = "prob", na.action = na.pass)
      train_pp <- predict(tree, type = "prob")
      train_keys <- apply(round(train_pp, 12), 1, paste, collapse = "|")
      new_keys <- apply(round(pp, 12), 1, paste, collapse = "|")
      lookup <- setNames(node_assign, train_keys)
      lookup <- lookup[!duplicated(names(lookup))]
      as.integer(lookup[new_keys])
    } else {
      pv <- predict(tree, nd)
      train_pv <- predict(tree)
      lookup <- setNames(node_assign, as.character(round(train_pv, 12)))
      lookup <- lookup[!duplicated(names(lookup))]
      as.integer(lookup[as.character(round(pv, 12))])
    }
  })
}


# Normalize a rule operator to one of the five canonical strings, or NA if unknown.
.normalize_rule_op <- function(op) {
  if (is.null(op) || length(op) == 0 || is.na(op)) return(NA_character_)
  x <- as.character(op)[1]
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("\u2264", "<=", x, fixed = TRUE)
  x <- gsub("\u2265", ">=", x, fixed = TRUE)
  x <- trimws(x)
  if (x %in% c("<", "<=", ">", ">=", "==")) x else NA_character_
}

# ── Test if rows of df match segment rule conditions ──
evaluate_segment_conditions <- function(df, conditions) {
  dim1_col <- conditions$dim1
  val1     <- conditions$val1
  op1      <- .normalize_rule_op(conditions$op1 %||% "==")
  type1    <- conditions$type1 %||% "categorical"
  if (is.na(op1)) return(rep(FALSE, nrow(df)))

  if (is.null(dim1_col) || !(dim1_col %in% names(df))) return(rep(FALSE, nrow(df)))

  if (type1 == "numeric") {
    col_vals <- as.numeric(df[[dim1_col]])
    num_val  <- as.numeric(val1)
    mask1 <- switch(op1,
      "<"  = col_vals < num_val,
      "<=" = col_vals <= num_val,
      ">"  = col_vals > num_val,
      ">=" = col_vals >= num_val,
      "==" = col_vals == num_val,
      rep(FALSE, nrow(df))
    )
  } else {
    mask1 <- as.character(df[[dim1_col]]) == as.character(val1)
  }
  mask1[is.na(mask1)] <- FALSE

  if (!is.null(conditions$dim2) && nchar(conditions$dim2 %||% "") > 0 &&
      conditions$dim2 %in% names(df)) {
    val2  <- conditions$val2
    op2   <- .normalize_rule_op(conditions$op2 %||% "==")
    type2 <- conditions$type2 %||% "categorical"
    if (is.na(op2)) return(mask1)

    if (type2 == "numeric") {
      col_vals2 <- as.numeric(df[[conditions$dim2]])
      num_val2  <- as.numeric(val2)
      mask2 <- switch(op2,
        "<"  = col_vals2 < num_val2,
        "<=" = col_vals2 <= num_val2,
        ">"  = col_vals2 > num_val2,
        ">=" = col_vals2 >= num_val2,
        "==" = col_vals2 == num_val2,
        rep(FALSE, nrow(df))
      )
    } else {
      mask2 <- as.character(df[[conditions$dim2]]) == as.character(val2)
    }
    mask2[is.na(mask2)] <- FALSE

    logic <- conditions$logic %||% "AND"
    if (identical(logic, "OR")) mask1 | mask2 else mask1 & mask2
  } else {
    mask1
  }
}
