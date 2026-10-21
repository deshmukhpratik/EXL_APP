# ============================================================
# R/engine/root_cause.R
# Pure engine functions for Root Cause Explorer.
# No Shiny dependencies.
# ============================================================

# ---- Metric functions --------------------------------------------------
# Each takes a data frame slice and returns a scalar (NA if undefined).

rc_metric_dq <- function(df) {
  if (is.null(df) || !"performance_delinquency_flag" %in% names(df)) return(NA_real_)
  x <- df$performance_delinquency_flag
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  mean(as.numeric(x))
}

rc_metric_loss_rate <- function(df) {
  if (is.null(df)) return(NA_real_)
  need <- c("performance_delinquency_amount", "issued_amount")
  if (!all(need %in% names(df))) return(NA_real_)
  denom <- sum(df$issued_amount, na.rm = TRUE)
  if (!is.finite(denom) || denom <= 0) return(NA_real_)
  sum(df$performance_delinquency_amount, na.rm = TRUE) / denom
}

rc_metric_chargeoff_dollars <- function(df) {
  if (is.null(df) || !"performance_delinquency_amount" %in% names(df)) return(NA_real_)
  sum(df$performance_delinquency_amount, na.rm = TRUE)
}

rc_metric_irr <- function(df) {
  # Uses loan_irr_cache if attached as attr(df, "irr_cache")
  cache <- attr(df, "irr_cache")
  if (is.null(cache) || !"loan_id" %in% names(df)) return(NA_real_)
  ids <- df$loan_id
  sub <- cache[cache$loan_id %in% ids & !is.na(cache$irr), , drop = FALSE]
  if (nrow(sub) == 0) return(NA_real_)
  if ("issued_amount" %in% names(sub) && sum(sub$issued_amount, na.rm = TRUE) > 0) {
    sum(sub$irr * sub$issued_amount, na.rm = TRUE) / sum(sub$issued_amount, na.rm = TRUE)
  } else {
    mean(sub$irr, na.rm = TRUE)
  }
}

# Registry for service layer to pick by name
rc_metric_registry <- function() {
  list(
    dq_rate           = list(fn = rc_metric_dq,                label = "DQ Rate",        fmt = "percent", better = "lower"),
    loss_rate         = list(fn = rc_metric_loss_rate,         label = "Loss Rate",      fmt = "percent", better = "lower"),
    chargeoff_dollars = list(fn = rc_metric_chargeoff_dollars, label = "Charge-off $",   fmt = "dollars", better = "lower"),
    irr               = list(fn = rc_metric_irr,               label = "IRR",            fmt = "percent", better = "higher")
  )
}

# ---- Variable binning --------------------------------------------------
# Returns a factor of length nrow(df), or NULL if the column is absent.

rc_bin_variable <- function(df, var) {
  if (!var %in% names(df)) return(NULL)
  x <- df[[var]]
  if (is.factor(x))    return(droplevels(x))
  if (is.character(x)) return(factor(x))
  if (is.logical(x))   return(factor(ifelse(x, "TRUE", "FALSE")))
  if (!is.numeric(x))  return(factor(as.character(x)))

  bins <- switch(var,
    fico_score = list(
      breaks = c(-Inf, 620, 660, 700, 740, 780, Inf),
      labels = c("<620", "620-660", "660-700", "700-740", "740-780", "780+")
    ),
    debt_to_income_ratio = list(
      breaks = c(-Inf, 0.20, 0.30, 0.40, Inf),
      labels = c("<20%", "20-30%", "30-40%", "40%+")
    ),
    N_HARD_INQ_6M = list(
      breaks = c(-Inf, 0, 2, 5, Inf),
      labels = c("0", "1-2", "3-5", "6+")
    ),
    REVOL_UTL = list(
      breaks = c(-Inf, 0.30, 0.60, 0.90, Inf),
      labels = c("<30%", "30-60%", "60-90%", "90%+")
    ),
    BC_UTL = list(
      breaks = c(-Inf, 0.30, 0.60, 0.90, Inf),
      labels = c("<30%", "30-60%", "60-90%", "90%+")
    ),
    annual_income = list(
      breaks = c(-Inf, 40000, 75000, 120000, Inf),
      labels = c("<40k", "40-75k", "75-120k", "120k+")
    ),
    credit_file_age_months = list(
      breaks = c(-Inf, 24, 60, 120, Inf),
      labels = c("<2y", "2-5y", "5-10y", "10y+")
    ),
    maximum_approved_amount = list(
      breaks = c(-Inf, 5000, 15000, 30000, Inf),
      labels = c("<5k", "5-15k", "15-30k", "30k+")
    ),
    interest_rate_apr = list(
      breaks = c(-Inf, 0.10, 0.18, 0.26, Inf),
      labels = c("<10%", "10-18%", "18-26%", "26%+")
    ),
    months_on_books = list(
      breaks = c(-Inf, 6, 12, 24, Inf),
      labels = c("<6m", "6-12m", "12-24m", "24m+")
    ),
    NULL
  )

  if (!is.null(bins)) {
    return(cut(x, breaks = bins$breaks, labels = bins$labels,
               include.lowest = TRUE, right = FALSE))
  }

  qs <- stats::quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  qs <- unique(qs)
  if (length(qs) < 3) return(factor(rep("all", length(x))))
  cut(x, breaks = qs, include.lowest = TRUE, dig.lab = 4)
}

# Canonical candidate-variable registry
rc_candidate_registry <- function() {
  list(
    fico_score              = "FICO",
    debt_to_income_ratio    = "DTI",
    N_HARD_INQ_6M           = "Hard Inquiries (6m)",
    REVOL_UTL               = "Revolving Utilization",
    BC_UTL                  = "Bankcard Utilization",
    annual_income           = "Annual Income",
    credit_file_age_months  = "Credit File Age",
    delinquent_accounts_count = "Delinquent Accounts",
    N_OPEN_ACCTS            = "Open Accounts",
    N_COLL                  = "Collections",
    risk_grade              = "Risk Grade",
    term_months             = "Term (months)",
    maximum_approved_amount = "Loan Amount",
    interest_rate_apr       = "APR",
    partner_channel         = "Channel",
    borrower_repeat_type    = "Repeat Type",
    product_type            = "Product",
    booking_vintage         = "Vintage",
    months_on_books         = "Months on Books"
  )
}

rc_default_candidates <- function() {
  c("fico_score", "debt_to_income_ratio", "N_HARD_INQ_6M", "REVOL_UTL",
    "risk_grade", "maximum_approved_amount", "term_months",
    "partner_channel", "booking_vintage")
}

# ---- Split scoring -----------------------------------------------------

rc_score_split <- function(df, var, metric_fn) {
  bucket <- rc_bin_variable(df, var)
  if (is.null(bucket)) {
    return(data.frame(bucket=character(0), n=integer(0), pct=numeric(0),
                      metric_value=numeric(0), contribution_pp=numeric(0),
                      stringsAsFactors = FALSE))
  }
  parent_metric <- metric_fn(df)
  n_total <- nrow(df)

  lvls <- levels(bucket)
  rows <- lapply(lvls, function(lv) {
    sub <- df[!is.na(bucket) & bucket == lv, , drop = FALSE]
    if (!is.null(attr(df, "irr_cache"))) attr(sub, "irr_cache") <- attr(df, "irr_cache")
    n <- nrow(sub)
    mv <- if (n == 0) NA_real_ else metric_fn(sub)
    pct <- if (n_total == 0) 0 else n / n_total
    contrib <- if (is.na(mv) || is.na(parent_metric)) NA_real_ else (mv - parent_metric) * 100 * pct
    data.frame(bucket = lv, n = n, pct = pct,
               metric_value = mv, contribution_pp = contrib,
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# ---- Greedy Pareto path ------------------------------------------------
# Returns a nested list representing the tree. Each node:
#   list(depth, n, pct, metric_value, rule, chosen, alternatives, child)

rc_compute_pareto_path <- function(df, metric_fn, candidates,
                                   max_depth = 4, min_seg_pct = 0.02,
                                   depth = 0, rule = "All loans",
                                   total_n = nrow(df)) {
  node <- list(
    depth = depth,
    n = nrow(df),
    pct = if (total_n == 0) 0 else nrow(df) / total_n,
    metric_value = metric_fn(df),
    rule = rule,
    chosen = NULL,
    alternatives = NULL,
    child = NULL
  )
  if (depth >= max_depth || nrow(df) == 0 || length(candidates) == 0) return(node)

  per_var <- lapply(candidates, function(v) {
    sc <- rc_score_split(df, v, metric_fn)
    if (nrow(sc) == 0) return(NULL)
    abs_size_pct <- (sc$n) / total_n
    sc <- sc[abs_size_pct >= min_seg_pct & !is.na(sc$contribution_pp), , drop = FALSE]
    if (nrow(sc) == 0) return(NULL)
    # Score by absolute bad-rate attribution (metric_value * local_pct) to
    # surface the segment that carries the most of the outcome metric.
    sc$.score <- abs(sc$metric_value * sc$pct)
    top <- sc[which.max(sc$.score), , drop = FALSE]
    top$variable <- v
    top
  })
  per_var <- Filter(Negate(is.null), per_var)
  if (length(per_var) == 0) return(node)

  alts <- do.call(rbind, per_var)
  alts <- alts[order(-abs(alts$.score)), , drop = FALSE]
  node$alternatives <- alts

  best <- alts[1, , drop = FALSE]
  node$chosen <- list(
    variable = best$variable,
    bucket   = best$bucket,
    n        = best$n,
    metric_value = best$metric_value,
    contribution_pp = best$contribution_pp
  )

  bucket_col <- rc_bin_variable(df, best$variable)
  sub <- df[!is.na(bucket_col) & as.character(bucket_col) == best$bucket, , drop = FALSE]
  if (!is.null(attr(df, "irr_cache"))) attr(sub, "irr_cache") <- attr(df, "irr_cache")
  remaining <- setdiff(candidates, best$variable)
  child_rule <- sprintf("%s: %s", best$variable, best$bucket)
  node$child <- rc_compute_pareto_path(sub, metric_fn, remaining,
                                       max_depth = max_depth,
                                       min_seg_pct = min_seg_pct,
                                       depth = depth + 1,
                                       rule = child_rule,
                                       total_n = total_n)
  node
}

# ---- Shift decomposition (Oaxaca-style) --------------------------------
# delta_total = sum_i( (w_i^cur - w_i^base) * r_i^base )  +  sum_i( w_i^cur * (r_i^cur - r_i^base) )
#               \_________ MIX _________/                    \_________ RATE ________/

rc_decompose_shift <- function(df_current, df_baseline, metric_fn, seg_col) {
  stopifnot(seg_col %in% names(df_current), seg_col %in% names(df_baseline))
  segs <- union(unique(df_current[[seg_col]]), unique(df_baseline[[seg_col]]))
  n_cur  <- nrow(df_current)
  n_base <- nrow(df_baseline)

  rows <- lapply(segs, function(s) {
    cur  <- df_current[df_current[[seg_col]]  == s & !is.na(df_current[[seg_col]]),  , drop = FALSE]
    base <- df_baseline[df_baseline[[seg_col]] == s & !is.na(df_baseline[[seg_col]]), , drop = FALSE]
    if (!is.null(attr(df_current,  "irr_cache"))) attr(cur,  "irr_cache") <- attr(df_current,  "irr_cache")
    if (!is.null(attr(df_baseline, "irr_cache"))) attr(base, "irr_cache") <- attr(df_baseline, "irr_cache")
    w_cur  <- if (n_cur  == 0) 0 else nrow(cur)  / n_cur
    w_base <- if (n_base == 0) 0 else nrow(base) / n_base
    r_cur  <- if (nrow(cur)  == 0) NA_real_ else metric_fn(cur)
    r_base <- if (nrow(base) == 0) NA_real_ else metric_fn(base)
    mix  <- if (is.na(r_base)) 0 else (w_cur - w_base) * r_base * 100
    rate <- if (is.na(r_cur) || is.na(r_base)) 0 else w_cur * (r_cur - r_base) * 100
    data.frame(segment = as.character(s),
               n_current = nrow(cur), n_baseline = nrow(base),
               mix_contrib_pp = mix, rate_contrib_pp = rate,
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}
