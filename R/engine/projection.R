# Engine: Projection
# Functions: compute_funnel, compute_est_bad_rate, compute_adjusted_xirr_v2
# Source: Extracted from R/02_global_utils.R

# Compute funnel metrics.
#
# conv_rate uses max(n_approved, n_issued) as denominator. This is a safety
# net: in a self-consistent run we expect n_issued <= n_approved so the max
# equals n_approved. If the re-run declines loans that were originally
# issued (engine drift), using n_approved would produce conv_rate > 100%;
# using the max caps it at 100% and surfaces the drift as a warning rather
# than a silent NA/Inf in downstream KPIs.
compute_funnel <- function(df,
                           decision_col = "offered_decision_flag") {
  n_apps     <- nrow(df)
  n_approved <- sum(df[[decision_col]] == "APPR", na.rm = TRUE)
  n_issued   <- sum(df$issued_flag == 1, na.rm = TRUE)
  if (n_issued > n_approved) {
    warning(sprintf(
      "compute_funnel: n_issued (%d) > n_approved (%d). Engine drift: re-run declined loans that were originally issued.",
      n_issued, n_approved), call. = FALSE)
  }
  denom_take <- max(n_approved, n_issued)
  list(
    apps      = n_apps,
    approved  = n_approved,
    issued    = n_issued,
    appr_rate = if (n_apps     > 0) n_approved / n_apps     else NA_real_,
    conv_rate = if (denom_take > 0) n_issued   / denom_take else NA_real_,
    avg_apr   = if (n_issued > 0) mean(df$interest_rate_apr[df$issued_flag == 1],          na.rm = TRUE) else NA_real_,
    avg_amt   = if (n_issued > 0) mean(df$issued_amount[df$issued_flag == 1],              na.rm = TRUE) else NA_real_,
    bad_rate  = if (n_issued > 0) mean(df$performance_delinquency_flag[df$issued_flag == 1], na.rm = TRUE) else NA_real_
  )
}

# Grade-weighted estimated bad rate: uses historical grade-level delinquency
# rates weighted by the scenario's grade distribution among issued loans.
# Responds to grading, decline rule, and affordability changes.
compute_est_bad_rate <- function(df, grade_del_rates) {
  issued <- df[df$issued_flag == 1, , drop = FALSE]
  if (nrow(issued) == 0) return(NA_real_)
  gc <- table(issued$risk_grade)
  total <- sum(gc)
  if (total == 0) return(NA_real_)
  sum(sapply(names(gc), function(g) {
    rate <- if (g %in% names(grade_del_rates)) grade_del_rates[[g]] else 0
    gc[[g]] * rate
  })) / total
}

# ---- Compute APR-adjusted portfolio XIRR from v2 performance tape ----
# Combines observed + projected cashflows, both adjusted for APR delta.
# df_sim: simulated results (with modified interest_rate_apr)
# tape: v2 performance tape object (tape$rows, tape$projected, tape$loan_view)
# df_bl: baseline dataframe (for computing APR deltas)
# return_rows: if TRUE, returns the adjusted observed rows_sub (for charts); scalar XIRR uses full obs+proj
compute_adjusted_xirr_v2 <- function(df_sim, tape, df_bl, return_rows = FALSE) {
  if (is.null(tape) || is.null(df_bl)) {
    if (return_rows) return(NULL) else return(NA_real_)
  }
  ids <- as.character(df_sim$loan_id[df_sim$issued_flag == 1])
  rows_sub <- tape$rows[tape$rows$loan_id %in% ids, ]
  if (nrow(rows_sub) == 0) {
    if (return_rows) return(rows_sub) else return(NA_real_)
  }

  # Compute per-loan APR deltas
  bl_apr  <- setNames(df_bl$interest_rate_apr, as.character(df_bl$loan_id))
  sim_apr <- setNames(df_sim$interest_rate_apr, as.character(df_sim$loan_id))
  apr_delta_map <- sim_apr[names(bl_apr)] - bl_apr
  has_delta <- any(!is.na(apr_delta_map) & apr_delta_map != 0)

  if (has_delta) {
    rows_sub <- rows_sub %>%
      arrange(loan_id, payment_date) %>%
      group_by(loan_id) %>%
      mutate(.period_int = interest_payment - lag(interest_payment, default = 0)) %>%
      ungroup()
    loan_apr <- setNames(tape$loan_view$interest_rate_apr,
                         as.character(tape$loan_view$loan_id))
    row_apr    <- loan_apr[rows_sub$loan_id]
    loan_delta <- apr_delta_map[rows_sub$loan_id]
    incr <- ifelse(!is.na(loan_delta) & loan_delta != 0 &
                   !is.na(row_apr) & row_apr > 0,
                   pmax(rows_sub$.period_int, 0) * loan_delta / row_apr, 0)
    rows_sub$adj_cf <- rows_sub$adj_cf + incr
    rows_sub$.period_int <- NULL
  }

  if (return_rows) return(rows_sub)

  # Observed cashflows aggregated by date
  obs_agg <- rows_sub %>%
    group_by(payment_date) %>%
    summarise(cf = sum(adj_cf, na.rm = TRUE), .groups = "drop")

  # Projected cashflows (with APR adjustment)
  proj_sub <- if (!is.null(tape$projected)) {
    tape$projected[tape$projected$loan_id %in% ids, ]
  } else {
    data.frame(loan_id = character(), payment_date = as.Date(character()),
               proj_cf = numeric(), proj_int = numeric(), stringsAsFactors = FALSE)
  }

  if (nrow(proj_sub) > 0 && has_delta) {
    loan_apr <- setNames(tape$loan_view$interest_rate_apr,
                         as.character(tape$loan_view$loan_id))
    p_apr   <- loan_apr[proj_sub$loan_id]
    p_delta <- apr_delta_map[proj_sub$loan_id]
    p_int   <- if ("proj_int" %in% names(proj_sub)) proj_sub$proj_int else 0
    p_incr  <- ifelse(!is.na(p_delta) & p_delta != 0 &
                      !is.na(p_apr) & p_apr > 0,
                      pmax(p_int, 0) * p_delta / p_apr, 0)
    proj_sub$proj_cf <- proj_sub$proj_cf + p_incr
  }

  proj_agg <- if (nrow(proj_sub) > 0) {
    proj_sub %>%
      group_by(payment_date) %>%
      summarise(cf = sum(proj_cf, na.rm = TRUE), .groups = "drop")
  } else {
    data.frame(payment_date = as.Date(character()), cf = numeric(),
               stringsAsFactors = FALSE)
  }

  # Combine observed + projected
  all_cf <- bind_rows(obs_agg, proj_agg) %>%
    group_by(payment_date) %>%
    summarise(cf = sum(cf), .groups = "drop") %>%
    arrange(payment_date)

  if (nrow(all_cf) < 2) return(NA_real_)
  tryCatch(xirr(all_cf$cf, all_cf$payment_date), error = function(e) NA_real_)
}

# Legacy alias for backward compatibility
compute_adjusted_xirr <- compute_adjusted_xirr_v2


# ---- Projected Lifetime Bad Rate using DCOR Curves ----
#
# For each loan, computes the probability of ever going delinquent over its
# full term by combining:
#   - Observed history (actual delinquency flag for MOBs already seen)
#   - DCOR curve projection (conditional delinquency onset rate for future MOBs)
#
# Formula for a loan currently at MOB m with term T:
#   If already delinquent:  projected_lifetime_prob = 1.0
#   Otherwise:              projected_lifetime_prob = 1 - prod(1 - DCOR(t)) for t = m+1 .. T
#
# This gives the probability of becoming delinquent in at least one of the
# remaining months, conditional on having survived to MOB m without delinquency.
#
# @param loan_ids Character vector of loan IDs to compute for
# @param tape The performance tape object from build_perf_tape_v2, containing
#   $rows (monthly rows with month_on_books, ever_del, loan_id),
#   $loan_view (loan-level statics with product_type, term_months, vintage_quarter),
#   $curves (DCOR/DPOR curves by product x term x vintage x scenario x mob)
# @param credit_data The main credit data frame (used for fallback risk_grade
#   lookup; must have loan_id column)
# @return data.frame with columns: loan_id, observed_bad_rate (0/1),
#   projected_lifetime_prob, current_mob, term_months, curve_source
compute_projected_bad_rate <- function(loan_ids, tape, credit_data) {

  loan_ids <- as.character(loan_ids)

  # Early return for empty input
  if (length(loan_ids) == 0) {
    return(data.frame(
      loan_id                = character(0),
      observed_bad_rate      = numeric(0),
      projected_lifetime_prob = numeric(0),
      current_mob            = integer(0),
      term_months            = integer(0),
      curve_source           = character(0),
      stringsAsFactors       = FALSE
    ))
  }

  # --- Validate inputs ---
  if (is.null(tape) || is.null(tape$rows) || is.null(tape$loan_view) || is.null(tape$curves)) {
    warning("compute_projected_bad_rate: tape is missing required components; returning empty frame.")
    return(data.frame(
      loan_id               = character(0),
      observed_bad_rate      = numeric(0),
      projected_lifetime_prob = numeric(0),
      current_mob            = integer(0),
      term_months            = integer(0),
      curve_source           = character(0),
      stringsAsFactors       = FALSE
    ))
  }

  # --- Extract per-loan current state from tape rows ---
  # For each loan: current MOB, observed ever-delinquent flag
  tape_rows <- tape$rows
  tape_rows$loan_id <- as.character(tape_rows$loan_id)
  loan_state <- tape_rows[tape_rows$loan_id %in% loan_ids, , drop = FALSE]

  if (nrow(loan_state) == 0) {
    # No tape data for these loans — return fallback with observed bad rate from credit_data
    cd_sub <- credit_data[as.character(credit_data$loan_id) %in% loan_ids, , drop = FALSE]
    obs_flag <- if ("performance_delinquency_flag" %in% names(cd_sub)) {
      as.numeric(cd_sub$performance_delinquency_flag)
    } else {
      rep(NA_real_, nrow(cd_sub))
    }
    return(data.frame(
      loan_id                = as.character(cd_sub$loan_id),
      observed_bad_rate      = obs_flag,
      projected_lifetime_prob = obs_flag,
      current_mob            = NA_integer_,
      term_months            = NA_integer_,
      curve_source           = "none",
      stringsAsFactors       = FALSE
    ))
  }

  # Compute per-loan summary: max MOB and whether ever delinquent
  loan_summary <- do.call(rbind, lapply(split(loan_state, loan_state$loan_id), function(d) {
    data.frame(
      loan_id   = d$loan_id[1],
      current_mob = max(d$month_on_books, na.rm = TRUE),
      ever_del    = max(d$ever_del, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  }))
  rownames(loan_summary) <- NULL

  # --- Join loan statics from loan_view ---
  lv <- tape$loan_view
  lv$loan_id <- as.character(lv$loan_id)
  lv_sub <- lv[lv$loan_id %in% loan_ids,
               c("loan_id", "product_type", "term_months", "vintage_quarter"),
               drop = FALSE]

  loan_df <- merge(loan_summary, lv_sub, by = "loan_id", all.x = TRUE)

  # --- Look up DCOR curves ---
  curves <- tape$curves
  # Filter to base scenario only
  if ("scenario" %in% names(curves)) {
    curves <- curves[curves$scenario == "base", , drop = FALSE]
  }

  # Pre-split curves by (product_type, term_months, vintage_quarter) for fast lookup
  curves$curve_key <- paste(curves$product_type, curves$term_months,
                            curves$vintage_quarter, sep = "|")
  loan_df$curve_key <- paste(loan_df$product_type, loan_df$term_months,
                             loan_df$vintage_quarter, sep = "|")

  curve_list <- split(curves, curves$curve_key)

  # --- Compute projected lifetime prob for each loan ---
  n <- nrow(loan_df)
  projected_prob <- numeric(n)
  curve_src      <- character(n)

  for (i in seq_len(n)) {
    row <- loan_df[i, ]

    # If already delinquent, projected prob is 1.0
    if (!is.na(row$ever_del) && row$ever_del >= 1) {
      projected_prob[i] <- 1.0
      curve_src[i]      <- "observed"
      next
    }

    # If missing statics, fall back to observed
    if (is.na(row$term_months) || is.na(row$current_mob)) {
      projected_prob[i] <- ifelse(!is.na(row$ever_del), row$ever_del, NA_real_)
      curve_src[i]      <- "none"
      next
    }

    mob_now  <- as.integer(row$current_mob)
    term     <- as.integer(row$term_months)

    # Remaining MOBs to project
    if (mob_now >= term) {
      # Fully matured: observed is final
      projected_prob[i] <- ifelse(!is.na(row$ever_del), row$ever_del, 0)
      curve_src[i]      <- "matured"
      next
    }

    # Look up curve
    ckey <- row$curve_key
    crv  <- curve_list[[ckey]]

    if (is.null(crv) || nrow(crv) == 0) {
      # No matching curve — fall back to observed
      projected_prob[i] <- ifelse(!is.na(row$ever_del), row$ever_del, NA_real_)
      curve_src[i]      <- "none"
      next
    }

    # Get DCOR rates for future MOBs: (current_mob + 1) through term
    future_mobs <- seq.int(mob_now + 1L, term)
    dcor_idx    <- match(future_mobs, crv$mob)

    # Extract DCOR rates; default to 0 for any missing MOBs
    dcor_rates <- ifelse(is.na(dcor_idx), 0, crv$dcor_rate[dcor_idx])

    # Survival probability through remaining months
    # P(never delinquent in remaining) = prod(1 - DCOR(t))
    # P(delinquent at least once)      = 1 - prod(1 - DCOR(t))
    survival_prob <- prod(1 - pmin(pmax(dcor_rates, 0), 1))
    projected_prob[i] <- 1 - survival_prob

    # Track where the curve came from (empirical, fitted, inherited, or mixed)
    future_sources <- ifelse(is.na(dcor_idx), NA_character_,
                             crv$curve_source[dcor_idx])
    future_sources <- future_sources[!is.na(future_sources)]
    if (length(future_sources) == 0) {
      curve_src[i] <- "none"
    } else {
      src_unique <- unique(future_sources)
      if (length(src_unique) == 1) {
        curve_src[i] <- src_unique
      } else {
        curve_src[i] <- "mixed"
      }
    }
  }

  data.frame(
    loan_id                = loan_df$loan_id,
    observed_bad_rate      = ifelse(!is.na(loan_df$ever_del), loan_df$ever_del, 0),
    projected_lifetime_prob = projected_prob,
    current_mob            = as.integer(loan_df$current_mob),
    term_months            = as.integer(loan_df$term_months),
    curve_source           = curve_src,
    stringsAsFactors       = FALSE
  )
}


# ---- Segment-level projected bad rate wrapper ----
# Convenience function: compute mean projected bad rate for a segment of loans.
# @param loan_ids Character vector of loan IDs
# @param tape Performance tape object
# @param credit_data Main credit data frame
# @return Named list with segment_mean_observed, segment_mean_projected, n_loans
compute_segment_projected_bad_rate <- function(loan_ids, tape, credit_data) {
  result <- compute_projected_bad_rate(loan_ids, tape, credit_data)
  if (nrow(result) == 0) {
    return(list(
      segment_mean_observed  = NA_real_,
      segment_mean_projected = NA_real_,
      n_loans                = 0L
    ))
  }
  list(
    segment_mean_observed  = mean(result$observed_bad_rate, na.rm = TRUE),
    segment_mean_projected = mean(result$projected_lifetime_prob, na.rm = TRUE),
    n_loans                = nrow(result)
  )
}
