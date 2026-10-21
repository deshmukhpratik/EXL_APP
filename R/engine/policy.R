# Engine: Policy
# Extracted from R/02_global_utils.R
# Functions: get_active_decline_thresholds, apply_decline_waterfall, replay_layers_50_80_90

# Get active decline thresholds from the default policy params (DRY).
# Consumers: Opportunity Finder swap-in/swap-out analysis.
get_active_decline_thresholds <- function(params = build_default_params(policy_manifest)) {
  thresholds <- list(
    "01. Fico"                = params$fico_min,
    "02. Thin file"           = params$thin_file_months,
    "03. Delinquency decline" = params$delinquency_max,
    "04. HighDTI"             = params$dti_cutoff
  )
  # V2+ decline thresholds (only active when non-default)
  if (!is.null(params$n_hard_inq_6m_max) && params$n_hard_inq_6m_max < 99) {
    thresholds[["06. Hard inquiries"]] <- params$n_hard_inq_6m_max
    thresholds[["05. HardInquiry"]]    <- params$n_hard_inq_6m_max  # alias for Python-generated data
  }
  if (!is.null(params$thin_file_reentry_delinq_gate) && params$thin_file_reentry_delinq_gate < 99)
    thresholds[["02b. Thin file re-entry gate"]] <- params$thin_file_reentry_delinq_gate
  if (!is.null(params$revol_utl_max) && params$revol_utl_max < 1.0)
    thresholds[["07. High utilization"]] <- params$revol_utl_max
  thresholds
}

# Single decline waterfall function — all consumers must call this (DRY).
# max_amount_col: column name for the max-offered-amount check (varies by context).
apply_decline_waterfall <- function(df,
                                    fico_min       = 300,
                                    fico_max       = 850,
                                    thin_file_mo   = 30,
                                    delinq_thresh  = 1,
                                    dti_thresh     = 0.50,
                                    max_amount_col = "maximum_offered_amount",
                                    dti_col        = "debt_to_income_ratio",
                                    n_hard_inq_6m_max = 99L,
                                    thin_file_reentry_delinq_gate = 99L,
                                    thin_file_reentry_min = NULL,
                                    thin_file_reentry_max = NULL,
                                    revol_utl_max  = 1.0) {
  flag <- rep("APPR", nrow(df))

  c1 <- is.na(df$fico_score) | df$fico_score > fico_max | df$fico_score < fico_min
  flag[c1] <- "01. Fico"

  # Thin file check with optional re-entry band.
  # Re-entry logic: applicants in the [reentry_min, reentry_max] band who
  # pass the delinquency gate are allowed through even if their file is
  # thinner than thin_file_mo. This models the "swap-in" of previously
  # declined segments under a relaxed policy, without opening the gate
  # for everyone thin. Mirrors policy_params_engine.R apply_decline_rules.
  c2_base <- !c1 & !is.na(df$credit_file_age_months) &
    df$credit_file_age_months <= thin_file_mo
  if (!is.null(thin_file_reentry_min) && !is.null(thin_file_reentry_max) &&
      !is.null(thin_file_reentry_delinq_gate) && thin_file_reentry_delinq_gate < 99) {
    in_band <- !is.na(df$credit_file_age_months) &
               df$credit_file_age_months >  thin_file_reentry_min &
               df$credit_file_age_months <= thin_file_reentry_max
    gate_pass <- !is.na(df$delinquent_accounts_count) &
                 df$delinquent_accounts_count <= thin_file_reentry_delinq_gate
    reentry_eligible <- in_band & gate_pass
    c2 <- c2_base & !reentry_eligible
  } else {
    c2 <- c2_base
  }
  flag[c2] <- "02. Thin file"

  c3 <- !(c1 | c2) & !is.na(df$delinquent_accounts_count) &
    df$delinquent_accounts_count > delinq_thresh
  flag[c3] <- "03. Delinquency decline"

  dti_vals <- if (dti_col %in% names(df)) df[[dti_col]] else df$debt_to_income_ratio
  c4 <- !(c1 | c2 | c3) & !is.na(dti_vals) & dti_vals > dti_thresh
  flag[c4] <- "04. HighDTI"

  if (max_amount_col %in% names(df)) {
    c5 <- !(c1 | c2 | c3 | c4) & !is.na(df[[max_amount_col]]) &
      df[[max_amount_col]] < 1
    flag[c5] <- "05. max_amount_limit"
  }
  already_declined <- c1 | c2 | c3 | c4 |
    (if (max_amount_col %in% names(df)) (!is.na(df[[max_amount_col]]) & df[[max_amount_col]] < 1 & !(c1 | c2 | c3 | c4)) else rep(FALSE, nrow(df)))

  # Hard inquiry check (V2+ policies)
  if (!is.null(n_hard_inq_6m_max) && n_hard_inq_6m_max < 99) {
    inq_col <- intersect(c("N_HARD_INQ_6M", "n_hard_inq_6m"), names(df))
    if (length(inq_col) > 0) {
      inq_col <- inq_col[1]
      c_inq <- !already_declined & !is.na(df[[inq_col]]) & df[[inq_col]] >= n_hard_inq_6m_max
      flag[c_inq] <- "06. Hard inquiries"
      already_declined <- already_declined | c_inq
    }
  }

  # Revolving utilization check (V2+ policies)
  if (!is.null(revol_utl_max) && revol_utl_max < 1.0) {
    utl_col <- intersect(c("REVOL_UTL", "revol_utl"), names(df))
    if (length(utl_col) > 0) {
      utl_col <- utl_col[1]
      c_utl <- !already_declined & !is.na(df[[utl_col]]) & df[[utl_col]] > revol_utl_max
      flag[c_utl] <- "07. High utilization"
      already_declined <- already_declined | c_utl
    }
  }

  flag
}

# Legacy alias — callers that used apply_custom_rules() still work
apply_custom_rules <- apply_decline_waterfall

# ---- Replay downstream policy layers after APR modification ----
# Takes a full dataframe with adjusted_apr_pre_term_cap already modified.
# Replays Layer 50 (amounts), Layer 80 (affordability), Layer 90 (decline waterfall).
# Uses adjusted_apr_pre_term_cap (policy-derived APR from L30/L40) as the source,
# NOT interest_rate_apr (original CSV column). This ensures final_apr deltas
# match the intended pricing bump exactly.
# Optional decline threshold params default to baseline policy values.
replay_layers_50_80_90 <- function(df, fico_min = 300, fico_max = 850,
                                   thin_file_mo = 30, delinq_thresh = 1,
                                   dti_thresh = 0.50,
                                   n_hard_inq_6m_max = 99L,
                                   thin_file_reentry_delinq_gate = 99L,
                                   thin_file_reentry_min = NULL,
                                   thin_file_reentry_max = NULL,
                                   revol_utl_max = 1.0) {
  sql_least_v <- function(a, b) { out <- pmin(a, b); out[is.na(a) | is.na(b)] <- NA_real_; out }

  # Source APR is the policy-derived adjusted_apr_pre_term_cap (after L30+L40)
  new_apr <- df$adjusted_apr_pre_term_cap

  # Layer 10 intermediates (not in output projection, must recompute)
  ann_inc  <- ifelse(is.na(df$annual_income), 0, df$annual_income)
  mth_debt <- ifelse(is.na(df$monthly_debt_payment), 0, df$monthly_debt_payment)
  rent_est <- ifelse(!is.na(df$monthly_rent), pmax(df$monthly_rent, 300), NA_real_)
  soc_tax  <- ifelse(ann_inc < 140000, ann_inc * 0.06, 0)
  med_tax  <- ann_inc * 0.04
  post_tax <- ann_inc - soc_tax - med_tax

  # Layer 50: 72-month APR cap, income-based limit, max approved amount
  tm <- df$term_months
  repriced_final_apr <- ifelse(!is.na(tm) & tm == 72, pmin(new_apr, 0.18), new_apr)
  income_limit <- (post_tax - 12 * mth_debt) / (1 - repriced_final_apr)
  exp_cap <- df$exposure_limit_cap
  new_max_approved <- sql_least_v(income_limit, exp_cap)

  df$final_apr                <- repriced_final_apr
  df$income_based_limit_cap   <- income_limit
  df$maximum_approved_amount  <- new_max_approved

  # Layer 80: offered APR (72-month cap on offered_term_months), affordability
  ot <- df$offered_term_months
  repriced_offered_apr <- ifelse(!is.na(ot) & ot == 72, pmin(new_apr, 0.18), new_apr)
  mth_pti <- post_tax / 12.0
  pti_cap <- ifelse(df$risk_grade == "High", 0.25,
             ifelse(df$risk_grade == "Med",  0.20,
             ifelse(df$risk_grade == "Low",  0.15, 0.20)))
  # ALIGNED with scripts/gen_policy.py and policy_params_engine.R: affordability
  # treats only monthly debt service as existing obligation. Rent is a living
  # expense, not debt service, and was excluded from the data generator's L80
  # calculation to avoid over-tightening. Including rent here caused the R
  # re-run to decline ~70% of Python-approved PL-CURRENT loans via Layer 90
  # max_amount_limit, making Scanner take-rates hit the 100% safety cap for
  # every segment. See scripts/gen_policy.py:147-153.
  exist_oblig <- mth_debt
  max_install <- (pti_cap * mth_pti) - exist_oblig
  r_new <- repriced_offered_apr / 12.0
  n_new <- ot
  new_afford <- ifelse(
    max_install <= 0, 0,
    ifelse(r_new == 0, max_install * n_new,
           max_install * ((1 + r_new)^n_new - 1) / (r_new * (1 + r_new)^n_new)))
  new_max_offered <- sql_least_v(new_max_approved, new_afford)

  df$offered_final_apr        <- repriced_offered_apr
  df$affordability_limit_cap  <- new_afford
  df$maximum_offered_amount   <- new_max_offered

  # Layer 90: re-run decline waterfall (single source of truth)
  # Use cleaned dti_ratio column when available (matches canonical Layer 90 in policy engine)
  dti_col_name <- if ("dti_ratio" %in% names(df)) "dti_ratio" else "debt_to_income_ratio"
  df$offered_decision_flag <- apply_decline_waterfall(
    df, fico_min = fico_min, fico_max = fico_max,
    thin_file_mo = thin_file_mo, delinq_thresh = delinq_thresh,
    dti_thresh = dti_thresh, dti_col = dti_col_name,
    n_hard_inq_6m_max = n_hard_inq_6m_max,
    thin_file_reentry_delinq_gate = thin_file_reentry_delinq_gate,
    thin_file_reentry_min = thin_file_reentry_min,
    thin_file_reentry_max = thin_file_reentry_max,
    revol_utl_max = revol_utl_max)

  df
}
