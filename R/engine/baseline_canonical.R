#!/usr/bin/env Rscript

# Personal Loan Baseline Policy Master (Canonical Labels, v03 Logic)
# Interface contract uses canonical demo-friendly variable names only.
# Decision wording is preserved exactly: decline reason codes / APPR.
#
# Usage:
# source("scripts/policies/master/dummy_credit_policy_strategy_master.R")
# out <- apply_master_credit_policy(df)

required_master_input_columns <- c(
  "loan_id", "fico_score", "annual_income", "monthly_rent", "monthly_debt_payment",
  "debt_to_income_ratio", "credit_file_age_months", "delinquent_accounts_count",
  "months_since_last_delinquency", "internal_file_age_months", "term_months",
  "partner_channel", "pricing_test_variant", "risk_model_score_1",
  "risk_model_score_2", "risk_model_score_3"
)

core_master_output_columns <- c(
  "model_1_risk_band", "model_2_risk_band", "intermediate_risk_rank", "final_model_adjusted_rank",
  "underwriting_risk_tier", "risk_grade", "base_apr", "adjusted_apr_pre_term_cap",
  "final_apr", "income_based_limit_cap", "exposure_limit_cap",
  "maximum_approved_amount", "decision_flag", "dti_ratio", "term_month_policy",
  "offered_term_months", "offered_final_apr", "affordability_limit_cap",
  "maximum_offered_amount", "offered_decision_flag"
)

sql_least <- function(a, b) {
  out <- pmin(a, b)
  out[is.na(a) | is.na(b)] <- NA_real_
  out
}

first_hit_assign <- function(current, condition, value) {
  idx <- is.na(current) & condition
  current[idx] <- value
  current
}

apply_decline_rules <- function(df, fico_col, file_age_col, delinq_col, dti_col, max_amt_col,
                                fico_min = 300, fico_max = 850, thin_file_mo = 30,
                                delinq_thresh = 1, dti_thresh = 0.50, ...) {
  flag <- rep("APPR", nrow(df))
  c1 <- is.na(df[[fico_col]]) | df[[fico_col]] > fico_max | df[[fico_col]] < fico_min
  flag[c1] <- "01. Fico"
  c2 <- !c1 & !is.na(df[[file_age_col]]) & df[[file_age_col]] <= thin_file_mo
  flag[c2] <- "02. Thin file"
  c3 <- !(c1 | c2) & !is.na(df[[delinq_col]]) & df[[delinq_col]] > delinq_thresh
  flag[c3] <- "03. Delinquency decline"
  c4 <- !(c1 | c2 | c3) & !is.na(df[[dti_col]]) & df[[dti_col]] > dti_thresh
  flag[c4] <- "04. HighDTI"
  if (max_amt_col %in% names(df)) {
    c5 <- !(c1 | c2 | c3 | c4) & (is.na(df[[max_amt_col]]) | df[[max_amt_col]] < 1)
    flag[c5] <- "05. max_amount_limit"
  }
  flag
}

apply_credit_policy <- function(df) {
  # ===========================================================================
  # Layer 00: Contract and Inputs
  # ===========================================================================
  missing <- setdiff(required_master_input_columns, names(df))
  if (length(missing) > 0) {
    stop(sprintf("Missing required master columns: %s", paste(missing, collapse = ", ")))
  }

  n <- nrow(df)
  out <- df

  # Local aliases for readability (canonical names only).
  fico_score <- out$fico_score
  annual_income <- out$annual_income
  monthly_rent <- out$monthly_rent
  monthly_debt_payment <- out$monthly_debt_payment
  debt_to_income_ratio <- out$debt_to_income_ratio
  credit_file_age_months <- out$credit_file_age_months
  delinquent_accounts_count <- out$delinquent_accounts_count
  months_since_last_delinquency <- out$months_since_last_delinquency
  internal_file_age_months <- out$internal_file_age_months
  term_months <- out$term_months
  partner_channel <- out$partner_channel
  pricing_test_variant <- out$pricing_test_variant
  risk_model_score_1 <- out$risk_model_score_1
  risk_model_score_2 <- out$risk_model_score_2
  risk_model_score_3 <- out$risk_model_score_3

  # ===========================================================================
  # Layer 10: Feature Derivation (rent_est, dti5, taxes, post_tax_income)
  # ===========================================================================
  out$rent_est <- ifelse(!is.na(monthly_rent), pmax(monthly_rent, 300), NA_real_)
  out$dti5 <- ifelse(
    !is.na(annual_income) & annual_income > 0 & !is.na(monthly_debt_payment) & !is.na(out$rent_est),
    round(((monthly_debt_payment + out$rent_est) / annual_income) * 100, 2),
    NA_real_
  )

  out$social_tax <- ifelse(!is.na(annual_income) & annual_income < 140000, annual_income * 0.06, 0)
  out$med_tax <- annual_income * 0.04
  out$post_tax_income <- annual_income - out$social_tax - out$med_tax

  # ===========================================================================
  # Layer 20: Grading Section (Risk Waterfall)
  # ===========================================================================
  out$model_1_risk_band <- ifelse(
    is.na(risk_model_score_1), 3L,
    ifelse(
      risk_model_score_1 == 0, 1L,
      ifelse(
        risk_model_score_1 < 0.016, 2L,
        ifelse(risk_model_score_1 < 0.022, 3L, 4L)
      )
    )
  )

  out$model_2_risk_band <- rep(NA_integer_, n)
  out$model_2_risk_band <- first_hit_assign(
    out$model_2_risk_band, !is.na(risk_model_score_2) & risk_model_score_2 < 0.003 & out$model_1_risk_band == 1, 1L
  )
  out$model_2_risk_band <- first_hit_assign(
    out$model_2_risk_band, !is.na(risk_model_score_2) & risk_model_score_2 < 0.005 & out$model_1_risk_band == 1, 2L
  )
  out$model_2_risk_band <- first_hit_assign(
    out$model_2_risk_band, !is.na(risk_model_score_2) & risk_model_score_2 < 0.008 & out$model_1_risk_band == 1, 3L
  )
  out$model_2_risk_band <- first_hit_assign(
    out$model_2_risk_band, !is.na(risk_model_score_2) & risk_model_score_2 < 0.009 & out$model_1_risk_band == 1, 4L
  )
  out$model_2_risk_band <- first_hit_assign(
    out$model_2_risk_band, !is.na(risk_model_score_2) & risk_model_score_2 < 0.003 & out$model_1_risk_band == 2, 1L
  )
  out$model_2_risk_band <- first_hit_assign(
    out$model_2_risk_band, !is.na(risk_model_score_2) & risk_model_score_2 < 0.005 & out$model_1_risk_band == 2, 2L
  )
  out$model_2_risk_band <- first_hit_assign(
    out$model_2_risk_band, !is.na(risk_model_score_2) & risk_model_score_2 < 0.008 & out$model_1_risk_band == 3, 3L
  )
  out$model_2_risk_band <- first_hit_assign(
    out$model_2_risk_band, !is.na(risk_model_score_2) & risk_model_score_2 < 0.009 & out$model_1_risk_band == 4, 4L
  )
  out$model_2_risk_band[is.na(out$model_2_risk_band)] <- 2L

  out$intermediate_risk_rank <- rep(NA_integer_, n)
  out$intermediate_risk_rank <- first_hit_assign(
    out$intermediate_risk_rank, out$model_1_risk_band == 1 & out$model_2_risk_band == 1, 1L
  )
  out$intermediate_risk_rank <- first_hit_assign(
    out$intermediate_risk_rank, out$model_1_risk_band == 1 & out$model_2_risk_band == 2, 1L
  )
  out$intermediate_risk_rank <- first_hit_assign(
    out$intermediate_risk_rank, out$model_1_risk_band == 2 & out$model_2_risk_band == 2, 2L
  )
  out$intermediate_risk_rank <- first_hit_assign(
    out$intermediate_risk_rank, out$model_1_risk_band == 2 & out$model_2_risk_band == 3, 1L
  )
  out$intermediate_risk_rank <- first_hit_assign(
    out$intermediate_risk_rank, out$model_1_risk_band == 3 & out$model_2_risk_band == 3, 3L
  )
  out$intermediate_risk_rank <- first_hit_assign(
    out$intermediate_risk_rank, out$model_1_risk_band == 4 & out$model_2_risk_band == 3, 4L
  )
  out$intermediate_risk_rank <- first_hit_assign(
    out$intermediate_risk_rank, out$model_1_risk_band == 4 & out$model_2_risk_band == 4, 4L
  )
  out$intermediate_risk_rank[is.na(out$intermediate_risk_rank)] <- 3L

  out$final_model_adjusted_rank <- rep(NA_integer_, n)
  out$final_model_adjusted_rank <- first_hit_assign(
    out$final_model_adjusted_rank, !is.na(risk_model_score_3) & out$intermediate_risk_rank == 1 & risk_model_score_3 < 0.02, 1L
  )
  out$final_model_adjusted_rank <- first_hit_assign(
    out$final_model_adjusted_rank, !is.na(risk_model_score_3) & out$intermediate_risk_rank == 1 & risk_model_score_3 < 0.03, 2L
  )
  out$final_model_adjusted_rank <- first_hit_assign(
    out$final_model_adjusted_rank, !is.na(risk_model_score_3) & out$intermediate_risk_rank == 2 & risk_model_score_3 < 0.04, 1L
  )
  out$final_model_adjusted_rank <- first_hit_assign(
    out$final_model_adjusted_rank, !is.na(risk_model_score_3) & out$intermediate_risk_rank == 2 & risk_model_score_3 < 0.06, 2L
  )
  out$final_model_adjusted_rank <- first_hit_assign(
    out$final_model_adjusted_rank, !is.na(risk_model_score_3) & out$intermediate_risk_rank == 3 & risk_model_score_3 < 0.065, 3L
  )
  out$final_model_adjusted_rank <- first_hit_assign(
    out$final_model_adjusted_rank, !is.na(risk_model_score_3) & out$intermediate_risk_rank == 4 & risk_model_score_3 < 0.08, 4L
  )
  out$final_model_adjusted_rank[is.na(out$final_model_adjusted_rank)] <- 2L

  branch_1 <- (!is.na(internal_file_age_months) & internal_file_age_months >= 25) &
    (!is.na(months_since_last_delinquency) & months_since_last_delinquency >= 50)
  branch_2 <- !branch_1

  out$underwriting_risk_tier <- rep(NA_integer_, n)
  out$underwriting_risk_tier[branch_1 & out$final_model_adjusted_rank == 1] <- 1L
  out$underwriting_risk_tier[branch_1 & out$final_model_adjusted_rank == 2] <- 2L
  out$underwriting_risk_tier[branch_1 & out$final_model_adjusted_rank == 3] <- 3L
  out$underwriting_risk_tier[branch_1 & out$final_model_adjusted_rank == 4] <- 3L
  out$underwriting_risk_tier[branch_2 & out$final_model_adjusted_rank == 1] <- 1L
  out$underwriting_risk_tier[branch_2 & out$final_model_adjusted_rank == 2] <- 3L
  out$underwriting_risk_tier[branch_2 & out$final_model_adjusted_rank == 3] <- 4L
  out$underwriting_risk_tier[branch_2 & out$final_model_adjusted_rank == 4] <- 4L

  # ===========================================================================
  # Layer 30: Base Pricing Section
  # ===========================================================================
  # Grade convention: "High" = High Credit Quality (best borrowers, lowest APR)
  #                   "Med"  = Medium Credit Quality
  #                   "Low"  = Low Credit Quality (weakest borrowers, highest APR)
  # This refers to CREDIT QUALITY, not risk level.
  out$risk_grade <- ifelse(
    out$underwriting_risk_tier == 1, "High",
    ifelse(
      out$underwriting_risk_tier == 2, "Med",
      ifelse(out$underwriting_risk_tier %in% c(3, 4), "Low", NA_character_)
    )
  )

  out$base_apr <- ifelse(
    out$risk_grade == "High", 0.07,
    ifelse(out$risk_grade == "Med", 0.12, ifelse(out$risk_grade == "Low", 0.16, NA_real_))
  )

  # ===========================================================================
  # Layer 40: Channel/Variant Pricing Section
  # ===========================================================================
  out$adjusted_apr_pre_term_cap <- out$base_apr
  idx_a <- which(
    !is.na(partner_channel) & partner_channel == "Direct" &
      !is.na(fico_score) & fico_score > 660 &
      !is.na(pricing_test_variant) & pricing_test_variant == "A"
  )
  out$adjusted_apr_pre_term_cap[idx_a] <- out$base_apr[idx_a] + 0.05
  idx_b <- which(
    !is.na(partner_channel) & partner_channel == "Direct" &
      !is.na(fico_score) & fico_score > 660 &
      !is.na(pricing_test_variant) & pricing_test_variant == "B"
  )
  out$adjusted_apr_pre_term_cap[idx_b] <- out$base_apr[idx_b] + 0.10

  # ===========================================================================
  # Layer 50: Maximum Amount Section
  # ===========================================================================
  out$final_apr <- ifelse(term_months == 72, pmin(out$adjusted_apr_pre_term_cap, 0.18), out$adjusted_apr_pre_term_cap)

  out$income_based_limit_cap <- (out$post_tax_income - 12 * monthly_debt_payment) / (1 - out$final_apr)
  out$exposure_limit_cap <- ifelse(out$risk_grade == "High", 50000, 40000)
  out$maximum_approved_amount <- sql_least(out$income_based_limit_cap, out$exposure_limit_cap)

  # ===========================================================================
  # Layer 60: Base Decision Section
  # ===========================================================================
  out$decision_flag <- apply_decline_rules(
    out, fico_col = "fico_score", file_age_col = "credit_file_age_months",
    delinq_col = "delinquent_accounts_count", dti_col = "debt_to_income_ratio",
    max_amt_col = "maximum_approved_amount"
  )

  # ===========================================================================
  # Layer 70: Tenure Policy Section
  # ===========================================================================
  # Data quality guard: if raw debt_to_income_ratio exceeds 1.0 (i.e. >100%),
  # treat as suspect and re-derive from component fields (monthly_debt_payment,
  # rent_est, annual_income) via dti5. The dti5 value is percentage-scaled
  # (e.g. 45 = 45%), so divide by 100 to get decimal ratio.
  out$dti_ratio <- ifelse(
    !is.na(debt_to_income_ratio) & debt_to_income_ratio <= 1, debt_to_income_ratio,
    ifelse(!is.na(out$dti5), out$dti5 / 100.0, NA_real_)
  )

  out$term_month_policy <- 36
  high_72 <- out$risk_grade == "High" &
    !is.na(credit_file_age_months) & credit_file_age_months >= 60 &
    !is.na(months_since_last_delinquency) & months_since_last_delinquency >= 50 &
    !is.na(out$dti_ratio) & out$dti_ratio <= 0.30
  high_60 <- out$risk_grade == "High" &
    !is.na(credit_file_age_months) & credit_file_age_months >= 48 &
    !is.na(months_since_last_delinquency) & months_since_last_delinquency >= 50 &
    !is.na(out$dti_ratio) & out$dti_ratio <= 0.35 &
    !high_72
  med_60 <- out$risk_grade == "Med" &
    !is.na(credit_file_age_months) & credit_file_age_months >= 60 &
    !is.na(months_since_last_delinquency) & months_since_last_delinquency >= 50 &
    !is.na(out$dti_ratio) & out$dti_ratio <= 0.30
  med_48 <- out$risk_grade == "Med" &
    !is.na(credit_file_age_months) & credit_file_age_months >= 48 &
    !is.na(months_since_last_delinquency) & months_since_last_delinquency >= 50 &
    !is.na(out$dti_ratio) & out$dti_ratio <= 0.35 &
    !med_60
  low_36 <- out$risk_grade == "Low" &
    !is.na(credit_file_age_months) & credit_file_age_months >= 60 &
    !is.na(months_since_last_delinquency) & months_since_last_delinquency >= 50 &
    !is.na(out$dti_ratio) & out$dti_ratio <= 0.30

  out$term_month_policy[out$risk_grade == "High"] <- 48
  out$term_month_policy[high_60] <- 60
  out$term_month_policy[high_72] <- 72

  out$term_month_policy[out$risk_grade == "Med"] <- 36
  out$term_month_policy[med_48] <- 48
  out$term_month_policy[med_60] <- 60

  out$term_month_policy[out$risk_grade == "Low"] <- 24
  out$term_month_policy[low_36] <- 36

  # ===========================================================================
  # Layer 80: Affordability and Offered Amount Section
  # ===========================================================================
  out$offered_term_months <- ifelse(
    is.na(term_months),
    out$term_month_policy,
    pmin(term_months, out$term_month_policy)
  )

  out$offered_final_apr <- ifelse(
    out$offered_term_months == 72,
    pmin(out$adjusted_apr_pre_term_cap, 0.18),
    out$adjusted_apr_pre_term_cap
  )

  out$monthly_post_tax_income <- out$post_tax_income / 12.0
  out$pti_cap <- ifelse(
    out$risk_grade == "High", 0.25,
    ifelse(out$risk_grade == "Med", 0.20, ifelse(out$risk_grade == "Low", 0.15, 0.20))
  )
  # ALIGNED with scripts/gen_policy.py: rent is a living expense, not debt
  # service, and is excluded from the affordability obligation. The previous
  # inclusion of rent_est caused the R engine to decline ~70% of Python-
  # approved PL-CURRENT loans on re-run. See scripts/gen_policy.py:147-153
  # and policy_params_engine.R for the matching fix.
  out$existing_monthly_obligation <- ifelse(is.na(monthly_debt_payment), 0, monthly_debt_payment)
  out$max_new_installment <- (out$pti_cap * out$monthly_post_tax_income) - out$existing_monthly_obligation
  out$r <- out$offered_final_apr / 12.0
  out$n <- out$offered_term_months

  out$affordability_limit_cap <- ifelse(
    out$max_new_installment <= 0, 0,
    ifelse(
      out$r == 0, out$max_new_installment * out$n,
      out$max_new_installment * ((1 + out$r)^out$n - 1) / (out$r * (1 + out$r)^out$n)
    )
  )
  out$maximum_offered_amount <- sql_least(out$maximum_approved_amount, out$affordability_limit_cap)

  # ===========================================================================
  # Layer 90: Offered Decision Section
  # ===========================================================================
  out$offered_decision_flag <- apply_decline_rules(
    out, fico_col = "fico_score", file_age_col = "credit_file_age_months",
    delinq_col = "delinquent_accounts_count", dti_col = "dti_ratio",
    max_amt_col = "maximum_offered_amount"
  )

  # ===========================================================================
  # Layer 99: Final Output Projection
  # ===========================================================================
  projection <- c(required_master_input_columns, core_master_output_columns)
  out[, projection, drop = FALSE]
}


# Backward-compatible alias for master callers.
apply_master_credit_policy <- apply_credit_policy
