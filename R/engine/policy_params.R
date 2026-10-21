#!/usr/bin/env Rscript
# ===========================================================================
# policy_params_engine.R — Shared policy parameter manifest & engine
#
# Sourced by both app.R (Policy Editor tab) and whatif_sandbox.R.
# Contains:
#   - policy_manifest        (28 entries, 39 adjustable params)
#   - apply_policy_with_params(df, params)  — full L10-L90 parameterised engine
#   - build_default_params(manifest)        — named list from manifest defaults
#   - compute_kpis(df)                      — approval rate, avg APR, etc.
#   - validate_params(params, manifest)     — 9-category validation
#   - compute_cascade_impact(params, default_params, manifest) — impact display
#   - cascade_map / layer_names             — static dependency data
# ===========================================================================

# ===========================================================================
# SHARED DECLINE WATERFALL
# IMPORTANT: This definition intentionally REPLACES the stub in
# personal_loan_baseline_policy_master_canonical.R (sourced first by
# 01_packages.R). The canonical stub does not support n_hard_inq_6m_max,
# revol_utl_max, or the thin-file re-entry band — silently dropping them
# via `...`. Guarding with `!exists(...)` would skip this definition and
# cause all inquiry-based decline rules to be no-ops.
# ===========================================================================
{
  apply_decline_rules <- function(df, fico_col, file_age_col, delinq_col, dti_col, max_amt_col,
                                  fico_min = 300, fico_max = 850, thin_file_mo = 30,
                                  delinq_thresh = 1, dti_thresh = 0.50,
                                  n_hard_inq_6m_max = 99L,
                                  thin_file_reentry_delinq_gate = 99L,
                                  thin_file_reentry_min = NULL,
                                  thin_file_reentry_max = NULL,
                                  revol_utl_max = 1.0) {
    flag <- rep("APPR", nrow(df))
    c1 <- is.na(df[[fico_col]]) | df[[fico_col]] > fico_max | df[[fico_col]] < fico_min
    flag[c1] <- "01. Fico"

    # Thin file check with optional re-entry band.
    # Re-entry logic: applicants in the [reentry_min, reentry_max] band who
    # pass the delinquency gate are allowed through even if their file is
    # thinner than thin_file_mo. This models the "swap-in" of previously
    # declined segments under a relaxed policy, without opening the gate
    # for everyone thin.
    c2_base <- !c1 & !is.na(df[[file_age_col]]) & df[[file_age_col]] <= thin_file_mo
    if (!is.null(thin_file_reentry_min) && !is.null(thin_file_reentry_max) &&
        !is.null(thin_file_reentry_delinq_gate) && thin_file_reentry_delinq_gate < 99) {
      in_band <- !is.na(df[[file_age_col]]) &
                 df[[file_age_col]] > thin_file_reentry_min &
                 df[[file_age_col]] <= thin_file_reentry_max
      gate_pass <- !is.na(df[[delinq_col]]) & df[[delinq_col]] <= thin_file_reentry_delinq_gate
      reentry_eligible <- in_band & gate_pass
      c2 <- c2_base & !reentry_eligible
    } else {
      c2 <- c2_base
    }
    flag[c2] <- "02. Thin file"

    c3 <- !(c1 | c2) & !is.na(df[[delinq_col]]) & df[[delinq_col]] > delinq_thresh
    flag[c3] <- "03. Delinquency decline"
    c4 <- !(c1 | c2 | c3) & !is.na(df[[dti_col]]) & df[[dti_col]] > dti_thresh
    flag[c4] <- "04. HighDTI"
    if (max_amt_col %in% names(df)) {
      c5 <- !(c1 | c2 | c3 | c4) & !is.na(df[[max_amt_col]]) & df[[max_amt_col]] < 1
      flag[c5] <- "05. max_amount_limit"
    }
    already_declined <- c1 | c2 | c3 | c4 |
      (if (max_amt_col %in% names(df)) (!is.na(df[[max_amt_col]]) & df[[max_amt_col]] < 1 & !(c1 | c2 | c3 | c4)) else rep(FALSE, nrow(df)))

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
}

# ===========================================================================
# PARAMETER MANIFEST (28 entries)
# ===========================================================================
policy_manifest <- list(
  # --- Grading --- Layer 20 risk waterfall
  list(id = "m1_rule_na",               group = "Grading", subtab = "Model 1", label = "NA score",
       type = "display", default = "Band 3", unit = "", layers = c(20),
       tip = "Missing score defaults to Band 3 (fixed)"),
  list(id = "m1_rule_zero",             group = "Grading", subtab = "Model 1", label = "Score == 0",
       type = "display", default = "Band 1", unit = "", layers = c(20),
       tip = "Zero score maps to Band 1 (fixed)"),
  list(id = "model_score_1_cutoff",     group = "Grading", subtab = "Model 1", label = "Score < cutoff \u2192 Band 2",
       type = "slider", default = 0.016, min = 0.005, max = 0.030, step = 0.001,
       choices = NULL, unit = "", layers = c(20),
       tip = "Scores below this \u2192 Band 2; above \u2192 Band 3 (or 4)"),
  list(id = "model_score_1_upper",      group = "Grading", subtab = "Model 1", label = "Score < upper \u2192 Band 3",
       type = "slider", default = 0.022, min = 0.015, max = 0.040, step = 0.001,
       choices = NULL, unit = "", layers = c(20),
       tip = "Scores below this \u2192 Band 3; at or above \u2192 Band 4"),
  list(id = "m1_rule_else",             group = "Grading", subtab = "Model 1", label = "Score >= upper",
       type = "display", default = "Band 4", unit = "", layers = c(20),
       tip = "Scores at or above upper cutoff map to Band 4 (fixed)"),

  # Model 2: band assignment matrix (8 rules + fallback, 4 adjustable cutoffs)
  list(id = "model_score_2_band1",      group = "Grading", subtab = "Model 2", label = "M1-Band1 & score < cutoff \u2192 Band 1",
       type = "slider", default = 0.003, min = 0.001, max = 0.010, step = 0.001,
       choices = NULL, unit = "", layers = c(20),
       tip = "Model 2 threshold for Band 1 (when Model 1 = Band 1 or 2)"),
  list(id = "model_score_2_band2",      group = "Grading", subtab = "Model 2", label = "M1-Band1 & score < cutoff \u2192 Band 2",
       type = "slider", default = 0.005, min = 0.002, max = 0.012, step = 0.001,
       choices = NULL, unit = "", layers = c(20),
       tip = "Model 2 threshold for Band 2 (when Model 1 = Band 1 or 2)"),
  list(id = "model_score_2_band3",      group = "Grading", subtab = "Model 2", label = "M1-Band3 & score < cutoff \u2192 Band 3",
       type = "slider", default = 0.008, min = 0.004, max = 0.015, step = 0.001,
       choices = NULL, unit = "", layers = c(20),
       tip = "Model 2 threshold for Band 3 (when Model 1 = Band 1 or 3)"),
  list(id = "model_score_2_band4",      group = "Grading", subtab = "Model 2", label = "M1-Band4 & score < cutoff \u2192 Band 4",
       type = "slider", default = 0.009, min = 0.005, max = 0.018, step = 0.001,
       choices = NULL, unit = "", layers = c(20),
       tip = "Model 2 threshold for Band 4 (when Model 1 = Band 1 or 4)"),
  list(id = "m2_rule_fallback",         group = "Grading", subtab = "Model 2", label = "Fallback (no match)",
       type = "display", default = "Band 2", unit = "", layers = c(20),
       tip = "Unmatched cases default to Band 2 (fixed)"),

  # Model 3: final adjustment (6 rules + fallback, 6 adjustable cutoffs)
  list(id = "model_score_3_r1_tight",   group = "Grading", subtab = "Model 3", label = "IntRank 1 & score < cutoff \u2192 Rank 1",
       type = "slider", default = 0.02, min = 0.005, max = 0.05, step = 0.005,
       choices = NULL, unit = "", layers = c(20),
       tip = "Tight cutoff for intermediate rank 1 \u2192 final rank 1"),
  list(id = "model_score_3_r1_loose",   group = "Grading", subtab = "Model 3", label = "IntRank 1 & score < cutoff \u2192 Rank 2",
       type = "slider", default = 0.03, min = 0.010, max = 0.06, step = 0.005,
       choices = NULL, unit = "", layers = c(20),
       tip = "Loose cutoff for intermediate rank 1 \u2192 final rank 2 (step-up from tight)"),
  list(id = "model_score_3_r2_keep",    group = "Grading", subtab = "Model 3", label = "IntRank 2 & score < cutoff \u2192 Rank 1",
       type = "slider", default = 0.04, min = 0.015, max = 0.08, step = 0.005,
       choices = NULL, unit = "", layers = c(20),
       tip = "Cutoff for intermediate rank 2 \u2192 upgrades to final rank 1"),
  list(id = "model_score_3_r2_upgrade", group = "Grading", subtab = "Model 3", label = "IntRank 2 & score < cutoff \u2192 Rank 2",
       type = "slider", default = 0.06, min = 0.030, max = 0.10, step = 0.005,
       choices = NULL, unit = "", layers = c(20),
       tip = "Cutoff for intermediate rank 2 \u2192 stays final rank 2"),
  list(id = "model_score_3_r3",         group = "Grading", subtab = "Model 3", label = "IntRank 3 & score < cutoff \u2192 Rank 3",
       type = "slider", default = 0.065, min = 0.030, max = 0.10, step = 0.005,
       choices = NULL, unit = "", layers = c(20),
       tip = "Cutoff for intermediate rank 3 \u2192 final rank 3"),
  list(id = "model_score_3_r4",         group = "Grading", subtab = "Model 3", label = "IntRank 4 & score < cutoff \u2192 Rank 4",
       type = "slider", default = 0.08, min = 0.040, max = 0.12, step = 0.005,
       choices = NULL, unit = "", layers = c(20),
       tip = "Cutoff for intermediate rank 4 \u2192 final rank 4"),
  list(id = "m3_rule_fallback",         group = "Grading", subtab = "Model 3", label = "Fallback (no match)",
       type = "display", default = "Rank 2", unit = "", layers = c(20),
       tip = "Unmatched cases default to final Rank 2 (fixed)"),

  # Branch: tier mapping conditions + fixed tier matrix
  list(id = "branch_file_age_threshold", group = "Grading", subtab = "Branch", label = "File Age >= threshold",
       type = "slider", default = 25, min = 12, max = 48, step = 1,
       choices = NULL, unit = "mo", layers = c(20),
       tip = "Internal file age cutoff for branch_1 (both conditions must be true)"),
  list(id = "branch_delinq_months",     group = "Grading", subtab = "Branch", label = "Delinq Months < threshold",
       type = "slider", default = 50, min = 12, max = 72, step = 1,
       choices = NULL, unit = "mo", layers = c(20),
       tip = "Months since last delinquency cutoff for branch_1 (both conditions must be true)"),
  list(id = "branch_rule_tier",         group = "Grading", subtab = "Branch", label = "Tier mapping matrix",
       type = "display", default = "B1: R1\u21921, R2\u21922, R3/4\u21923 | B2: R1\u21921, R2\u21923, R3/4\u21924", unit = "", layers = c(20),
       tip = "Branch \u00d7 FinalRank \u2192 UW Tier (fixed)"),

  # --- Eligibility (5) --- Layers 60, 90
  list(id = "fico_min",         group = "Decline Rules", subtab = "All", label = "FICO Minimum",
       type = "slider", default = 300,  min = 300,  max = 600,  step = 10,
       choices = NULL, unit = "", layers = c(60, 90), tip = "Minimum FICO score to approve"),
  list(id = "fico_max",         group = "Decline Rules", subtab = "All", label = "FICO Maximum",
       type = "slider", default = 850,  min = 700,  max = 900,  step = 10,
       choices = NULL, unit = "", layers = c(60, 90), tip = "Maximum FICO score (above = decline)"),
  list(id = "thin_file_months", group = "Decline Rules", subtab = "All", label = "Thin File Threshold",
       type = "slider", default = 30,   min = 6,    max = 60,   step = 6,
       choices = NULL, unit = "mo", layers = c(60, 90), tip = "Min credit file age; below = thin file decline"),
  list(id = "delinquency_max",  group = "Decline Rules", subtab = "All", label = "Max Delinquent Accounts",
       type = "slider", default = 1,    min = 0,    max = 5,    step = 1,
       choices = NULL, unit = "", layers = c(60, 90), tip = "Max delinquent accounts before decline"),
  list(id = "dti_cutoff",       group = "Decline Rules", subtab = "All", label = "DTI Cutoff",
       type = "slider", default = 0.50, min = 0.30, max = 0.70, step = 0.01,
       choices = NULL, unit = "", layers = c(60, 90), tip = "DTI ratio above which applicants are declined"),

  list(id = "n_hard_inq_6m_max", group = "Decline Rules", subtab = "Underwriting",
       label = "Max Hard Inquiries (6M)", type = "slider",
       default = 99L, min = 0L, max = 10L, step = 1L, unit = "",
       choices = NULL, layers = c(60L, 90L),
       tip = "Decline if N_HARD_INQ_6M >= this value. Default 99 = no inquiry rule."),

  list(id = "thin_file_reentry_delinq_gate", group = "Decline Rules", subtab = "Underwriting",
       label = "Re-entry Max Delinquencies", type = "slider",
       default = 99L, min = 0L, max = 5L, step = 1L, unit = "",
       choices = NULL, layers = c(60L, 90L),
       tip = "Max delinquent accounts for thin-file borrowers to qualify for re-entry"),

  list(id = "revol_utl_max", group = "Decline Rules", subtab = "Underwriting",
       label = "Max Revolving Utilization", type = "slider",
       default = 1.0, min = 0.50, max = 1.0, step = 0.05, unit = "%",
       choices = NULL, layers = c(60L, 90L),
       tip = "Decline if REVOL_UTL > this. Default 1.0 = no utilization rule."),

  # --- Pricing (7) --- Layers 30, 40, 50
  list(id = "apr_high",          group = "Pricing", subtab = "Base APR", label = "High Grade",
       type = "slider", default = 0.07, min = 0.04, max = 0.15, step = 0.005,
       choices = NULL, unit = "%", layers = c(30), tip = "Base APR for High (best) risk grade"),
  list(id = "apr_med",           group = "Pricing", subtab = "Base APR", label = "Med Grade",
       type = "slider", default = 0.12, min = 0.08, max = 0.22, step = 0.005,
       choices = NULL, unit = "%", layers = c(30), tip = "Base APR for Med (middle) risk grade"),
  list(id = "apr_low",           group = "Pricing", subtab = "Base APR", label = "Low Grade",
       type = "slider", default = 0.16, min = 0.10, max = 0.25, step = 0.005,
       choices = NULL, unit = "%", layers = c(30), tip = "Base APR for Low (worst) risk grade"),
  list(id = "variant_a_add",     group = "Pricing", subtab = "Variant", label = "Variant A Add-on",
       type = "slider", default = 0.05, min = 0.00, max = 0.15, step = 0.005,
       choices = NULL, unit = "%", layers = c(40), tip = "APR add-on for Direct channel Variant A"),
  list(id = "variant_b_add",     group = "Pricing", subtab = "Variant", label = "Variant B Add-on",
       type = "slider", default = 0.10, min = 0.00, max = 0.20, step = 0.005,
       choices = NULL, unit = "%", layers = c(40), tip = "APR add-on for Direct channel Variant B"),
  list(id = "variant_fico_floor", group = "Pricing", subtab = "Variant", label = "FICO Floor",
       type = "slider", default = 660,  min = 580,  max = 760,  step = 10,
       choices = NULL, unit = "", layers = c(40), tip = "FICO floor for variant pricing eligibility"),
  list(id = "term72_apr_cap",    group = "Pricing", subtab = "Term Cap", label = "72-Month APR Cap",
       type = "slider", default = 0.18, min = 0.12, max = 0.25, step = 0.005,
       choices = NULL, unit = "%", layers = c(50, 80), tip = "Max APR allowed on 72-month terms"),

  # --- Loan Limits (2) --- Layer 50
  list(id = "exposure_cap_high",  group = "Loan Limits", subtab = "All", label = "Exposure Cap \u2014 High Grade",
       type = "slider", default = 50000, min = 10000, max = 100000, step = 5000,
       choices = NULL, unit = "$", layers = c(50), tip = "Max loan amount for High grade (best quality)"),
  list(id = "exposure_cap_other", group = "Loan Limits", subtab = "All", label = "Exposure Cap \u2014 Other Grades",
       type = "slider", default = 40000, min = 10000, max = 100000, step = 5000,
       choices = NULL, unit = "$", layers = c(50), tip = "Max loan amount for Med/Low grades"),

  # --- Affordability (3) --- Layer 80
  list(id = "pti_high", group = "Affordability", subtab = "All", label = "PTI Cap \u2014 High Grade",
       type = "slider", default = 0.25, min = 0.10, max = 0.45, step = 0.01,
       choices = NULL, unit = "", layers = c(80), tip = "Payment-to-income cap for High grade (best quality)"),
  list(id = "pti_med",  group = "Affordability", subtab = "All", label = "PTI Cap \u2014 Med Grade",
       type = "slider", default = 0.20, min = 0.10, max = 0.40, step = 0.01,
       choices = NULL, unit = "", layers = c(80), tip = "Payment-to-income cap for Med grade"),
  list(id = "pti_low",  group = "Affordability", subtab = "All", label = "PTI Cap \u2014 Low Grade",
       type = "slider", default = 0.15, min = 0.10, max = 0.35, step = 0.01,
       choices = NULL, unit = "", layers = c(80), tip = "Payment-to-income cap for Low grade (weakest quality)"),

  # --- Tenure (8) --- Layer 70
  list(id = "term_base_high",          group = "Tenure", subtab = "Base Term", label = "High Grade",
       type = "select", default = 48, min = NA, max = NA, step = NA,
       choices = c(24, 36, 48, 60, 72), unit = "mo", layers = c(70), tip = "Base term for High grade"),
  list(id = "term_base_med",           group = "Tenure", subtab = "Base Term", label = "Med Grade",
       type = "select", default = 36, min = NA, max = NA, step = NA,
       choices = c(24, 36, 48, 60), unit = "mo", layers = c(70), tip = "Base term for Med grade"),
  list(id = "term_base_low",           group = "Tenure", subtab = "Base Term", label = "Low Grade",
       type = "select", default = 24, min = NA, max = NA, step = NA,
       choices = c(24, 36), unit = "mo", layers = c(70), tip = "Base term for Low grade"),
  list(id = "term_ext_file_age_upper", group = "Tenure", subtab = "Extension Rules", label = "File Age \u2014 Upper Tier",
       type = "slider", default = 60, min = 36, max = 84, step = 6,
       choices = NULL, unit = "mo", layers = c(70), tip = "File age threshold for top extension tier"),
  list(id = "term_ext_file_age_lower", group = "Tenure", subtab = "Extension Rules", label = "File Age \u2014 Lower Tier",
       type = "slider", default = 48, min = 24, max = 72, step = 6,
       choices = NULL, unit = "mo", layers = c(70), tip = "File age threshold for middle extension tier"),
  list(id = "term_ext_delinq_free",    group = "Tenure", subtab = "Extension Rules", label = "Delinq-Free Months",
       type = "slider", default = 50, min = 12, max = 72, step = 6,
       choices = NULL, unit = "mo", layers = c(70), tip = "Months since last delinquency for extension"),
  list(id = "term_ext_dti_upper",      group = "Tenure", subtab = "Extension Rules", label = "DTI Ceiling \u2014 Upper",
       type = "slider", default = 0.30, min = 0.15, max = 0.50, step = 0.05,
       choices = NULL, unit = "", layers = c(70), tip = "DTI ceiling for top extension tier"),
  list(id = "term_ext_dti_lower",      group = "Tenure", subtab = "Extension Rules", label = "DTI Ceiling \u2014 Lower",
       type = "slider", default = 0.35, min = 0.20, max = 0.55, step = 0.05,
       choices = NULL, unit = "", layers = c(70), tip = "DTI ceiling for middle extension tier")
)

# ===========================================================================
# BUILD DEFAULT PARAMS — named list from manifest defaults
# ===========================================================================
build_default_params <- function(manifest = policy_manifest) {
  params <- list()
  for (p in manifest) {
    if (identical(p$type, "display")) next
    val <- p$default
    if (p$type == "select") val <- as.integer(val)
    params[[p$id]] <- val
  }
  params
}

# ===========================================================================
# PARAMETERISED POLICY WRAPPER  (Layers 10-90)
# ===========================================================================
apply_policy_with_params <- function(df, params) {
  n   <- nrow(df)
  out <- df

  # =========================================================================
  # Layer 10: Feature Derivation
  # =========================================================================
  monthly_rent    <- out$monthly_rent
  annual_income_v <- out$annual_income
  mdp_v           <- out$monthly_debt_payment
  out$rent_est <- ifelse(!is.na(monthly_rent), pmax(monthly_rent, 300), NA_real_)
  out$dti5 <- ifelse(
    !is.na(annual_income_v) & annual_income_v > 0 & !is.na(mdp_v) & !is.na(out$rent_est),
    round(((mdp_v + out$rent_est) / annual_income_v) * 100, 2),
    NA_real_
  )
  out$social_tax <- ifelse(!is.na(annual_income_v) & annual_income_v < 140000, annual_income_v * 0.06, 0)
  out$med_tax <- annual_income_v * 0.04
  out$post_tax_income <- annual_income_v - out$social_tax - out$med_tax

  # Local aliases
  fico_score                    <- out$fico_score
  credit_file_age_months        <- out$credit_file_age_months
  delinquent_accounts_count     <- out$delinquent_accounts_count
  months_since_last_delinquency <- out$months_since_last_delinquency
  debt_to_income_ratio          <- out$debt_to_income_ratio
  term_months                   <- out$term_months
  partner_channel               <- out$partner_channel
  pricing_test_variant          <- out$pricing_test_variant
  monthly_debt_payment          <- out$monthly_debt_payment
  annual_income                 <- out$annual_income
  risk_model_score_1            <- out$risk_model_score_1
  risk_model_score_2            <- out$risk_model_score_2
  risk_model_score_3            <- out$risk_model_score_3
  internal_file_age_months      <- out$internal_file_age_months

  # =========================================================================
  # Layer 20: Grading Section (PARAMETERISED)
  # =========================================================================
  `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

  s1_cutoff <- params$model_score_1_cutoff %||% 0.016
  s1_upper  <- params$model_score_1_upper  %||% 0.022
  out$model_1_risk_band <- ifelse(
    is.na(risk_model_score_1), 3L,
    ifelse(
      risk_model_score_1 == 0, 1L,
      ifelse(
        risk_model_score_1 < s1_cutoff, 2L,
        ifelse(risk_model_score_1 < s1_upper, 3L, 4L)
      )
    )
  )

  s2_b1 <- params$model_score_2_band1 %||% 0.003
  s2_b2 <- params$model_score_2_band2 %||% 0.005
  s2_b3 <- params$model_score_2_band3 %||% 0.008
  s2_b4 <- params$model_score_2_band4 %||% 0.009
  out$model_2_risk_band <- rep(NA_integer_, n)
  out$model_2_risk_band <- first_hit_assign(out$model_2_risk_band, !is.na(risk_model_score_2) & risk_model_score_2 < s2_b1 & out$model_1_risk_band == 1, 1L)
  out$model_2_risk_band <- first_hit_assign(out$model_2_risk_band, !is.na(risk_model_score_2) & risk_model_score_2 < s2_b2 & out$model_1_risk_band == 1, 2L)
  out$model_2_risk_band <- first_hit_assign(out$model_2_risk_band, !is.na(risk_model_score_2) & risk_model_score_2 < s2_b3 & out$model_1_risk_band == 1, 3L)
  out$model_2_risk_band <- first_hit_assign(out$model_2_risk_band, !is.na(risk_model_score_2) & risk_model_score_2 < s2_b4 & out$model_1_risk_band == 1, 4L)
  out$model_2_risk_band <- first_hit_assign(out$model_2_risk_band, !is.na(risk_model_score_2) & risk_model_score_2 < s2_b1 & out$model_1_risk_band == 2, 1L)
  out$model_2_risk_band <- first_hit_assign(out$model_2_risk_band, !is.na(risk_model_score_2) & risk_model_score_2 < s2_b2 & out$model_1_risk_band == 2, 2L)
  out$model_2_risk_band <- first_hit_assign(out$model_2_risk_band, !is.na(risk_model_score_2) & risk_model_score_2 < s2_b3 & out$model_1_risk_band == 3, 3L)
  out$model_2_risk_band <- first_hit_assign(out$model_2_risk_band, !is.na(risk_model_score_2) & risk_model_score_2 < s2_b4 & out$model_1_risk_band == 4, 4L)
  out$model_2_risk_band[is.na(out$model_2_risk_band)] <- 2L

  # Intermediate risk rank
  out$intermediate_risk_rank <- rep(NA_integer_, n)
  out$intermediate_risk_rank <- first_hit_assign(out$intermediate_risk_rank, out$model_1_risk_band == 1 & out$model_2_risk_band == 1, 1L)
  out$intermediate_risk_rank <- first_hit_assign(out$intermediate_risk_rank, out$model_1_risk_band == 1 & out$model_2_risk_band == 2, 1L)
  out$intermediate_risk_rank <- first_hit_assign(out$intermediate_risk_rank, out$model_1_risk_band == 2 & out$model_2_risk_band == 2, 2L)
  out$intermediate_risk_rank <- first_hit_assign(out$intermediate_risk_rank, out$model_1_risk_band == 2 & out$model_2_risk_band == 3, 1L)
  out$intermediate_risk_rank <- first_hit_assign(out$intermediate_risk_rank, out$model_1_risk_band == 3 & out$model_2_risk_band == 3, 3L)
  out$intermediate_risk_rank <- first_hit_assign(out$intermediate_risk_rank, out$model_1_risk_band == 4 & out$model_2_risk_band == 3, 4L)
  out$intermediate_risk_rank <- first_hit_assign(out$intermediate_risk_rank, out$model_1_risk_band == 4 & out$model_2_risk_band == 4, 4L)
  out$intermediate_risk_rank[is.na(out$intermediate_risk_rank)] <- 3L

  # Final model adjusted rank
  s3_r1t <- params$model_score_3_r1_tight   %||% 0.02
  s3_r1l <- params$model_score_3_r1_loose   %||% 0.03
  s3_r2k <- params$model_score_3_r2_keep    %||% 0.04
  s3_r2u <- params$model_score_3_r2_upgrade %||% 0.06
  s3_r3  <- params$model_score_3_r3         %||% 0.065
  s3_r4  <- params$model_score_3_r4         %||% 0.08
  out$final_model_adjusted_rank <- rep(NA_integer_, n)
  out$final_model_adjusted_rank <- first_hit_assign(out$final_model_adjusted_rank, !is.na(risk_model_score_3) & out$intermediate_risk_rank == 1 & risk_model_score_3 < s3_r1t, 1L)
  out$final_model_adjusted_rank <- first_hit_assign(out$final_model_adjusted_rank, !is.na(risk_model_score_3) & out$intermediate_risk_rank == 1 & risk_model_score_3 < s3_r1l, 2L)
  out$final_model_adjusted_rank <- first_hit_assign(out$final_model_adjusted_rank, !is.na(risk_model_score_3) & out$intermediate_risk_rank == 2 & risk_model_score_3 < s3_r2k, 1L)
  out$final_model_adjusted_rank <- first_hit_assign(out$final_model_adjusted_rank, !is.na(risk_model_score_3) & out$intermediate_risk_rank == 2 & risk_model_score_3 < s3_r2u, 2L)
  out$final_model_adjusted_rank <- first_hit_assign(out$final_model_adjusted_rank, !is.na(risk_model_score_3) & out$intermediate_risk_rank == 3 & risk_model_score_3 < s3_r3, 3L)
  out$final_model_adjusted_rank <- first_hit_assign(out$final_model_adjusted_rank, !is.na(risk_model_score_3) & out$intermediate_risk_rank == 4 & risk_model_score_3 < s3_r4, 4L)
  out$final_model_adjusted_rank[is.na(out$final_model_adjusted_rank)] <- 2L

  # Branch tier mapping
  branch_age_thresh    <- params$branch_file_age_threshold %||% 25
  branch_delinq_thresh <- params$branch_delinq_months      %||% 50
  branch_1 <- (!is.na(internal_file_age_months) & internal_file_age_months >= branch_age_thresh) &
    (!is.na(months_since_last_delinquency) & months_since_last_delinquency >= branch_delinq_thresh)
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

  # =========================================================================
  # Layer 30: Base Pricing
  # =========================================================================
  out$risk_grade <- ifelse(
    out$underwriting_risk_tier == 1, "High",
    ifelse(
      out$underwriting_risk_tier == 2, "Med",
      ifelse(out$underwriting_risk_tier %in% c(3, 4), "Low", NA_character_)
    )
  )

  out$base_apr <- ifelse(
    out$risk_grade == "High", params$apr_high,
    ifelse(out$risk_grade == "Med", params$apr_med,
           ifelse(out$risk_grade == "Low", params$apr_low, NA_real_))
  )

  # =========================================================================
  # Layer 40: Channel / Variant Pricing
  # =========================================================================
  out$adjusted_apr_pre_term_cap <- out$base_apr

  idx_a <- which(
    !is.na(partner_channel) & partner_channel == "Direct" &
    !is.na(fico_score) & fico_score > params$variant_fico_floor &
    !is.na(pricing_test_variant) & pricing_test_variant == "A"
  )
  out$adjusted_apr_pre_term_cap[idx_a] <- out$base_apr[idx_a] + params$variant_a_add

  idx_b <- which(
    !is.na(partner_channel) & partner_channel == "Direct" &
    !is.na(fico_score) & fico_score > params$variant_fico_floor &
    !is.na(pricing_test_variant) & pricing_test_variant == "B"
  )
  out$adjusted_apr_pre_term_cap[idx_b] <- out$base_apr[idx_b] + params$variant_b_add

  # =========================================================================
  # Layer 50: Maximum Amount
  # =========================================================================
  out$final_apr <- ifelse(
    !is.na(term_months) & term_months == 72,
    pmin(out$adjusted_apr_pre_term_cap, params$term72_apr_cap),
    out$adjusted_apr_pre_term_cap
  )

  out$income_based_limit_cap <- (out$post_tax_income -
    12 * monthly_debt_payment) / (1 - out$final_apr)

  out$exposure_limit_cap <- ifelse(
    out$risk_grade == "High", params$exposure_cap_high, params$exposure_cap_other
  )

  out$maximum_approved_amount <- sql_least(out$income_based_limit_cap, out$exposure_limit_cap)

  # =========================================================================
  # Layer 60: Base Decision
  # =========================================================================
  out$decision_flag <- apply_decline_rules(
    out, fico_col = "fico_score", file_age_col = "credit_file_age_months",
    delinq_col = "delinquent_accounts_count", dti_col = "debt_to_income_ratio",
    max_amt_col = "maximum_approved_amount",
    fico_min = params$fico_min, fico_max = params$fico_max,
    thin_file_mo = params$thin_file_months, delinq_thresh = params$delinquency_max,
    dti_thresh = params$dti_cutoff,
    n_hard_inq_6m_max = params$n_hard_inq_6m_max %||% 99L,
    thin_file_reentry_delinq_gate = params$thin_file_reentry_delinq_gate %||% 99L,
    thin_file_reentry_min = params$thin_file_reentry_min,
    thin_file_reentry_max = params$thin_file_reentry_max,
    revol_utl_max = params$revol_utl_max %||% 1.0
  )

  # =========================================================================
  # Layer 70: Tenure Policy
  # =========================================================================
  out$dti_ratio <- ifelse(
    !is.na(debt_to_income_ratio) & debt_to_income_ratio <= 1,
    debt_to_income_ratio,
    ifelse(!is.na(out$dti5), out$dti5 / 100.0, NA_real_)
  )

  out$term_month_policy <- ifelse(
    out$risk_grade == "High", params$term_base_high,
    ifelse(out$risk_grade == "Med", params$term_base_med,
           ifelse(out$risk_grade == "Low", params$term_base_low, 36))
  )

  upper_cond <-
    !is.na(credit_file_age_months) & credit_file_age_months >= params$term_ext_file_age_upper &
    !is.na(months_since_last_delinquency) & months_since_last_delinquency >= params$term_ext_delinq_free &
    !is.na(out$dti_ratio) & out$dti_ratio <= params$term_ext_dti_upper

  lower_cond <-
    !is.na(credit_file_age_months) & credit_file_age_months >= params$term_ext_file_age_lower &
    !is.na(months_since_last_delinquency) & months_since_last_delinquency >= params$term_ext_delinq_free &
    !is.na(out$dti_ratio) & out$dti_ratio <= params$term_ext_dti_lower

  high_upper <- out$risk_grade == "High" & upper_cond
  high_lower <- out$risk_grade == "High" & lower_cond & !high_upper
  out$term_month_policy[high_upper] <- params$term_base_high + 24
  out$term_month_policy[high_lower] <- params$term_base_high + 12

  med_upper <- out$risk_grade == "Med" & upper_cond
  med_lower <- out$risk_grade == "Med" & lower_cond & !med_upper
  out$term_month_policy[med_upper] <- params$term_base_med + 24
  out$term_month_policy[med_lower] <- params$term_base_med + 12

  low_upper <- out$risk_grade == "Low" & upper_cond
  out$term_month_policy[low_upper] <- params$term_base_low + 12

  # =========================================================================
  # Layer 80: Affordability and Offered Amount
  # =========================================================================
  out$offered_term_months <- ifelse(
    is.na(term_months),
    out$term_month_policy,
    pmin(term_months, out$term_month_policy)
  )

  out$offered_final_apr <- ifelse(
    out$offered_term_months == 72,
    pmin(out$adjusted_apr_pre_term_cap, params$term72_apr_cap),
    out$adjusted_apr_pre_term_cap
  )

  out$monthly_post_tax_income <- out$post_tax_income / 12.0
  out$pti_cap <- ifelse(
    out$risk_grade == "High", params$pti_high,
    ifelse(out$risk_grade == "Med", params$pti_med,
           ifelse(out$risk_grade == "Low", params$pti_low, 0.20))
  )

  # ALIGNED with scripts/gen_policy.py: affordability treats only monthly debt
  # service as existing obligation. Rent is a living expense, not debt service,
  # and was excluded from the data generator's L80 calculation to avoid
  # over-tightening. Including rent here caused the R re-run to decline
  # ~70% of the Python-approved PL-CURRENT loans via Layer 90 max_amount_limit,
  # making Scanner take-rates hit the 100% safety cap for every segment.
  # See scripts/gen_policy.py:147-153.
  out$existing_monthly_obligation <-
    ifelse(is.na(monthly_debt_payment), 0, monthly_debt_payment)

  out$max_new_installment <-
    (out$pti_cap * out$monthly_post_tax_income) - out$existing_monthly_obligation

  out$r <- out$offered_final_apr / 12.0
  out$n <- out$offered_term_months

  out$affordability_limit_cap <- ifelse(
    out$max_new_installment <= 0, 0,
    ifelse(
      out$r == 0,
      out$max_new_installment * out$n,
      out$max_new_installment * ((1 + out$r)^out$n - 1) / (out$r * (1 + out$r)^out$n)
    )
  )

  out$maximum_offered_amount <- sql_least(out$maximum_approved_amount, out$affordability_limit_cap)

  # =========================================================================
  # Layer 90: Offered Decision
  # =========================================================================
  out$offered_decision_flag <- apply_decline_rules(
    out, fico_col = "fico_score", file_age_col = "credit_file_age_months",
    delinq_col = "delinquent_accounts_count", dti_col = "dti_ratio",
    max_amt_col = "maximum_offered_amount",
    fico_min = params$fico_min, fico_max = params$fico_max,
    thin_file_mo = params$thin_file_months, delinq_thresh = params$delinquency_max,
    dti_thresh = params$dti_cutoff,
    n_hard_inq_6m_max = params$n_hard_inq_6m_max %||% 99L,
    thin_file_reentry_delinq_gate = params$thin_file_reentry_delinq_gate %||% 99L,
    thin_file_reentry_min = params$thin_file_reentry_min,
    thin_file_reentry_max = params$thin_file_reentry_max,
    revol_utl_max = params$revol_utl_max %||% 1.0
  )

  out
}

# ===========================================================================
# COMPUTE KPIs
# ===========================================================================
compute_kpis <- function(df) {
  n_total    <- nrow(df)
  approved   <- df$offered_decision_flag == "APPR"
  n_approved <- sum(approved, na.rm = TRUE)
  n_declined <- n_total - n_approved
  list(
    total_apps    = n_total,
    approval_rate = if (n_total > 0) n_approved / n_total else 0,
    avg_apr       = mean(df$offered_final_apr[approved], na.rm = TRUE),
    avg_amount    = mean(df$maximum_offered_amount[approved], na.rm = TRUE),
    bad_rate      = mean(df$performance_delinquency_flag[approved & df$issued_flag == 1], na.rm = TRUE),
    decline_vol   = n_declined
  )
}

# ===========================================================================
# CASCADE MAP — Layer dependency graph
# ===========================================================================
cascade_map <- list(
  "20" = c(30, 40, 50, 60, 70, 80, 90),
  "30" = c(40, 50, 80, 90),
  "40" = c(50, 80, 90),
  "50" = c(60, 80, 90),
  "60" = integer(0),
  "70" = c(80, 90),
  "80" = c(90),
  "90" = integer(0)
)

layer_names <- c(
  "20" = "L20 Grading",
  "30" = "L30 Base APR",
  "40" = "L40 Variant",
  "50" = "L50 Limits",
  "60" = "L60 Decline",
  "70" = "L70 Term",
  "80" = "L80 Afford",
  "90" = "L90 Offered"
)

# ===========================================================================
# VALIDATE PARAMS — 9-category validation
# ===========================================================================
validate_params <- function(params, manifest = policy_manifest) {
  errors   <- character(0)
  warnings <- character(0)

  # Helper: get manifest entry by id
  get_entry <- function(id) {
    for (p in manifest) if (p$id == id) return(p)
    NULL
  }

  # Params that use 99L as a "rule disabled" sentinel — skip bounds check
  # when they hold the sentinel value so the UI can represent "not active"
  # policies without triggering false-positive range errors.
  .disabled_sentinels <- list(
    n_hard_inq_6m_max             = 99,
    thin_file_reentry_delinq_gate = 99
  )

  # 1. Range bounds
  for (p in manifest) {
    if (identical(p$type, "display")) next
    val <- params[[p$id]]
    if (is.null(val)) next
    # Skip bounds check when the value equals the rule-disabled sentinel
    sentinel <- .disabled_sentinels[[p$id]]
    if (!is.null(sentinel) && !is.na(val) && val >= sentinel) next
    if (p$type == "slider") {
      if (!is.na(p$min) && val < p$min)
        errors <- c(errors, sprintf("%s (%.4g) is below minimum (%.4g)", p$label, val, p$min))
      if (!is.na(p$max) && val > p$max)
        errors <- c(errors, sprintf("%s (%.4g) is above maximum (%.4g)", p$label, val, p$max))
    }
  }

  # 2. Model 1 cutoff monotonicity
  m1c <- params$model_score_1_cutoff
  m1u <- params$model_score_1_upper
  if (!is.null(m1c) && !is.null(m1u) && m1c >= m1u)
    errors <- c(errors, sprintf("Model 1 cutoff (%.4g) must be < upper (%.4g)", m1c, m1u))

  # 3. Model 2 band cutoff ordering
  b <- c(params$model_score_2_band1, params$model_score_2_band2,
         params$model_score_2_band3, params$model_score_2_band4)
  if (length(b) == 4 && all(!is.null(b))) {
    for (i in seq_len(3)) {
      if (b[i] >= b[i + 1])
        errors <- c(errors, sprintf("Model 2 band%d (%.4g) must be < band%d (%.4g)", i, b[i], i + 1, b[i + 1]))
    }
  }

  # 4. Model 3 rank cutoff ordering
  r1t <- params$model_score_3_r1_tight
  r1l <- params$model_score_3_r1_loose
  if (!is.null(r1t) && !is.null(r1l) && r1t >= r1l)
    errors <- c(errors, sprintf("Model 3 r1_tight (%.4g) must be < r1_loose (%.4g)", r1t, r1l))

  r2k <- params$model_score_3_r2_keep
  r2u <- params$model_score_3_r2_upgrade
  if (!is.null(r2k) && !is.null(r2u) && r2k >= r2u)
    errors <- c(errors, sprintf("Model 3 r2_keep (%.4g) must be < r2_upgrade (%.4g)", r2k, r2u))

  # 5. APR ordering by grade (High = best quality borrowers → expect lowest APR)
  ah <- params$apr_high; am <- params$apr_med; al <- params$apr_low
  if (!is.null(ah) && !is.null(am) && !is.null(al)) {
    if (ah > al)
      warnings <- c(warnings, sprintf("High grade APR (%.1f%%) > Low grade APR (%.1f%%) - High grade (best quality) should have the LOWEST APR", ah * 100, al * 100))
    if (ah > am)
      warnings <- c(warnings, sprintf("High grade APR (%.1f%%) > Med grade APR (%.1f%%) - High grade (best quality) should have a lower APR than Med", ah * 100, am * 100))
  }

  # 6. PTI cap ordering (High = best quality → expect most generous PTI cap)
  ph <- params$pti_high; pm <- params$pti_med; pl <- params$pti_low
  if (!is.null(ph) && !is.null(pm) && !is.null(pl)) {
    if (ph < pl)
      warnings <- c(warnings, sprintf("High grade PTI (%.0f%%) < Low grade PTI (%.0f%%) - High grade (best quality) should have a HIGHER PTI cap", ph * 100, pl * 100))
    if (ph < pm)
      warnings <- c(warnings, sprintf("High grade PTI (%.0f%%) < Med grade PTI (%.0f%%) - High grade (best quality) should have a higher PTI cap than Med", ph * 100, pm * 100))
  }

  # 7. Term extension thresholds
  fau <- params$term_ext_file_age_upper; fal <- params$term_ext_file_age_lower
  if (!is.null(fau) && !is.null(fal) && fau < fal)
    warnings <- c(warnings, sprintf("Upper tier file age (%d mo) < lower tier (%d mo) - unusual", fau, fal))

  dtu <- params$term_ext_dti_upper; dtl <- params$term_ext_dti_lower
  if (!is.null(dtu) && !is.null(dtl) && dtu > dtl)
    warnings <- c(warnings, sprintf("Upper tier DTI ceiling (%.0f%%) > lower tier (%.0f%%) - unusual", dtu * 100, dtl * 100))

  # 8. Positive exposure caps
  ech <- params$exposure_cap_high; eco <- params$exposure_cap_other
  if (!is.null(ech) && ech <= 0)
    errors <- c(errors, "High grade exposure cap must be > 0")
  if (!is.null(eco) && eco <= 0)
    errors <- c(errors, "Other grade exposure cap must be > 0")

  # 9. Cross-layer consistency — L60/L90 share same params, enforced architecturally (no check needed)

  list(
    valid    = length(errors) == 0,
    errors   = errors,
    warnings = warnings
  )
}

# ===========================================================================
# COMPUTE CASCADE IMPACT
# ===========================================================================
compute_cascade_impact <- function(params, default_params, manifest = policy_manifest) {
  changed_ids    <- character(0)
  direct_layers  <- integer(0)

  for (p in manifest) {
    if (identical(p$type, "display")) next
    pid <- p$id
    cur <- params[[pid]]
    def <- default_params[[pid]]
    if (is.null(cur) || is.null(def)) next
    if (!identical(as.numeric(cur), as.numeric(def))) {
      changed_ids   <- c(changed_ids, pid)
      direct_layers <- c(direct_layers, p$layers)
    }
  }

  direct_layers <- sort(unique(direct_layers))

  # Compute cascade (all downstream layers)
  all_layers <- direct_layers
  for (dl in direct_layers) {
    downstream <- cascade_map[[as.character(dl)]]
    if (!is.null(downstream)) all_layers <- c(all_layers, downstream)
  }
  all_layers <- sort(unique(all_layers))

  # Determine impact level
  impact <- if (length(changed_ids) == 0) {
    "none"
  } else if (20 %in% direct_layers) {
    "high"
  } else if (any(c(30, 40, 50, 70, 80) %in% direct_layers)) {
    "medium"
  } else {
    "low"
  }

  list(
    changed_ids   = changed_ids,
    direct_layers = direct_layers,
    all_layers    = all_layers,
    impact        = impact
  )
}
