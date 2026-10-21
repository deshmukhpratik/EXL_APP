# ============================================================
# R/03_globals_policies.R — Strategy manifest loader (slim)
#
# By the time this file is sourced, R/engine/strategy_manifest.R
# has already defined `strategy_overrides` and `build_strategy_fn`
# in the global env. We just:
#   1. Sanity-check strategy_overrides exists
#   2. Build policy_fns — named list of closures, one per policy
#   3. Build mockPolicies, base_loans, vintage_raw via service fns
#   4. Load persisted policies
#   5. Emit a startup summary
# ============================================================

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

if (!exists("strategy_overrides") || !is.list(strategy_overrides) ||
    length(strategy_overrides) == 0) {
  warning("cpt: strategy_overrides missing or empty - Policy Catalog will be empty")
  strategy_overrides <- list()
  policy_fns <- list()
} else {
  if (!exists("build_strategy_fn", mode = "function")) {
    warning("cpt: build_strategy_fn not defined in engine - using identity closures")
    policy_fns <- lapply(strategy_overrides, function(s) function(df) df)
  } else {
    policy_fns <- lapply(strategy_overrides, function(s) {
      params <- s$params %||% list()
      build_strategy_fn(params)
    })
  }

  # Also load the canonical baseline engine for any code that references it by file
  .baseline_fn <- tryCatch({
    e <- new.env(parent = globalenv())
    source("R/engine/baseline_canonical.R", local = e, echo = FALSE)
    e$apply_credit_policy
  }, error = function(err) NULL)

  # Combine: canonical baseline available under "PL-BASELINE" key + all manifest entries
  policy_fns <- c(list("PL-BASELINE" = .baseline_fn), policy_fns)
  names(policy_fns) <- c("PL-BASELINE", names(strategy_overrides))

  message(sprintf(
    "cpt: policies loaded - %d total, products: %s",
    length(strategy_overrides),
    paste(unique(vapply(strategy_overrides, function(s) s$product %||% "Unknown", character(1))),
          collapse = ", ")
  ))
}

# NOTE: mockPolicies, base_loans, vintage_raw, and persistence are built
# in app.R AFTER services are sourced (govern_service.R defines the builders).

# Export policy_fns and strategy_overrides to .GlobalEnv
for (.nm in c("policy_fns", "strategy_overrides")) {
  if (exists(.nm, inherits = FALSE))
    assign(.nm, get(.nm), envir = .GlobalEnv)
}
rm(.nm)

# wi_rule_vars is defined below; we mirror it to .GlobalEnv after the
# definition (see end of file). Shiny's auto-source places top-level
# R/*.R files into a support env, so engine helpers and tab files
# (which look in .GlobalEnv) need an explicit export.

# ---- What-If rule variable metadata (migrated from tab_whatif_simulator.R) ----
# Defined in globals so engine helpers (e.g. translate_rule_var_labels in
# R/engine/segments.R) can resolve friendly labels without depending on tab
# files being sourced first. SMART Risk Scanner, Opportunity Explorer, and
# What-If Simulator all read from this single source of truth.
wi_rule_vars <- list(
  # --- Original variables ---
  list(id = "fico_score",                    label = "FICO Score",                    type = "numeric"),
  list(id = "annual_income",                 label = "Annual Income",                 type = "numeric"),
  list(id = "monthly_debt_payment",          label = "Monthly Debt Payment",          type = "numeric"),
  list(id = "debt_to_income_ratio",          label = "Debt-to-Income Ratio",          type = "numeric"),
  list(id = "credit_file_age_months",        label = "Credit File Age (Months)",      type = "numeric"),
  list(id = "delinquent_accounts_count",     label = "Delinquent Accounts",           type = "numeric"),
  list(id = "months_since_last_delinquency", label = "Months Since Last Delinquency", type = "numeric"),
  list(id = "internal_file_age_months",      label = "Internal File Age (Months)",    type = "numeric"),
  list(id = "term_months",                   label = "Loan Term (Months)",            type = "numeric"),
  list(id = "risk_model_score_1",            label = "Risk Score 1",                  type = "numeric"),
  list(id = "risk_model_score_2",            label = "Risk Score 2",                  type = "numeric"),
  list(id = "risk_model_score_3",            label = "Risk Score 3",                  type = "numeric"),
  list(id = "partner_channel",               label = "Partner Channel",               type = "categorical"),
  list(id = "risk_grade",                    label = "Credit Quality Grade",          type = "categorical"),
  list(id = "fico_band",                     label = "FICO Band",                     type = "categorical"),
  list(id = "borrower_repeat_type",          label = "Repeat Type",                   type = "categorical"),
  list(id = "pricing_test_variant",          label = "Pricing Variant",               type = "categorical"),
  # --- Bureau: Trade Counts ---
  list(id = "TR01",                          label = "Total Trades (TR01)",           type = "numeric"),
  list(id = "TR03",                          label = "Satisfactory Trades (TR03)",    type = "numeric"),
  list(id = "TR20",                          label = "Months Since Oldest Acct (TR20)", type = "numeric"),
  list(id = "RV01",                          label = "Revolving Accounts (RV01)",     type = "numeric"),
  list(id = "IL06",                          label = "Installment Trades 6M (IL06)",  type = "numeric"),
  list(id = "N_REVOL_TRADES",               label = "Revolving Trades",              type = "numeric"),
  list(id = "N_INST_TRADES",                label = "Installment Trades",            type = "numeric"),
  list(id = "N_OPEN_ACCTS",                 label = "Open Accounts",                 type = "numeric"),
  list(id = "N_CLOSED_ACCTS",               label = "Closed Accounts",               type = "numeric"),
  list(id = "AVG_ACCT_AGE",                 label = "Avg Account Age (Months)",      type = "numeric"),
  list(id = "MAX_ACCT_AGE",                 label = "Max Account Age (Months)",      type = "numeric"),
  list(id = "MIN_ACCT_AGE",                 label = "Min Account Age (Months)",      type = "numeric"),
  list(id = "ACCT_ACTIVITY_3M",             label = "Account Activity 3M",           type = "numeric"),
  # --- Bureau: Delinquency & Derogatories ---
  list(id = "DQ82",                          label = "Trades 30 DPD (DQ82)",          type = "numeric"),
  list(id = "DQ07",                          label = "Trades 60 DPD (DQ07)",          type = "numeric"),
  list(id = "N_30DPD_12M",                  label = "30 DPD Last 12M",               type = "numeric"),
  # list(id = "N_60DPD_12M",                  label = "60 DPD Last 12M",               type = "numeric"),
  list(id = "N_90DPD_12M",                  label = "90 DPD Last 12M",               type = "numeric"),
  # list(id = "N_CHRGOFF",                    label = "Chargeoffs",                    type = "numeric"),
  list(id = "N_COLL",                       label = "Collections",                   type = "numeric"),
  list(id = "TOT_COLL_BAL",                label = "Collection Balance",            type = "numeric"),
  list(id = "BK07S",                         label = "Months Since Bankruptcy (BK07S)", type = "numeric"),
  list(id = "CO09A",                         label = "Months Since Collection (CO09A)", type = "numeric"),
  list(id = "CF01",                          label = "Months Since Chargeoff (CF01)", type = "numeric"),
  list(id = "CC30",                          label = "Credit Counseling Accts (CC30)", type = "numeric"),
  # --- Bureau: Inquiries ---
  list(id = "INQ6S",                         label = "Inquiries 6M (INQ6S)",          type = "numeric"),
  list(id = "N_HARD_INQ_6M",               label = "Hard Inquiries 6M",             type = "numeric"),
  # list(id = "N_HARD_INQ_12M",              label = "Hard Inquiries 12M",            type = "numeric"),
  # --- Bureau: Utilization & Balance ---
  list(id = "BC_UTL",                        label = "Bankcard Utilization",          type = "numeric"),
  list(id = "BC29",                          label = "Bankcard Balance 3M (BC29)",    type = "numeric"),
  # list(id = "AGG901",                        label = "Aggregate Bankcard Bal (AGG901)", type = "numeric"),
  list(id = "REVOL_BAL",                     label = "Revolving Balance",             type = "numeric"),
  list(id = "REVOL_LMT",                     label = "Revolving Limit",               type = "numeric"),
  list(id = "REVOL_UTL",                     label = "Revolving Utilization",         type = "numeric"),
  list(id = "UTL_TREND_6M",                 label = "Utilization Trend 6M",          type = "numeric"),
  list(id = "BAL_TREND_6M",                 label = "Balance Trend 6M",              type = "numeric"),
  list(id = "PMT_RATIO",                    label = "Payment Ratio",                 type = "numeric"),
  # --- Risk Scores ---
  # list(id = "risk_bucket",                  label = "Risk Bucket",                   type = "numeric"),
  list(id = "pd_estimate",                   label = "Bureau Risk Index",             type = "numeric"),
  list(id = "lgd_estimate",                  label = "Trade Stability Score",         type = "numeric"),
  list(id = "ead_amount",                    label = "Exposure at Default",           type = "numeric"),
  list(id = "fraud_estimate",                label = "Fraud Estimate",                type = "numeric"),
  list(id = "behav_score",                   label = "Behavior Score",                type = "numeric"),
  list(id = "policy_hits",                  label = "Policy Hits",                   type = "numeric"),
  # --- Loan Financial ---
  # list(id = "monthly_emi",                   label = "Monthly EMI",                   type = "numeric"),
  list(id = "total_int_paid",                label = "Total Interest Paid",           type = "numeric"),
  # --- Product (categorical) ---
  list(id = "CRED_MIX",                      label = "Credit Mix",                    type = "categorical"),
  list(id = "promo_category",                 label = "Promo Category",                type = "categorical"),
  list(id = "offer_type",                     label = "Offer Type",                    type = "categorical")
)

# Now that wi_rule_vars is defined, mirror it to .GlobalEnv so engine helpers
# and other tab files (sourced into .GlobalEnv via app.R) can see it.
assign("wi_rule_vars", wi_rule_vars, envir = .GlobalEnv)
