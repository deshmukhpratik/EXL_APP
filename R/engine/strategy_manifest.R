# ============================================================
# Strategy Manifest: Policy History Registry (v2)
#
# Each entry defines a policy regime with effective dates and
# parameter overrides that merge with build_default_params().
# At runtime: build_default_params() + modifyList(defaults, params)
#   -> apply_policy_with_params()
#
# Data-backed policies (data_backed=TRUE) have loans tagged with
# their policy_id in credit_data.
# Simulated policies (data_backed=FALSE) were never deployed —
# can be run on-demand.
#
# Policy narrative: 5 variables evolve across regimes, aligned to
# US macro 2021-2026:
#   n_hard_inq_6m_max, thin_file_months, apr_low,
#   exposure_cap_other, pti_low
# ============================================================

# v2 Policy History Registry
# Each entry defines a policy regime with effective dates and parameter overrides.
# Data-backed policies (data_backed=TRUE) have loans tagged with their policy_id in credit_data.
# Simulated policies (data_backed=FALSE) were never deployed — can be run on-demand.
#
# Policy narrative: 5 variables evolve across regimes (n_hard_inq_6m_max,
# thin_file_months, apr_low, exposure_cap_other, pti_low).

strategy_overrides <- list(

  # ===== DATA-BACKED: Personal Loan =====

  "PL-V1" = list(
    name = "PL Baseline — Growth Phase",
    product = "Personal Loan",
    effective_from = "2021-01-01", effective_to = "2021-12-31",
    status = "retired", parent_policy = NULL,
    test_control = NULL, data_backed = TRUE,
    params = list()
  ),

  "PL-V2" = list(
    name = "PL Thin File Relaxation",
    product = "Personal Loan",
    effective_from = "2022-01-01", effective_to = "2023-06-30",
    status = "retired", parent_policy = "PL-V1",
    test_control = NULL, data_backed = TRUE,
    params = list(thin_file_months = 18, exposure_cap_other = 45000)
  ),

  "PL-V3" = list(
    name = "PL Inquiry Rule + Tightening",
    product = "Personal Loan",
    effective_from = "2023-07-01", effective_to = "2024-06-30",
    status = "retired", parent_policy = "PL-V2",
    test_control = NULL, data_backed = TRUE,
    params = list(n_hard_inq_6m_max = 3, thin_file_months = 30, apr_low = 0.18,
                  exposure_cap_other = 40000)
  ),

  "PL-V4" = list(
    name = "PL Inquiry Relaxation — Control",
    product = "Personal Loan",
    effective_from = "2024-07-01", effective_to = "2024-12-31",
    status = "retired", parent_policy = "PL-V3",
    test_control = list(group = "control", assignment = "random_33pct",
                        target_segment = "n_hard_inq_6m == 3",
                        pricing_adjustment_bps = 0),
    data_backed = TRUE,
    params = list(n_hard_inq_6m_max = 4, thin_file_months = 30,
                  thin_file_reentry_min = 18, thin_file_reentry_max = 24,
                  thin_file_reentry_delinq_gate = 0, apr_low = 0.18)
  ),

  "PL-V4-TA" = list(
    name = "PL Inquiry Relaxation — Test A (+100bps)",
    product = "Personal Loan",
    effective_from = "2024-07-01", effective_to = "2024-12-31",
    status = "retired", parent_policy = "PL-V4",
    test_control = list(group = "test_a", assignment = "random_33pct",
                        target_segment = "n_hard_inq_6m == 3",
                        pricing_adjustment_bps = 100),
    data_backed = TRUE,
    params = list(n_hard_inq_6m_max = 4, thin_file_months = 30,
                  thin_file_reentry_min = 18, thin_file_reentry_max = 24,
                  thin_file_reentry_delinq_gate = 0, apr_low = 0.18)
  ),

  "PL-V4-TB" = list(
    name = "PL Inquiry Relaxation — Test B (+150bps)",
    product = "Personal Loan",
    effective_from = "2024-07-01", effective_to = "2024-12-31",
    status = "retired", parent_policy = "PL-V4",
    test_control = list(group = "test_b", assignment = "random_33pct",
                        target_segment = "n_hard_inq_6m == 3",
                        pricing_adjustment_bps = 150),
    data_backed = TRUE,
    params = list(n_hard_inq_6m_max = 4, thin_file_months = 30,
                  thin_file_reentry_min = 18, thin_file_reentry_max = 24,
                  thin_file_reentry_delinq_gate = 0, apr_low = 0.18)
  ),

  "PL-CURRENT" = list(
    name = "PL Prime Production Policy",
    product = "Personal Loan",
    effective_from = "2025-01-01", effective_to = NA,
    status = "active", parent_policy = "PL-V4-TB",
    test_control = NULL, data_backed = TRUE,
    params = list(n_hard_inq_6m_max = 4, thin_file_months = 30,
                  thin_file_reentry_min = 18, thin_file_reentry_max = 24,
                  thin_file_reentry_delinq_gate = 0, apr_low = 0.17,
                  pti_low = 0.17)
  ),

  # ===== DATA-BACKED: Top-Up Loan =====

  "TU-V1" = list(
    name = "Top-Up Baseline",
    product = "Top-Up",
    effective_from = "2026-01-01", effective_to = NA,
    status = "active", parent_policy = NULL,
    test_control = NULL, data_backed = TRUE,
    params = list(n_hard_inq_6m_max = 4, thin_file_months = 24,
                  thin_file_reentry_min = 18, thin_file_reentry_max = 24,
                  thin_file_reentry_delinq_gate = 0, apr_low = 0.17,
                  pti_low = 0.17, exposure_cap_high = 25000,
                  exposure_cap_other = 25000)
  ),

  # ===== SIMULATED: Personal Loan =====

  "PL-SIM-01" = list(
    name = "Aggressive Growth — Full Relaxation",
    product = "Personal Loan",
    effective_from = NA, effective_to = NA,
    status = "simulated", parent_policy = "PL-CURRENT",
    test_control = NULL, data_backed = FALSE,
    params = list(fico_min = 580, thin_file_months = 12, n_hard_inq_6m_max = 5),
    simulation_result = list(bad_rate_delta = -0.05, volume_delta_pct = 0.18,
                             irr_delta_bps = -120, verdict = "Board rejected — too aggressive")
  ),

  "PL-SIM-02" = list(
    name = "Premium Pricing — High Grade Reprice",
    product = "Personal Loan",
    effective_from = NA, effective_to = NA,
    status = "simulated", parent_policy = "PL-CURRENT",
    test_control = NULL, data_backed = FALSE,
    params = list(apr_high = 0.075, apr_med = 0.1225),
    simulation_result = list(bad_rate_delta = 0.001, volume_delta_pct = -0.08,
                             irr_delta_bps = 15, verdict = "Not worth it — 8% volume loss for 15bps")
  ),

  "PL-SIM-03" = list(
    name = "Zero Delinquency Tolerance",
    product = "Personal Loan",
    effective_from = NA, effective_to = NA,
    status = "simulated", parent_policy = "PL-CURRENT",
    test_control = NULL, data_backed = FALSE,
    params = list(delinquency_max = 0),
    simulation_result = list(bad_rate_delta = -0.02, volume_delta_pct = -0.06,
                             irr_delta_bps = 30, verdict = "Poor efficiency — cuts 6% for 2pp")
  ),

  "PL-SIM-04" = list(
    name = "Channel Restriction — Web Only",
    product = "Personal Loan",
    effective_from = NA, effective_to = NA,
    status = "simulated", parent_policy = "PL-CURRENT",
    test_control = NULL, data_backed = FALSE,
    params = list(),
    simulation_result = list(verdict = "Not yet simulated — channel restriction requires engine extension")
  ),

  "PL-SIM-05" = list(
    name = "Utilization-Based Decline (REVOL_UTL > 85%)",
    product = "Personal Loan",
    effective_from = NA, effective_to = NA,
    status = "simulated", parent_policy = "PL-CURRENT",
    test_control = NULL, data_backed = FALSE,
    params = list(revol_utl_max = 0.85),
    simulation_result = list(bad_rate_delta = -0.03, volume_delta_pct = -0.04,
                             irr_delta_bps = 45, verdict = "Under review for Q4 2026")
  ),

  "PL-SIM-06" = list(
    name = "Affordability Tightening — Low PTI 12%",
    product = "Personal Loan",
    effective_from = NA, effective_to = NA,
    status = "simulated", parent_policy = "PL-CURRENT",
    test_control = NULL, data_backed = FALSE,
    params = list(pti_low = 0.12),
    simulation_result = list(bad_rate_delta = -0.01, volume_delta_pct = -0.04,
                             irr_delta_bps = 20, verdict = "Parked for rate-sensitive environment")
  ),

  # ===== SIMULATED: Top-Up =====

  "TU-SIM-01" = list(
    name = "Top-Up Full Expansion",
    product = "Top-Up",
    effective_from = NA, effective_to = NA,
    status = "simulated", parent_policy = "TU-V1",
    test_control = NULL, data_backed = FALSE,
    params = list(fico_min = 600),
    simulation_result = list(bad_rate_delta = -0.04, volume_delta_pct = 0.25,
                             irr_delta_bps = -80, verdict = "Too risky at 4 months — revisit at 12 MOBs")
  ),

  "TU-SIM-02" = list(
    name = "Top-Up PL Donor Stress Test (1.30x)",
    product = "Top-Up",
    effective_from = NA, effective_to = NA,
    status = "simulated", parent_policy = "TU-V1",
    test_control = NULL, data_backed = FALSE,
    params = list(),
    simulation_result = list(verdict = "Not yet simulated — donor stress test requires engine extension")
  )
)

# ============================================================
# Look up the active policy_id for a given product name.
# Scans strategy_overrides for status == "active" + matching product.
# Consumers: Scanner, What-If, Opportunity Explorer pop filters.
# ============================================================
get_active_policy_id <- function(product_name) {
  so <- get0("strategy_overrides", envir = .GlobalEnv, inherits = FALSE)
  if (is.null(so)) return("PL-CURRENT")
  for (pid in names(so)) {
    s <- so[[pid]]
    if (identical(s$product, product_name) && identical(s$status, "active"))
      return(pid)
  }
  "PL-CURRENT"
}

# ============================================================
# Build a full policy function from a strategy override.
# Returns a function(df) that applies baseline + overrides.
# ============================================================
build_strategy_fn <- function(override_params) {
  force(override_params)
  function(df) {
    base_params   <- build_default_params(policy_manifest)
    merged_params <- modifyList(base_params, override_params)
    apply_policy_with_params(df, merged_params)
  }
}
