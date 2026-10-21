# ============================================================
# R/services/simulate_service.R
#
# Policy simulation service - feeds What-If Lab. Thin wrapper
# around engine/policy_params.R primitives.
#
# Week 4 scope: run_scenario, validate_params_wrapper,
#               cascade_impact, save_as_variant (stub),
#               risk_appetite_check, filter_population.
# ============================================================

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# ---- filter_population ------------------------------------------------------
#' Apply UI filter predicates to credit_data before running a scenario.
#'
#' @param credit_data Full loan data frame
#' @param filters Named list with optional keys: product, fico_band, grade,
#'                partner, repeat_type, mob, date_from, date_to
#' @return filtered data.frame
filter_population <- function(credit_data, filters = list()) {
  df <- credit_data
  if (is.null(df) || nrow(df) == 0) return(df)

  if (!is.null(filters$product) && !identical(filters$product, "all") &&
      "product_type" %in% names(df)) {
    df <- df[df$product_type == filters$product, , drop = FALSE]
  }
  if (!is.null(filters$fico_band) && !"all" %in% filters$fico_band &&
      "fico_band" %in% names(df)) {
    df <- df[df$fico_band %in% filters$fico_band, , drop = FALSE]
  }
  if (!is.null(filters$grade) && !"all" %in% filters$grade &&
      "risk_grade" %in% names(df)) {
    df <- df[df$risk_grade %in% filters$grade, , drop = FALSE]
  }
  if (!is.null(filters$partner) && !"all" %in% filters$partner &&
      "partner_channel" %in% names(df)) {
    df <- df[df$partner_channel %in% filters$partner, , drop = FALSE]
  }
  if (!is.null(filters$repeat_type) && !"all" %in% filters$repeat_type &&
      "borrower_repeat_type" %in% names(df)) {
    df <- df[df$borrower_repeat_type %in% filters$repeat_type, , drop = FALSE]
  }
  if (!is.null(filters$mob) && !"all" %in% filters$mob &&
      "mob_bins" %in% names(df)) {
    df <- df[df$mob_bins %in% filters$mob, , drop = FALSE]
  }
  if (!is.null(filters$date_from) && "date" %in% names(df)) {
    df <- df[!is.na(df$date) & df$date >= as.Date(filters$date_from), , drop = FALSE]
  }
  if (!is.null(filters$date_to) && "date" %in% names(df)) {
    df <- df[!is.na(df$date) & df$date <= as.Date(filters$date_to), , drop = FALSE]
  }
  df
}

# ---- run_scenario -----------------------------------------------------------
#' Apply a parameter override set to credit_data and return the before/after
#' KPIs, funnels, and a changed-params summary.
#'
#' @param credit_data Full loan data frame
#' @param perf_tape Performance tape (for IRR lookups)
#' @param baseline_policy_id The policy_id used for the baseline (scoping)
#' @param param_overrides Named list of policy_manifest param overrides
#' @param filters Optional filter list for filter_population
#' @return list(baseline_kpis, scenario_kpis, delta, changed_params,
#'              filter_rows, elapsed_seconds)
run_scenario <- function(credit_data, perf_tape = NULL,
                         baseline_policy_id = "PL-CURRENT",
                         param_overrides = list(),
                         filters = list()) {
  t0 <- Sys.time()

  if (!exists("build_default_params", mode = "function") ||
      !exists("apply_policy_with_params", mode = "function") ||
      !exists("compute_kpis", mode = "function")) {
    stop("engine/policy_params.R functions not loaded")
  }

  # Resolve baseline params from the manifest (optionally merged with the
  # strategy_overrides entry for the selected baseline policy).
  default_params <- build_default_params(policy_manifest)
  baseline_params <- default_params
  if (exists("strategy_overrides") && baseline_policy_id %in% names(strategy_overrides)) {
    so <- strategy_overrides[[baseline_policy_id]]$params %||% list()
    baseline_params <- modifyList(baseline_params, so)
  }
  scenario_params <- modifyList(baseline_params, param_overrides %||% list())

  # Filter population
  df <- filter_population(credit_data, filters)
  if (nrow(df) == 0) {
    return(list(
      baseline_kpis = NULL, scenario_kpis = NULL, delta = NULL,
      changed_params = param_overrides, filter_rows = 0L,
      elapsed_seconds = 0
    ))
  }

  # Apply baseline policy
  baseline_df <- tryCatch(
    apply_policy_with_params(df, baseline_params),
    error = function(e) { message("cpt run_scenario baseline: ", conditionMessage(e)); NULL }
  )
  scenario_df <- tryCatch(
    apply_policy_with_params(df, scenario_params),
    error = function(e) { message("cpt run_scenario scenario: ", conditionMessage(e)); NULL }
  )

  if (is.null(baseline_df) || is.null(scenario_df)) {
    return(list(
      baseline_kpis = NULL, scenario_kpis = NULL, delta = NULL,
      changed_params = param_overrides, filter_rows = nrow(df),
      elapsed_seconds = as.numeric(Sys.time() - t0, units = "secs")
    ))
  }

  # Compute KPIs on both
  baseline_kpis <- compute_kpis(baseline_df)
  scenario_kpis <- compute_kpis(scenario_df)

  # Compute delta structure (scenario - baseline)
  delta <- list(
    approval_rate = .safe_sub(scenario_kpis$approval_rate, baseline_kpis$approval_rate),
    portfolio_irr = .safe_sub(scenario_kpis$portfolio_irr %||% scenario_kpis$irr,
                              baseline_kpis$portfolio_irr %||% baseline_kpis$irr),
    avg_loss_rate = .safe_sub(scenario_kpis$avg_loss_rate %||% scenario_kpis$bad_rate,
                              baseline_kpis$avg_loss_rate %||% baseline_kpis$bad_rate),
    n_loans       = .safe_sub(scenario_kpis$n_loans, baseline_kpis$n_loans)
  )

  list(
    baseline_kpis   = baseline_kpis,
    scenario_kpis   = scenario_kpis,
    delta           = delta,
    changed_params  = param_overrides,
    filter_rows     = nrow(df),
    elapsed_seconds = as.numeric(Sys.time() - t0, units = "secs")
  )
}

.safe_sub <- function(a, b) {
  if (is.null(a) || is.null(b) || is.na(a) || is.na(b)) return(NA_real_)
  as.numeric(a - b)
}

# ---- validate_params_wrapper ------------------------------------------------
#' Wrap engine::validate_params to normalize the return shape.
validate_params_wrapper <- function(params) {
  if (!exists("validate_params", mode = "function")) {
    return(list(ok = TRUE, errors = character(0), warnings = character(0)))
  }
  v <- tryCatch(validate_params(params, policy_manifest),
                error = function(e) {
                  list(ok = FALSE, errors = conditionMessage(e), warnings = character(0))
                })
  v
}

# ---- cascade_impact ---------------------------------------------------------
#' Thin wrapper for engine::compute_cascade_impact.
cascade_impact <- function(param_overrides) {
  if (!exists("compute_cascade_impact", mode = "function")) return(NULL)
  default_params <- build_default_params(policy_manifest)
  merged <- modifyList(default_params, param_overrides %||% list())
  tryCatch(compute_cascade_impact(merged, default_params, policy_manifest),
           error = function(e) {
             message("cpt cascade_impact: ", conditionMessage(e))
             NULL
           })
}

# ---- risk_appetite_check (Addition 6) ---------------------------------------
#' Hardcoded firm-wide risk appetite checks. Week 5.5 polish: make configurable.
#'
#' @param kpis A list with approval_rate, portfolio_irr (or irr), avg_loss_rate (or bad_rate)
#' @return list of list(name, limit_desc, actual, ok)
risk_appetite_check <- function(kpis) {
  if (is.null(kpis)) return(list())

  approval <- kpis$approval_rate %||% NA_real_
  irr      <- kpis$portfolio_irr %||% kpis$irr %||% NA_real_
  loss     <- kpis$avg_loss_rate %||% kpis$bad_rate %||% NA_real_

  list(
    list(name = "Loss rate ceiling", limit_desc = "<= 5.0%",
         actual = loss, ok = is.na(loss) || loss <= 0.05),
    list(name = "Approval rate floor", limit_desc = ">= 60.0%",
         actual = approval, ok = is.na(approval) || approval >= 0.60),
    list(name = "Portfolio IRR floor", limit_desc = ">= 12.0%",
         actual = irr, ok = is.na(irr) || irr >= 0.12)
  )
}

# ---- save_as_variant (Week 4 in-memory stub) --------------------------------
#' Append a new simulated variant to an in-memory reactive. Week 5 persists
#' to the strategy manifest file.
#'
#' @param name Variant display name
#' @param params Parameter override list
#' @param parent_policy Parent policy_id
#' @param product Product (default "Personal Loan")
#' @return a new policy_id string
save_as_variant <- function(name, params, parent_policy = "PL-CURRENT",
                            product = "Personal Loan") {
  # Generate a unique ID
  if (!exists(".cpt_variant_counter", envir = globalenv())) {
    assign(".cpt_variant_counter", 0L, envir = globalenv())
  }
  n <- get(".cpt_variant_counter", envir = globalenv()) + 1L
  assign(".cpt_variant_counter", n, envir = globalenv())

  pid <- sprintf("PL-SIM-USER-%02d", n)

  new_entry <- list(
    name           = name,
    product        = product,
    effective_from = NA,
    effective_to   = NA,
    status         = "simulated",
    parent_policy  = parent_policy,
    test_control   = NULL,
    data_backed    = FALSE,
    params         = params,
    simulation_result = list(
      bad_rate_delta   = 0,
      volume_delta_pct = 0,
      irr_delta_bps    = 0,
      verdict          = "User-saved scenario (Week 4 in-memory only)"
    )
  )

  # Append to the global strategy_overrides. This mutates the global.
  # Week 5 replaces this with real persistence.
  so <- get("strategy_overrides", envir = globalenv())
  so[[pid]] <- new_entry
  assign("strategy_overrides", so, envir = globalenv())

  pid
}
