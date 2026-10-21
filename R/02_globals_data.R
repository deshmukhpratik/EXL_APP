# ============================================================
# R/02_globals_data.R — Data Loading & Preparation
#
# Eager loaders that run once at app startup. Populate these
# globals that services and tabs read:
#   credit_data        — data.frame with all loan rows + derived cols
#   perf_tape_global   — list(rows, loan_view, vintages, ...) from engine
#   irr_cache_global   — optional pre-computed per-loan IRR table
#   policy_input_data  — clean input snapshot for policy re-runs
# ============================================================

# ---- load_perf_tape: build or load cached performance tape -----------------
load_perf_tape <- function() {
  existing <- get0("perf_tape_global", envir = .GlobalEnv)
  if (!is.null(existing)) return(existing)

  # Fast path: load pre-computed RDS cache (~2s vs ~90s from CSV)
  rds_cache <- "inst/data/perf_tape_cache.rds"
  csv_path  <- "inst/data/irr_performance_tape_FIXED_v2.csv"

  if (file.exists(rds_cache)) {
    message("Loading performance tape from cache...")
    result <- tryCatch(readRDS(rds_cache), error = function(e) NULL)
    if (!is.null(result)) {
      assign("perf_tape_global", result, envir = .GlobalEnv)
      return(result)
    }
    message("Cache read failed, falling back to CSV...")
  }

  message("Loading performance tape from CSV (this may take a minute)...")
  result <- tryCatch({
    .tape_raw <- read.csv(csv_path,
                          stringsAsFactors = FALSE, check.names = FALSE)
    # Zero out charge-offs at MOB 0 (disbursement row — no real default can occur)
    if ("co_amount" %in% names(.tape_raw) && "month_on_books" %in% names(.tape_raw)) {
      .tape_raw$co_amount[.tape_raw$month_on_books == 0] <- 0
    }
    tape <- build_perf_tape_v2(.tape_raw)
    # Save cache for fast startup next time
    tryCatch(saveRDS(tape, rds_cache), error = function(e)
      message("Could not save tape cache: ", e$message))
    tape
  }, error = function(e) {
    message("Performance tape not found or invalid: ", e$message)
    NULL
  })
  assign("perf_tape_global", result, envir = .GlobalEnv)
  result
}

# ---- load_irr_cache: pre-computed loan-level IRR cache --------------------
load_irr_cache <- function() {
  existing <- get0("irr_cache_global", envir = .GlobalEnv)
  if (!is.null(existing)) return(existing)
  rds_path <- "inst/data/loan_irr_cache.rds"
  if (file.exists(rds_path)) {
    message("Loading loan IRR cache...")
    result <- tryCatch(readRDS(rds_path), error = function(e) NULL)
    assign("irr_cache_global", result, envir = .GlobalEnv)
    return(result)
  }
  NULL
}

# ---- load_loan_data: CSV load + policy apply + derived columns ------------
load_loan_data <- function() {
  message("Loading dataset…")
  raw_data <- readr::read_csv("inst/data/loan_data.csv", show_col_types = FALSE)

  # If data already has policy_id column (v2 policy-aware data), use decisions as-is.
  # Otherwise, apply the baseline credit policy uniformly (legacy behavior).
  if ("policy_id" %in% names(raw_data) && "offered_decision_flag" %in% names(raw_data)) {
    message("Policy-aware data detected (policy_id column present) — using existing decisions.")
    credit_data <- as.data.frame(raw_data)
    # Still need to apply baseline grading for columns the app expects
    # (risk_grade, underwriting_risk_tier) if they aren't already present
    if (!"risk_grade" %in% names(credit_data)) {
      message("Applying baseline grading to derive risk_grade...")
      policy_out <- tryCatch(apply_credit_policy(raw_data), error = function(e) NULL)
      if (!is.null(policy_out)) {
        for (col in c("risk_grade", "underwriting_risk_tier")) {
          if (col %in% names(policy_out)) credit_data[[col]] <- policy_out[[col]]
        }
      }
    }
  } else {
    message("Applying baseline credit policy…")
    policy_out <- tryCatch(
      apply_credit_policy(raw_data),
      error = function(e) {
        message("Policy error: ", conditionMessage(e))
        NULL
      }
    )

    if (is.null(policy_out)) {
      credit_data <- raw_data
    } else {
      extra_cols  <- setdiff(names(raw_data), names(policy_out))
      credit_data <- bind_cols(policy_out, raw_data[, extra_cols, drop = FALSE])
    }
  }

  # ---- Post-policy amount reconciliation -----------------------
  issued_mask <- credit_data$issued_flag == 1

  appr_lt_issued <- issued_mask &
    !is.na(credit_data$maximum_approved_amount) &
    !is.na(credit_data$issued_amount) &
    credit_data$maximum_approved_amount < credit_data$issued_amount
  if (any(appr_lt_issued, na.rm = TRUE)) {
    n_fix <- sum(appr_lt_issued, na.rm = TRUE)
    message(sprintf("Reconciled %d loans: maximum_approved_amount raised to issued_amount.", n_fix))
    credit_data$maximum_approved_amount[appr_lt_issued] <-
      credit_data$issued_amount[appr_lt_issued]
  }

  off_lt_issued <- issued_mask &
    !is.na(credit_data$maximum_offered_amount) &
    !is.na(credit_data$issued_amount) &
    credit_data$maximum_offered_amount < credit_data$issued_amount
  if (any(off_lt_issued, na.rm = TRUE)) {
    n_fix <- sum(off_lt_issued, na.rm = TRUE)
    message(sprintf("Reconciled %d loans: maximum_offered_amount raised to issued_amount.", n_fix))
    credit_data$maximum_offered_amount[off_lt_issued] <-
      credit_data$issued_amount[off_lt_issued]
  }

  # ---- Derived columns ----------------------------------------
  credit_data <- credit_data %>%
    mutate(
      date = as.Date(booking_vintage, format = "%d/%m/%Y"),
      mob_bins = case_when(
        months_on_books <= 3  ~ "0-3",
        months_on_books <= 6  ~ "3-6",
        months_on_books <= 9  ~ "6-9",
        months_on_books <= 12 ~ "9-12",
        months_on_books >  12 ~ ">12",
        TRUE                  ~ "Unknown"
      ),
      fico_band = case_when(
        fico_score <  600 ~ "<600",
        fico_score <  650 ~ "600-649",
        fico_score <  700 ~ "650-699",
        fico_score <  750 ~ "700-749",
        fico_score >= 750 ~ "750+",
        TRUE              ~ "Unknown"
      )
    )

  # ---- Assign application dates to unfunded records ----------------
  missing_date_idx <- which(is.na(credit_data$date))
  if (length(missing_date_idx) > 0) {
    funded_dates  <- credit_data$date[!is.na(credit_data$date)]
    funded_months <- lubridate::floor_date(funded_dates, "month")
    month_counts  <- table(funded_months)
    month_probs   <- month_counts / sum(month_counts)

    set.seed(42)
    sampled_months <- sample(
      as.Date(names(month_probs)),
      size    = length(missing_date_idx),
      replace = TRUE,
      prob    = as.numeric(month_probs)
    )
    days_in_month <- lubridate::days_in_month(sampled_months)
    random_days   <- vapply(days_in_month, function(d) sample.int(d, 1), integer(1))
    credit_data$date[missing_date_idx] <- sampled_months + (random_days - 1L)

    message(sprintf("Assigned application dates to %d unfunded records.", length(missing_date_idx)))
  }

  # Add loan_id if the policy engine did not produce one
  if (!"loan_id" %in% names(credit_data))
    credit_data$loan_id <- seq_len(nrow(credit_data))

  # Round FICO to integers (source data may have decimal precision from generation)
  if ("fico_score" %in% names(credit_data)) credit_data$fico_score <- round(credit_data$fico_score)

  credit_data
}

# ============================================================
# Eager load at startup
# ============================================================
assign("perf_tape_global", NULL, envir = .GlobalEnv)
assign("irr_cache_global", NULL, envir = .GlobalEnv)

load_perf_tape()
load_irr_cache()
credit_data <- load_loan_data()

message(sprintf("Ready: %d records loaded (%d with original vintage, %d with synthetic date).",
                nrow(credit_data),
                sum(!is.na(credit_data$booking_vintage)),
                sum(is.na(credit_data$booking_vintage))))

# Date bounds for date range filter
vs_date_min <- min(credit_data$date, na.rm = TRUE)
vs_date_max <- max(credit_data$date, na.rm = TRUE)

# Pre-computed filter choices (used in pickerInput UI definitions)
filter_fico_bands  <- sort(unique(as.character(credit_data$fico_band[!is.na(credit_data$fico_band)])))
filter_partners    <- sort(unique(as.character(credit_data$partner_channel[!is.na(credit_data$partner_channel)])))
filter_repeat_types <- sort(unique(as.character(credit_data$borrower_repeat_type[!is.na(credit_data$borrower_repeat_type)])))
filter_mob_bins    <- sort(unique(as.character(credit_data$mob_bins[!is.na(credit_data$mob_bins)])))

# Clean input snapshot for policy re-runs.
# Two groups of columns:
#  (1) Engine inputs — read by apply_policy_with_params() to drive grading,
#      pricing, and decline cascades.
#  (2) Rule pass-throughs — bureau / risk attributes that the engine does NOT
#      consume, but which the What-If Simulator's manual decline/pricing rules
#      reference via evaluate_segment_conditions(). Without these, rules on
#      Bureau Risk Index, trade counts, utilization etc. silently match zero
#      rows because the engine output is missing the column.
#      Source of truth for pass-throughs: wi_rule_vars in 03_globals_policies.R.
.pid_cols <- c("loan_id",
  # (1) Engine inputs
  "fico_score", "annual_income", "monthly_rent", "monthly_debt_payment",
  "debt_to_income_ratio", "credit_file_age_months", "delinquent_accounts_count",
  "months_since_last_delinquency", "internal_file_age_months", "term_months",
  "partner_channel", "pricing_test_variant", "risk_model_score_1",
  "risk_model_score_2", "risk_model_score_3",
  "N_HARD_INQ_6M", "REVOL_UTL",
  # (2) Rule pass-throughs — bureau & risk attributes used by manual rules
  "risk_grade", "borrower_repeat_type",
  "TR01", "TR03", "TR20", "RV01", "IL06",
  "N_REVOL_TRADES", "N_INST_TRADES", "N_OPEN_ACCTS", "N_CLOSED_ACCTS",
  "AVG_ACCT_AGE", "MAX_ACCT_AGE", "MIN_ACCT_AGE", "ACCT_ACTIVITY_3M",
  "DQ82", "DQ07", "N_30DPD_12M", "N_90DPD_12M", "N_COLL", "TOT_COLL_BAL",
  "BK07S", "CO09A", "CF01", "CC30", "INQ6S",
  "BC_UTL", "BC29", "REVOL_BAL", "REVOL_LMT",
  "UTL_TREND_6M", "BAL_TREND_6M", "PMT_RATIO",
  "pd_estimate", "lgd_estimate", "fraud_estimate", "behav_score",
  "policy_hits")
.pid_cols <- intersect(.pid_cols, names(credit_data))
policy_input_data <- credit_data[, .pid_cols, drop = FALSE]

# ---- Join risk_grade into performance tape rows for IRR-by-grade ----
perf_tape_global <- get0("perf_tape_global", envir = .GlobalEnv)
if (!is.null(perf_tape_global) && "risk_grade" %in% names(credit_data)) {
  grade_map <- credit_data[, c("loan_id", "risk_grade"), drop = FALSE]
  grade_map$loan_id <- as.character(grade_map$loan_id)
  perf_tape_global$rows <- perf_tape_global$rows %>%
    left_join(grade_map, by = "loan_id")
  assign("perf_tape_global", perf_tape_global, envir = .GlobalEnv)
  message("Joined risk_grade into performance tape rows.")
}

# ---- Pre-compute first delinquency MOB per loan for MOB-based bad rates ----
if (!is.null(get0("perf_tape_global", envir = .GlobalEnv))) {
  .pt_rows <- get0("perf_tape_global", envir = .GlobalEnv)$rows
  .first_co <- .pt_rows %>%
    filter(co_amount > 0) %>%
    group_by(loan_id) %>%
    summarise(first_delinquency_mob = min(month_on_books), .groups = "drop")
  .flagged_no_co <- .pt_rows %>%
    filter(performance_delinquency_flag == 1) %>%
    distinct(loan_id) %>%
    anti_join(.first_co, by = "loan_id")
  if (nrow(.flagged_no_co) > 0) {
    .first_co <- bind_rows(.first_co,
      .flagged_no_co %>% mutate(first_delinquency_mob = 4L))
  }
  .first_co$first_delinquency_mob <- pmax(.first_co$first_delinquency_mob, 4L)
  .first_co$loan_id <- as.character(.first_co$loan_id)
  credit_data$loan_id <- as.character(credit_data$loan_id)
  if ("first_delinquency_mob" %in% names(credit_data)) {
    credit_data$first_delinquency_mob <- NULL
  }
  credit_data <- credit_data %>%
    left_join(.first_co, by = "loan_id")
  message(sprintf("Joined first_delinquency_mob: %d loans have delinquency timing.",
                  sum(!is.na(credit_data$first_delinquency_mob))))
  rm(.pt_rows, .first_co, .flagged_no_co)
}

# ── Load consultant assumptions config ──
.config_path <- "inst/data/consultant_assumptions.csv"
.cfg <- if (file.exists(.config_path)) {
  tryCatch(readr::read_csv(.config_path, show_col_types = FALSE),
           error = function(e) { message("WARNING: Failed to parse consultant_assumptions.csv: ", conditionMessage(e)); NULL })
} else NULL
if (!is.null(.cfg) && nrow(.cfg) > 0) {
  .donor_rows <- .cfg[.cfg$parameter == "donor_product", ]
  .donor_map <- if (nrow(.donor_rows) > 0) {
    setNames(as.character(.donor_rows$value), .donor_rows$grade)
  } else {
    character(0)
  }
  .stress_rows <- .cfg[.cfg$parameter == "donor_stress", ]
  .stress_map <- if (nrow(.stress_rows) > 0) {
    setNames(as.numeric(.stress_rows$value), .stress_rows$grade)
  } else {
    numeric(0)
  }

  consultant_config <<- list(
    elasticity = setNames(.cfg$value[.cfg$parameter == "elasticity"],
                          .cfg$grade[.cfg$parameter == "elasticity"]),
    adverse_selection = setNames(.cfg$value[.cfg$parameter == "adverse_selection"],
                                 .cfg$grade[.cfg$parameter == "adverse_selection"]),
    anchor_fico_floor = .cfg$value[.cfg$parameter == "anchor_fico_floor"][1],
    donor_product = .donor_map,
    donor_stress  = .stress_map
  )
  message("Loaded consultant_assumptions.csv: elasticity=",
          paste(consultant_config$elasticity, collapse="/"),
          ", anchor_fico=", consultant_config$anchor_fico_floor,
          ", donor_products=", paste(names(.donor_map), collapse="/"))
  rm(.cfg, .donor_rows, .donor_map, .stress_rows, .stress_map)
  assign("consultant_config", consultant_config, envir = .GlobalEnv)
} else {
  message("WARNING: consultant_assumptions.csv not found or invalid — using hardcoded defaults")
  consultant_config <<- list(
    elasticity = c(High = 3, Med = 5, Low = 8),
    adverse_selection = c(High = 2, Med = 5, Low = 8),
    anchor_fico_floor = 760,
    donor_product = character(0),
    donor_stress  = numeric(0)
  )
  assign("consultant_config", consultant_config, envir = .GlobalEnv)
}
rm(.config_path)

# ── Enrich thin-data products with donor curves ──
if (!is.null(get0("perf_tape_global", envir = .GlobalEnv)) && !is.null(consultant_config) &&
    length(consultant_config$donor_product) > 0) {
  assign("perf_tape_global",
         enhance_tape_curves_with_donors(get0("perf_tape_global", envir = .GlobalEnv),
                                          consultant_config),
         envir = .GlobalEnv)
}

# Build data-derived calibration cache (Tier 2 model, with disk caching).
# n_boot=20 keeps cold-start under ~1 minute; the result is cached to disk so
# subsequent launches are instant. For production-grade CIs run
# build_calibration_cache(credit_data, n_boot = 500L, refresh = TRUE) offline.
calibration_cache_global <- tryCatch({
  cache <- build_calibration_cache(credit_data, n_boot = 20L)
  message("Calibration cache ready: ",
          nrow(cache$adverse_selection), " adverse rows, ",
          nrow(cache$elasticity), " L1 elasticity rows")

  # Side-by-side log of the three elasticity models so a quant can audit
  # exactly which numbers the live policy logic is using.
  .fmt_grade_row <- function(df) {
    if (is.null(df) || !is.data.frame(df) || !nrow(df)) return("(unavailable)")
    paste(sprintf("%s=%.2f", df$grade, df$elasticity_pct_per_100bps), collapse = " / ")
  }
  message("Elasticity models (pp drop per 100bps, by grade):")
  message("  legacy variant-dummy : ", .fmt_grade_row(cache$obs_legacy))
  message("  tier1 continuous-APR : ", .fmt_grade_row(cache$tier1_elasticity))
  message("  tier2 rich-interact  : ", .fmt_grade_row(cache$tier2_elasticity))
  message("  ACTIVE source        : ",
          attr(cache, "active_obs_version") %||% "legacy_observational")
  cache
}, error = function(e) {
  message("Calibration cache build failed: ", e$message, " — using consultant defaults only")
  NULL
})
assign("calibration_cache_global", calibration_cache_global, envir = .GlobalEnv)

# ============================================================
# Analysis & Policy Variable Definitions
# (migrated from 02b_globals_utils.R)
# ============================================================

# Named vector of analysis variables
analysis_vars <- c(
  # --- Original policy & risk variables ---
  "FICO Score"                    = "fico_score",
  "Annual Income"                 = "annual_income",
  "Monthly Debt Payment"          = "monthly_debt_payment",
  "Debt-to-Income Ratio"          = "debt_to_income_ratio",
  "Credit File Age (Months)"      = "credit_file_age_months",
  "Delinquent Accounts"           = "delinquent_accounts_count",
  "Months Since Last Delinquency" = "months_since_last_delinquency",
  "Internal File Age (Months)"    = "internal_file_age_months",
  "Loan Term (Months)"            = "term_months",
  "Risk Score 1"                  = "risk_model_score_1",
  "Risk Score 2"                  = "risk_model_score_2",
  "Risk Score 3"                  = "risk_model_score_3",
  "Risk Score 4"                  = "risk_model_score_4",
  "Delinquency Signal Count"      = "delinquency_signal_count",
  "Monthly Rent"                  = "monthly_rent",
  # --- Bureau: Trade Counts ---
  "Total Trades (TR01)"           = "TR01",
  "Satisfactory Trades (TR03)"    = "TR03",
  "Months Since Oldest Acct (TR20)" = "TR20",
  "Revolving Accounts (RV01)"     = "RV01",
  "Installment Trades 6M (IL06)"  = "IL06",
  "Revolving Trades"              = "N_REVOL_TRADES",
  "Installment Trades"            = "N_INST_TRADES",
  "Open Accounts"                 = "N_OPEN_ACCTS",
  "Closed Accounts"               = "N_CLOSED_ACCTS",
  "Avg Account Age (Months)"      = "AVG_ACCT_AGE",
  "Max Account Age (Months)"      = "MAX_ACCT_AGE",
  "Min Account Age (Months)"      = "MIN_ACCT_AGE",
  "Account Activity 3M"           = "ACCT_ACTIVITY_3M",
  # --- Bureau: Delinquency & Derogatories ---
  "Trades 30 DPD (DQ82)"          = "DQ82",
  "Trades 60 DPD (DQ07)"          = "DQ07",
  "30 DPD Last 12M"               = "N_30DPD_12M",
  # "60 DPD Last 12M"             = "N_60DPD_12M",       # excluded: constraint violation in synthetic data
  "90 DPD Last 12M"               = "N_90DPD_12M",
  # "Chargeoffs"                  = "N_CHRGOFF",         # excluded: wrong correlation direction in synthetic data
  "Collections"                   = "N_COLL",
  "Collection Balance"            = "TOT_COLL_BAL",
  "Months Since Bankruptcy (BK07S)" = "BK07S",
  "Months Since Collection (CO09A)" = "CO09A",
  "Months Since Chargeoff (CF01)" = "CF01",
  "Credit Counseling Accts (CC30)" = "CC30",
  # --- Bureau: Inquiries ---
  "Inquiries 6M (INQ6S)"          = "INQ6S",
  "Hard Inquiries 6M"             = "N_HARD_INQ_6M",
  # "Hard Inquiries 12M"          = "N_HARD_INQ_12M",    # excluded: 12M < 6M constraint violation in synthetic data
  # --- Bureau: Utilization & Balance ---
  "Bankcard Utilization"          = "BC_UTL",
  "Bankcard Balance 3M (BC29)"    = "BC29",
  # "Aggregate Bankcard Bal (AGG901)" = "AGG901",        # excluded: aggregate < individual constraint violation
  "Revolving Balance"             = "REVOL_BAL",
  "Revolving Limit"               = "REVOL_LMT",
  "Revolving Utilization"         = "REVOL_UTL",
  "Utilization Trend 6M"          = "UTL_TREND_6M",
  "Balance Trend 6M"              = "BAL_TREND_6M",
  "Payment Ratio"                 = "PMT_RATIO",
  # --- Risk Scores ---
  # "Risk Bucket"                 = "risk_bucket",       # excluded: no correlation with delinquency in synthetic data
  "Bureau Risk Index"             = "pd_estimate",
  "Trade Stability Score"         = "lgd_estimate",
  "Exposure at Default"           = "ead_amount",
  "Fraud Estimate"                = "fraud_estimate",
  "Behavior Score"                = "behav_score",
  "Policy Hits"                   = "policy_hits",
  # --- Loan Financial ---
  # "Monthly EMI"                 = "monthly_emi",       # excluded: wrong APR correlation in synthetic data
  "Total Interest Paid"           = "total_int_paid"
)

# Variables actively used in baseline policy (L10-L90: grading, decline, pricing, affordability)
policy_vars <- c("fico_score", "monthly_debt_payment",
                 "debt_to_income_ratio", "credit_file_age_months",
                 "delinquent_accounts_count", "months_since_last_delinquency",
                 "internal_file_age_months", "term_months",
                 "risk_model_score_1", "risk_model_score_2",
                 "risk_model_score_3")

# Candidate variables — in analysis_vars but NOT in policy decline/grading
candidate_vars <- setdiff(analysis_vars, policy_vars)

# ---- Export all globals to .GlobalEnv ----
# Shiny auto-sources R/*.R into its own env. Tab/service files sourced from
# app.R into .GlobalEnv need to find these, so mirror them.
for (.nm in c("credit_data", "policy_input_data",
              "vs_date_min", "vs_date_max",
              "filter_fico_bands", "filter_partners",
              "filter_repeat_types", "filter_mob_bins",
              "analysis_vars", "policy_vars", "candidate_vars")) {
  if (exists(.nm, inherits = FALSE))
    assign(.nm, get(.nm), envir = .GlobalEnv)
}
rm(.nm)
