# Engine: Calibration
# Data-derived adverse selection and elasticity, replacing fixed consultant assumptions.
#
# Fallback chain: consultant_config override -> data-derived cache -> hardcoded defaults
#
# Functions:
#   build_adverse_selection_cache(credit_data)  — empirical bad-rate lift by grade x attribute
#   build_elasticity_cache(credit_data)         — empirical take-rate sensitivity from test/control
#   get_calibrated_assumption(param_name, grade, calibration_cache, consultant_config)
#   build_calibration_cache(credit_data)        — convenience: builds both caches in one call

# ── Null-coalescing operator (defined locally so this file is self-contained
# at app startup, before later modules that also define %||% are sourced).
if (!exists("%||%", envir = .GlobalEnv, inherits = FALSE)) {
  `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b
}

# ── Hardcoded defaults (last-resort fallback, same as consultant_assumptions.csv) ──
.CALIBRATION_DEFAULTS <- list(

  adverse_selection = c(High = 2, Med = 5, Low = 8),   # bad-rate lift % per 100bps price increase
  elasticity        = c(High = 3, Med = 5, Low = 8)    # take-rate drop % per 100bps price increase
)

# ── Adverse Selection: attribute definitions ──
# Each entry defines a decline-relevant attribute, the "disputed zone" (loans near the
# decline boundary that could go either way under different policy thresholds), and
# the "safe zone" (clearly acceptable loans) for comparison.

.ADVERSE_SELECTION_ATTRIBUTES <- list(
  hard_inq = list(
    column      = "N_HARD_INQ_6M",
    # Disputed zone: loans with 3 hard inquiries (boundary under V3 rule: decline >= 4)
    # Safe zone: loans with 0-2 inquiries (clearly safe under all policy regimes)
    disputed_fn = function(x) x >= 3 & x <= 4,
    safe_fn     = function(x) x <= 2,
    label       = "Hard Inquiries (3-4 vs 0-2)"
  ),
  thin_file = list(
    column      = "credit_file_age_months",
    # Disputed zone: 18-30 months (near the thin-file boundary of 18-24 months)
    # Safe zone: > 36 months (clearly seasoned files)
    disputed_fn = function(x) x >= 18 & x <= 30,
    safe_fn     = function(x) x > 36,
    label       = "Thin File (18-30mo vs >36mo)"
  ),
  delinquency = list(
    column      = "delinquent_accounts_count",
    # Disputed zone: exactly 1 delinquent account (boundary case)
    # Safe zone: 0 delinquent accounts
    disputed_fn = function(x) x == 1,
    safe_fn     = function(x) x == 0,
    label       = "Delinquency (1 acct vs 0)"
  ),
  dti = list(
    column      = "debt_to_income_ratio",
    # Disputed zone: DTI 0.35-0.45 (near the 0.40 decline threshold)
    # Safe zone: DTI < 0.30 (well within acceptable range)
    disputed_fn = function(x) x >= 0.35 & x <= 0.45,
    safe_fn     = function(x) x < 0.30,
    label       = "DTI (0.35-0.45 vs <0.30)"
  ),
  fico = list(
    column      = "fico_score",
    # Disputed zone: FICO 640-680 (near typical decline thresholds of 640-660)
    # Safe zone: FICO > 720 (clearly strong credit)
    disputed_fn = function(x) x >= 640 & x <= 680,
    safe_fn     = function(x) x > 720,
    label       = "FICO (640-680 vs >720)"
  )
)

# ── Minimum sample sizes ──
.MIN_SAMPLE_ADVERSE <- 30L    # minimum funded loans per zone per grade
.MIN_SAMPLE_ELASTICITY <- 50L # minimum approved loans per test group per grade


# ══════════════════════════════════════════════════════════════════════════════
# build_adverse_selection_cache
# ══════════════════════════════════════════════════════════════════════════════
#
# For each grade x decline-relevant attribute, compute the empirical adverse
# selection percentage: how much worse (in % terms) is the bad rate in the
# disputed zone compared to the safe zone.
#
# Returns: data.frame with columns:
#   attribute, grade, disputed_bad_rate, safe_bad_rate, adverse_pct,
#   n_disputed, n_safe, reliable
#
build_adverse_selection_cache <- function(credit_data) {

  required_cols <- c("issued_flag", "risk_grade", "performance_delinquency_flag")
  missing <- setdiff(required_cols, names(credit_data))
  if (length(missing) > 0) {
    message("calibration: adverse selection cache skipped — missing columns: ",
            paste(missing, collapse = ", "))
    return(NULL)
  }

  # Only funded loans have observable performance

  funded <- credit_data[credit_data$issued_flag == 1, ]
  if (nrow(funded) == 0) {
    message("calibration: no funded loans, cannot build adverse selection cache")
    return(NULL)
  }

  grades <- c("High", "Med", "Low")
  results <- list()


  for (attr_name in names(.ADVERSE_SELECTION_ATTRIBUTES)) {
    attr_def <- .ADVERSE_SELECTION_ATTRIBUTES[[attr_name]]
    col <- attr_def$column

    if (!col %in% names(funded)) {
      message("calibration: skipping attribute '", attr_name,
              "' — column '", col, "' not found")
      next
    }

    col_vals <- funded[[col]]
    bad_flag <- funded$performance_delinquency_flag

    for (grade in grades) {
      grade_mask <- funded$risk_grade == grade
      if (sum(grade_mask) == 0) next

      g_vals <- col_vals[grade_mask]
      g_bad  <- bad_flag[grade_mask]

      # Apply zone filters (handle NAs)
      disputed_mask <- !is.na(g_vals) & attr_def$disputed_fn(g_vals)
      safe_mask     <- !is.na(g_vals) & attr_def$safe_fn(g_vals)

      n_disputed <- sum(disputed_mask)
      n_safe     <- sum(safe_mask)

      # Compute bad rates
      disputed_bad_rate <- if (n_disputed > 0) mean(g_bad[disputed_mask], na.rm = TRUE) else NA_real_
      safe_bad_rate     <- if (n_safe > 0) mean(g_bad[safe_mask], na.rm = TRUE) else NA_real_

      # Adverse selection % = absolute bad rate difference between disputed
      # and safe zones, expressed as percentage points.
      # This matches the unit of consultant_config$adverse_selection (2-8)
      # and the action code's formula: adv_pct / 100 * (bps / 100) * n_good.
      adverse_pct <- if (!is.na(disputed_bad_rate) && !is.na(safe_bad_rate)) {
        (disputed_bad_rate - safe_bad_rate) * 100
      } else {
        NA_real_
      }

      reliable <- n_disputed >= .MIN_SAMPLE_ADVERSE && n_safe >= .MIN_SAMPLE_ADVERSE

      results[[length(results) + 1]] <- data.frame(
        attribute         = attr_name,
        attribute_label   = attr_def$label,
        grade             = grade,
        disputed_bad_rate = round(disputed_bad_rate, 6),
        safe_bad_rate     = round(safe_bad_rate, 6),
        adverse_pct       = round(adverse_pct, 2),
        n_disputed        = n_disputed,
        n_safe            = n_safe,
        reliable          = reliable,
        stringsAsFactors  = FALSE
      )
    }
  }

  if (length(results) == 0) {
    message("calibration: no adverse selection results computed")
    return(NULL)
  }

  cache <- do.call(rbind, results)
  rownames(cache) <- NULL

  # Summarise: grade-level adverse selection % (average across reliable attributes)
  grade_summary <- aggregate(
    adverse_pct ~ grade,
    data = cache[cache$reliable & !is.na(cache$adverse_pct), ],
    FUN = function(x) round(mean(x, na.rm = TRUE), 2)
  )
  names(grade_summary)[2] <- "mean_adverse_pct"

  message("calibration: adverse selection cache built — ",
          nrow(cache), " attribute x grade cells, ",
          sum(cache$reliable), " reliable")

  attr(cache, "grade_summary") <- grade_summary
  cache
}


# ══════════════════════════════════════════════════════════════════════════════
# build_elasticity_cache
# ══════════════════════════════════════════════════════════════════════════════
#
# Use test/control pricing data from V4 period to estimate price elasticity.
# PL-V4 = control, PL-V4-TA = +100bps, PL-V4-TB = +150bps
#
# Among approved loans, compare take-up (issued_flag) rates between groups
# at the same grade to compute elasticity per 100bps.
#
# Returns: data.frame with columns:
#   grade, control_take_rate, test_take_rate, bps_difference, n_control, n_test,
#   elasticity_pct_per_100bps, source, reliable, sign_expected
#
build_elasticity_cache <- function(credit_data) {

  required_cols <- c("policy_id", "issued_flag", "risk_grade", "offered_decision_flag")
  missing <- setdiff(required_cols, names(credit_data))
  if (length(missing) > 0) {
    message("calibration: elasticity cache skipped — missing columns: ",
            paste(missing, collapse = ", "))
    return(NULL)
  }

  # Filter to V4 test period
  control_id <- "PL-V4"
  test_configs <- list(
    "PL-V4-TA" = 100,   # +100bps
    "PL-V4-TB" = 150    # +150bps
  )

  v4_ids <- c(control_id, names(test_configs))
  v4 <- credit_data[credit_data$policy_id %in% v4_ids, ]

  if (nrow(v4) == 0) {
    message("calibration: no V4 test/control data found, cannot build elasticity cache")
    return(NULL)
  }

  # Among approved loans only
  v4_approved <- v4[v4$offered_decision_flag == "APPR", ]
  if (nrow(v4_approved) == 0) {
    message("calibration: no approved V4 loans, cannot build elasticity cache")
    return(NULL)
  }

  grades <- c("High", "Med", "Low")
  results <- list()

  for (test_id in names(test_configs)) {
    bps_diff <- test_configs[[test_id]]

    for (grade in grades) {
      control_loans <- v4_approved[v4_approved$policy_id == control_id &
                                     v4_approved$risk_grade == grade, ]
      test_loans    <- v4_approved[v4_approved$policy_id == test_id &
                                     v4_approved$risk_grade == grade, ]

      n_control <- nrow(control_loans)
      n_test    <- nrow(test_loans)

      control_take_rate <- if (n_control > 0) mean(control_loans$issued_flag) else NA_real_
      test_take_rate    <- if (n_test > 0) mean(test_loans$issued_flag) else NA_real_

      # Elasticity = (control_take - test_take) / (bps_difference / 100)
      # Positive value means higher price reduces take-up (expected direction)
      elast <- if (!is.na(control_take_rate) && !is.na(test_take_rate) && bps_diff > 0) {
        ((control_take_rate - test_take_rate) / (bps_diff / 100)) * 100
      } else {
        NA_real_
      }

      reliable <- n_control >= .MIN_SAMPLE_ELASTICITY & n_test >= .MIN_SAMPLE_ELASTICITY
      # Sign check: higher price should lead to lower take-up (positive elasticity)
      sign_expected <- if (!is.na(elast)) elast >= 0 else NA

      # Look up target_segment attribute from strategy manifest
      ts_attr <- tryCatch({
        ts_expr <- strategy_overrides[[test_id]]$test_control$target_segment
        if (is.null(ts_expr)) NA_character_ else {
          # Extract the variable name from expressions like "n_hard_inq_6m == 3"
          m <- regmatches(ts_expr, regexpr("^[a-zA-Z_][a-zA-Z0-9_]*", ts_expr))
          if (length(m) == 0) NA_character_ else toupper(m)
        }
      }, error = function(e) NA_character_)

      results[[length(results) + 1]] <- data.frame(
        grade                     = grade,
        test_group                = test_id,
        attribute                 = ts_attr,
        bps_difference            = bps_diff,
        control_take_rate         = round(control_take_rate, 6),
        test_take_rate            = round(test_take_rate, 6),
        elasticity_pct_per_100bps = round(elast, 2),
        n_control                 = n_control,
        n_test                    = n_test,
        source                    = "test_control",
        reliable                  = reliable,
        sign_expected             = sign_expected,
        stringsAsFactors          = FALSE
      )
    }
  }

  if (length(results) == 0) {
    message("calibration: no elasticity results computed")
    return(NULL)
  }

  cache <- do.call(rbind, results)
  rownames(cache) <- NULL

  # Summarise: grade-level elasticity (average across reliable test groups with expected sign)
  usable <- cache[cache$reliable & cache$sign_expected & !is.na(cache$elasticity_pct_per_100bps), ]
  if (nrow(usable) > 0) {
    grade_summary <- aggregate(
      elasticity_pct_per_100bps ~ grade,
      data = usable,
      FUN = function(x) round(mean(x, na.rm = TRUE), 2)
    )
    names(grade_summary)[2] <- "mean_elasticity_pct"
  } else {
    # If no usable results (wrong sign or unreliable), use all reliable results
    # and flag in the summary
    usable_any <- cache[cache$reliable & !is.na(cache$elasticity_pct_per_100bps), ]
    if (nrow(usable_any) > 0) {
      grade_summary <- aggregate(
        elasticity_pct_per_100bps ~ grade,
        data = usable_any,
        FUN = function(x) round(mean(abs(x), na.rm = TRUE), 2)
      )
      names(grade_summary)[2] <- "mean_elasticity_pct"
      attr(grade_summary, "sign_warning") <- TRUE
    } else {
      grade_summary <- data.frame(grade = grades, mean_elasticity_pct = NA_real_)
    }
  }

  n_reliable <- sum(cache$reliable)
  n_expected_sign <- sum(cache$reliable & cache$sign_expected, na.rm = TRUE)
  message("calibration: elasticity cache built — ",
          nrow(cache), " test x grade cells, ",
          n_reliable, " reliable, ",
          n_expected_sign, " with expected sign")

  if (n_expected_sign == 0 && n_reliable > 0) {
    message("calibration WARNING: all test groups show inverted elasticity ",
            "(higher price -> higher take-up). ",
            "Data may not reflect true price sensitivity. ",
            "Falling back to consultant/default values recommended.")
  }

  attr(cache, "grade_summary") <- grade_summary
  attr(cache, "version") <- "1.1"
  cache
}


# ══════════════════════════════════════════════════════════════════════════════
# build_observational_elasticity_cache
# ══════════════════════════════════════════════════════════════════════════════
#
# Uses pricing_test_variant quasi-experiment (Standard / Variant_A / Variant_B)
# across ALL policy regimes to estimate price elasticity by grade.
#
# Method: Logistic regression with pricing_test_variant dummies × risk_grade,
# policy_id fixed effects, and FICO control. Elasticity is identified from
# exogenous variant assignment only — no endogenous APR variation.
#
# Returns: data.frame with columns:
#   grade, elasticity_pct_per_100bps, n, se, reliable, method
# with attr "grade_lookup" — named numeric vector for fast grade-level retrieval.
#
build_observational_elasticity_cache <- function(credit_data) {

  required_cols <- c("offered_decision_flag", "issued_flag", "risk_grade",
                     "pricing_test_variant", "policy_id", "fico_score",
                     "interest_rate_apr")
  missing <- setdiff(required_cols, names(credit_data))
  if (length(missing) > 0) {
    message("calibration: obs elasticity skipped — missing: ", paste(missing, collapse = ", "))
    return(NULL)
  }

  approved <- credit_data[credit_data$offered_decision_flag == "APPR", ]
  if (nrow(approved) == 0) {
    message("calibration: no approved records for obs elasticity")
    return(NULL)
  }

  variants <- unique(approved$pricing_test_variant)
  if (!all(c("Standard", "Variant_A", "Variant_B") %in% variants)) {
    message("calibration: obs elasticity requires Standard/Variant_A/Variant_B — found: ",
            paste(variants, collapse = ", "))
    return(NULL)
  }

  # Fit variant-dummy model (clean exogenous identification)
  model <- tryCatch(
    glm(issued_flag ~ pricing_test_variant * risk_grade + policy_id + fico_score,
        data = approved, family = binomial(link = "logit")),
    error = function(e) {
      message("calibration: obs elasticity model failed: ", e$message)
      NULL
    }
  )
  if (is.null(model)) return(NULL)

  grades <- c("High", "Med", "Low")
  results <- list()

  for (grade in grades) {
    gd <- approved[approved$risk_grade == grade, ]
    n_grade <- nrow(gd)
    if (n_grade < 200) next

    # Predict takeup at each variant level
    gd_std <- gd; gd_std$pricing_test_variant <- "Standard"
    gd_va  <- gd; gd_va$pricing_test_variant  <- "Variant_A"
    gd_vb  <- gd; gd_vb$pricing_test_variant  <- "Variant_B"

    p_std <- mean(predict(model, newdata = gd_std, type = "response"))
    p_va  <- mean(predict(model, newdata = gd_va,  type = "response"))
    p_vb  <- mean(predict(model, newdata = gd_vb,  type = "response"))

    # Known APR spreads between variants (from the data)
    std_apr <- mean(gd$interest_rate_apr[gd$pricing_test_variant == "Standard"], na.rm = TRUE)
    va_apr  <- mean(gd$interest_rate_apr[gd$pricing_test_variant == "Variant_A"], na.rm = TRUE)
    vb_apr  <- mean(gd$interest_rate_apr[gd$pricing_test_variant == "Variant_B"], na.rm = TRUE)

    bps_a <- (va_apr - std_apr) * 10000
    bps_b <- (vb_apr - std_apr) * 10000

    # Elasticity in L1 units: ((std_take - test_take) / (bps/100)) * 100
    elast_va <- if (bps_a > 10) ((p_std - p_va) / (bps_a / 100)) * 100 else NA_real_
    elast_vb <- if (bps_b > 10) ((p_std - p_vb) / (bps_b / 100)) * 100 else NA_real_

    elast_avg <- mean(c(elast_va, elast_vb), na.rm = TRUE)
    if (is.nan(elast_avg) || is.na(elast_avg)) next

    # Sign check
    sign_ok <- elast_avg > 0

    # Sample sizes per variant
    n_std <- sum(gd$pricing_test_variant == "Standard")
    n_va  <- sum(gd$pricing_test_variant == "Variant_A")
    n_vb  <- sum(gd$pricing_test_variant == "Variant_B")
    reliable <- n_std >= 100 && n_va >= 100 && n_vb >= 100 && sign_ok

    results[[length(results) + 1]] <- data.frame(
      grade                     = grade,
      elasticity_pct_per_100bps = round(elast_avg, 2),
      n                         = n_grade,
      n_std                     = n_std,
      n_va                      = n_va,
      n_vb                      = n_vb,
      p_std                     = round(p_std, 4),
      p_va                      = round(p_va, 4),
      p_vb                      = round(p_vb, 4),
      bps_spread_a              = round(bps_a),
      bps_spread_b              = round(bps_b),
      sign_expected             = sign_ok,
      reliable                  = reliable,
      method                    = "variant_dummy_logistic",
      stringsAsFactors          = FALSE
    )
  }

  if (length(results) == 0) {
    message("calibration: obs elasticity — no grade-level results")
    return(NULL)
  }

  cache <- do.call(rbind, results)
  rownames(cache) <- NULL

  # Grade lookup vector for fast retrieval
  grade_lookup <- setNames(cache$elasticity_pct_per_100bps, cache$grade)
  attr(cache, "grade_lookup") <- grade_lookup

  n_reliable <- sum(cache$reliable)
  message("calibration: obs elasticity cache built — ",
          nrow(cache), " grades, ", n_reliable, " reliable: ",
          paste(sprintf("%s=%.2f", cache$grade, cache$elasticity_pct_per_100bps),
                collapse = ", "))
  cache
}


# ══════════════════════════════════════════════════════════════════════════════
# build_continuous_elasticity_cache  (Tier-1 upgrade)
# ══════════════════════════════════════════════════════════════════════════════
#
# Improvements vs build_observational_elasticity_cache:
#   1. APR enters CONTINUOUSLY (not as 3-way Standard/A/B categorical).
#      Elasticity = marginal effect of a +100bps APR shift, read off the
#      fitted curve for any APR — not a post-hoc take-rate gap.
#   2. FICO enters as a natural cubic spline (default 4 df) — captures the
#      hump/U shape that a single linear coefficient cannot.
#   3. Out-of-sample validation: 80/20 split (by application_date if
#      available, else random). Reports elasticity on TRAIN and TEST so
#      large divergence flags overfit.
#   4. Bootstrap 95% CIs on per-grade elasticity (default 500 reps) —
#      lets a pricing committee reason about uncertainty.
#
# Returns: data.frame with columns:
#   grade, elasticity_pct_per_100bps, ci_lower, ci_upper,
#   elasticity_train, elasticity_test, n, reliable, method
# with attr "grade_lookup" — named numeric vector for fast retrieval.
#
# This function is NOT wired into the app startup by default. To enable,
# add it to build_calibration_cache() or call directly:
#   cache <- build_continuous_elasticity_cache(credit_data)
#
build_continuous_elasticity_cache <- function(credit_data,
                                              n_boot       = 500L,
                                              holdout_frac = 0.2,
                                              fico_df      = 4L,
                                              seed         = 42L) {

  if (!requireNamespace("splines", quietly = TRUE)) {
    message("calibration: continuous elasticity needs the 'splines' package")
    return(NULL)
  }

  required_cols <- c("offered_decision_flag", "issued_flag", "risk_grade",
                     "interest_rate_apr", "fico_score", "policy_id")
  missing <- setdiff(required_cols, names(credit_data))
  if (length(missing) > 0) {
    message("calibration: continuous elasticity skipped — missing: ",
            paste(missing, collapse = ", "))
    return(NULL)
  }
  has_date <- "application_date" %in% names(credit_data)

  # ── Filter to approved loans with complete inputs ──────────────────────
  approved <- credit_data[
    credit_data$offered_decision_flag == "APPR" &
    !is.na(credit_data$interest_rate_apr) &
    !is.na(credit_data$fico_score) &
    !is.na(credit_data$issued_flag) &
    !is.na(credit_data$risk_grade), ]

  if (nrow(approved) < 1000) {
    message("calibration: continuous elasticity needs >=1000 approved rows, got ",
            nrow(approved))
    return(NULL)
  }

  # ── Train / test split ─────────────────────────────────────────────────
  set.seed(seed)
  if (has_date) {
    approved <- approved[order(approved$application_date), ]
    n_train  <- floor(nrow(approved) * (1 - holdout_frac))
    train    <- approved[seq_len(n_train), ]
    test     <- approved[seq.int(n_train + 1L, nrow(approved)), ]
    split_kind <- "time"
  } else {
    train_idx <- sample.int(nrow(approved),
                            size = floor(nrow(approved) * (1 - holdout_frac)))
    train     <- approved[train_idx, ]
    test      <- approved[-train_idx, ]
    split_kind <- "random"
  }

  # ── Helper: fit one model and return per-grade elasticity (pp/100bps) ──
  # Marginal effect approach: predict take-up at observed APR and at
  # APR + 100bps for the same population, take the difference. This works
  # uniformly whether the APR effect is linear or nonlinear.
  .fit_extract <- function(df) {
    f <- issued_flag ~ interest_rate_apr * risk_grade +
                       splines::ns(fico_score, df = fico_df) +
                       policy_id
    m <- tryCatch(
      suppressWarnings(glm(f, data = df, family = binomial(link = "logit"))),
      error = function(e) NULL
    )
    if (is.null(m)) return(NULL)

    grades <- c("High", "Med", "Low")
    out <- setNames(rep(NA_real_, length(grades)), grades)
    for (g in grades) {
      gd <- df[df$risk_grade == g, ]
      if (nrow(gd) < 100) next
      gd_hi <- gd; gd_hi$interest_rate_apr <- gd$interest_rate_apr + 0.01
      p_lo <- mean(predict(m, newdata = gd,    type = "response"))
      p_hi <- mean(predict(m, newdata = gd_hi, type = "response"))
      # Take-rate drop per 100 bps, expressed in percentage points
      out[g] <- (p_lo - p_hi) * 100
    }
    out
  }

  # ── Point estimates: train and test ────────────────────────────────────
  elast_train <- .fit_extract(train)
  elast_test  <- .fit_extract(test)
  if (is.null(elast_train)) {
    message("calibration: continuous elasticity model failed on train split")
    return(NULL)
  }
  if (is.null(elast_test)) elast_test <- setNames(rep(NA_real_, 3),
                                                  c("High", "Med", "Low"))

  # ── Bootstrap CIs on the train set ─────────────────────────────────────
  message("calibration: bootstrapping continuous elasticity (",
          n_boot, " reps)...")
  boot_mat <- matrix(NA_real_, nrow = n_boot, ncol = 3,
                     dimnames = list(NULL, c("High", "Med", "Low")))
  for (b in seq_len(n_boot)) {
    idx <- sample.int(nrow(train), replace = TRUE)
    bt  <- .fit_extract(train[idx, ])
    if (!is.null(bt)) boot_mat[b, ] <- bt[c("High", "Med", "Low")]
  }
  ci_lower <- apply(boot_mat, 2, quantile, probs = 0.025, na.rm = TRUE)
  ci_upper <- apply(boot_mat, 2, quantile, probs = 0.975, na.rm = TRUE)

  # ── Assemble result ────────────────────────────────────────────────────
  grades <- c("High", "Med", "Low")
  results <- data.frame(
    grade                     = grades,
    elasticity_pct_per_100bps = round(unname(elast_train[grades]), 2),
    ci_lower                  = round(unname(ci_lower[grades]),    2),
    ci_upper                  = round(unname(ci_upper[grades]),    2),
    elasticity_train          = round(unname(elast_train[grades]), 2),
    elasticity_test           = round(unname(elast_test[grades]),  2),
    n                         = vapply(grades,
                                       function(g) sum(approved$risk_grade == g),
                                       integer(1)),
    method                    = "continuous_apr_spline_bootstrap",
    stringsAsFactors          = FALSE
  )

  # Reliable iff: CI fully positive AND train/test agree within 50% (or 1pp)
  results$reliable <- with(results,
    !is.na(ci_lower) & ci_lower > 0 &
    !is.na(elasticity_test) &
    abs(elasticity_train - elasticity_test) <=
      pmax(0.5 * abs(elasticity_train), 1.0)
  )

  attr(results, "grade_lookup") <- setNames(results$elasticity_pct_per_100bps,
                                            results$grade)
  attr(results, "n_boot")       <- n_boot
  attr(results, "holdout_frac") <- holdout_frac
  attr(results, "split_kind")   <- split_kind
  attr(results, "version")      <- "tier1"

  n_reliable <- sum(results$reliable, na.rm = TRUE)
  message("calibration: continuous elasticity cache built (",
          split_kind, " split) — ",
          paste(sprintf("%s=%.2f [%.2f, %.2f]",
                        results$grade,
                        results$elasticity_pct_per_100bps,
                        results$ci_lower, results$ci_upper),
                collapse = "; "),
          " | ", n_reliable, "/3 reliable")
  results
}


# ══════════════════════════════════════════════════════════════════════════════
# build_tier2_elasticity_cache  (Tier-2: rich-interaction model)
# ══════════════════════════════════════════════════════════════════════════════
#
# Adds the variables a model risk team would expect:
#   - APR × loan_amount  (bigger loans → more shopping)
#   - APR × term_months  (longer terms feel cheaper monthly)
#   - APR × DTI          (borrowers near affordability ceiling more sensitive)
#   - APR × channel      (Web vs Branch behave differently)
#   - APR × repeat_type  (existing customers stickier)
#   - Month fixed effects (from booking_vintage) — absorbs macro,
#     competitive, and seasonal variation we don't directly observe
#
# Everything else (continuous APR, FICO spline, time-based holdout, bootstrap
# CIs) is inherited from the Tier-1 design.
#
# Output: per-grade table compatible with the Tier-1 `grade_lookup` interface,
# PLUS an optional per-(grade × slice) breakdown when `slice_by` is given.
#
# This function is NOT wired into app startup. To enable, see the comment
# block on `build_continuous_elasticity_cache` above.
#
# Required columns (all in inst/data/loan_data.csv as of 2026-04):
#   offered_decision_flag, issued_flag, risk_grade, interest_rate_apr,
#   fico_score, policy_id, booking_vintage, term_months,
#   debt_to_income_ratio, partner_channel, borrower_repeat_type
# Optional:
#   issued_amount or maximum_offered_amount (loan-amount proxy),
#   joint_application_flag
#
build_tier2_elasticity_cache <- function(credit_data,
                                         n_boot       = 200L,
                                         holdout_frac = 0.2,
                                         fico_df      = 4L,
                                         cov_df       = 3L,
                                         slice_by     = NULL,
                                         seed         = 42L) {

  if (!requireNamespace("splines", quietly = TRUE)) {
    message("calibration: tier-2 elasticity needs the 'splines' package")
    return(NULL)
  }

  required <- c("offered_decision_flag", "issued_flag", "risk_grade",
                "interest_rate_apr", "fico_score", "policy_id",
                "booking_vintage", "term_months", "debt_to_income_ratio",
                "partner_channel", "borrower_repeat_type")
  missing <- setdiff(required, names(credit_data))
  if (length(missing) > 0) {
    message("calibration: tier-2 elasticity skipped — missing: ",
            paste(missing, collapse = ", "))
    return(NULL)
  }

  # ── Pick the loan-amount column ─────────────────────────────────────────
  amt_col <- if ("issued_amount" %in% names(credit_data)) "issued_amount" else
             if ("maximum_offered_amount" %in% names(credit_data)) "maximum_offered_amount" else NA
  has_amt <- !is.na(amt_col)
  has_joint <- "joint_application_flag" %in% names(credit_data)

  # ── Filter approved + complete inputs ───────────────────────────────────
  cd <- credit_data
  keep <- cd$offered_decision_flag == "APPR" &
    !is.na(cd$interest_rate_apr) & !is.na(cd$fico_score) &
    !is.na(cd$issued_flag) & !is.na(cd$risk_grade) &
    !is.na(cd$term_months) & !is.na(cd$debt_to_income_ratio) &
    !is.na(cd$partner_channel) & !is.na(cd$borrower_repeat_type) &
    !is.na(cd$booking_vintage)
  if (has_amt) keep <- keep & !is.na(cd[[amt_col]])
  approved <- cd[keep, ]

  if (nrow(approved) < 2000) {
    message("calibration: tier-2 elasticity needs >=2000 approved rows, got ",
            nrow(approved))
    return(NULL)
  }

  # ── Parse booking_vintage and derive month FE ───────────────────────────
  # booking_vintage is DD/MM/YYYY in the demo dataset
  bv <- suppressWarnings(as.Date(approved$booking_vintage, format = "%d/%m/%Y"))
  if (sum(is.na(bv)) > nrow(approved) * 0.5) {
    bv <- suppressWarnings(as.Date(approved$booking_vintage))  # fallback to ISO
  }
  approved$.booking_date  <- bv
  approved$.booking_month <- format(bv, "%Y-%m")
  approved <- approved[!is.na(approved$.booking_date), ]
  if (nrow(approved) < 2000) {
    message("calibration: tier-2 elasticity — booking_vintage parse left too few rows")
    return(NULL)
  }

  # Loan amount (rescaled to $10k units to keep coefficients stable)
  if (has_amt) approved$.loan_amount_10k <- approved[[amt_col]] / 10000

  # ── Time-based train/test split ─────────────────────────────────────────
  set.seed(seed)
  approved <- approved[order(approved$.booking_date), ]
  n_train <- floor(nrow(approved) * (1 - holdout_frac))
  train   <- approved[seq_len(n_train), ]
  test    <- approved[seq.int(n_train + 1L, nrow(approved)), ]

  # ── Build the formula dynamically ───────────────────────────────────────
  # APR-interaction terms; spline-on-spline gets ugly so use spline ON the
  # covariate, plain APR-times-covariate for interactions.
  rhs <- c(
    "interest_rate_apr * risk_grade",
    "interest_rate_apr * partner_channel",
    "interest_rate_apr * borrower_repeat_type",
    sprintf("interest_rate_apr * splines::ns(term_months, df = %d)", cov_df),
    sprintf("interest_rate_apr * splines::ns(debt_to_income_ratio, df = %d)", cov_df),
    sprintf("splines::ns(fico_score, df = %d)", fico_df),
    "factor(.booking_month)"
  )
  if (has_amt)   rhs <- c(rhs, sprintf("interest_rate_apr * splines::ns(.loan_amount_10k, df = %d)", cov_df))
  if (has_joint) rhs <- c(rhs, "joint_application_flag")
  f <- as.formula(paste("issued_flag ~", paste(rhs, collapse = " + ")))

  # ── Fit + per-grade marginal effect ─────────────────────────────────────
  .fit_extract <- function(df, return_segment = FALSE) {
    m <- tryCatch(
      suppressWarnings(glm(f, data = df, family = binomial(link = "logit"))),
      error = function(e) NULL
    )
    if (is.null(m)) return(NULL)

    # Grade-level marginal effect: pp drop in take-rate per 100bps
    grades <- c("High", "Med", "Low")
    out <- setNames(rep(NA_real_, length(grades)), grades)
    for (g in grades) {
      gd <- df[df$risk_grade == g, ]
      if (nrow(gd) < 100) next
      gd_hi <- gd; gd_hi$interest_rate_apr <- gd$interest_rate_apr + 0.01
      p_lo <- mean(predict(m, newdata = gd,    type = "response"))
      p_hi <- mean(predict(m, newdata = gd_hi, type = "response"))
      out[g] <- (p_lo - p_hi) * 100
    }

    if (!return_segment || is.null(slice_by)) return(list(grade = out, segment = NULL))

    # Optional per-(grade × slice) breakdown
    if (!slice_by %in% names(df)) return(list(grade = out, segment = NULL))
    seg_rows <- list()
    for (g in grades) {
      gd <- df[df$risk_grade == g, ]
      if (nrow(gd) < 100) next
      slice_vals <- unique(gd[[slice_by]])
      for (sv in slice_vals) {
        sd <- gd[gd[[slice_by]] == sv, ]
        if (nrow(sd) < 50) next
        sd_hi <- sd; sd_hi$interest_rate_apr <- sd$interest_rate_apr + 0.01
        p_lo  <- mean(predict(m, newdata = sd,    type = "response"))
        p_hi  <- mean(predict(m, newdata = sd_hi, type = "response"))
        seg_rows[[length(seg_rows) + 1]] <- data.frame(
          grade = g, slice_var = slice_by, slice_val = as.character(sv),
          n = nrow(sd),
          elasticity_pct_per_100bps = round((p_lo - p_hi) * 100, 2),
          stringsAsFactors = FALSE
        )
      }
    }
    seg_df <- if (length(seg_rows) > 0) do.call(rbind, seg_rows) else NULL
    list(grade = out, segment = seg_df)
  }

  # ── Point estimates: train and test ─────────────────────────────────────
  fit_train <- .fit_extract(train, return_segment = TRUE)
  fit_test  <- .fit_extract(test,  return_segment = FALSE)
  if (is.null(fit_train)) {
    message("calibration: tier-2 elasticity model failed on train split")
    return(NULL)
  }
  elast_train <- fit_train$grade
  elast_test  <- if (!is.null(fit_test)) fit_test$grade else
                 setNames(rep(NA_real_, 3), c("High", "Med", "Low"))

  # ── Bootstrap CIs on train ──────────────────────────────────────────────
  message("calibration: bootstrapping tier-2 elasticity (",
          n_boot, " reps; this can take a few minutes)...")
  boot_mat <- matrix(NA_real_, nrow = n_boot, ncol = 3,
                     dimnames = list(NULL, c("High", "Med", "Low")))
  for (b in seq_len(n_boot)) {
    idx <- sample.int(nrow(train), replace = TRUE)
    bt  <- .fit_extract(train[idx, ], return_segment = FALSE)
    if (!is.null(bt)) boot_mat[b, ] <- bt$grade[c("High", "Med", "Low")]
  }
  ci_lower <- apply(boot_mat, 2, quantile, probs = 0.025, na.rm = TRUE)
  ci_upper <- apply(boot_mat, 2, quantile, probs = 0.975, na.rm = TRUE)

  # ── Assemble result ─────────────────────────────────────────────────────
  grades  <- c("High", "Med", "Low")
  results <- data.frame(
    grade                     = grades,
    elasticity_pct_per_100bps = round(unname(elast_train[grades]), 2),
    ci_lower                  = round(unname(ci_lower[grades]),    2),
    ci_upper                  = round(unname(ci_upper[grades]),    2),
    elasticity_train          = round(unname(elast_train[grades]), 2),
    elasticity_test           = round(unname(elast_test[grades]),  2),
    n                         = vapply(grades,
                                       function(g) sum(approved$risk_grade == g),
                                       integer(1)),
    method                    = "tier2_rich_interactions",
    stringsAsFactors          = FALSE
  )
  results$reliable <- with(results,
    !is.na(ci_lower) & ci_lower > 0 &
    !is.na(elasticity_test) &
    abs(elasticity_train - elasticity_test) <=
      pmax(0.5 * abs(elasticity_train), 1.0)
  )

  attr(results, "grade_lookup") <- setNames(results$elasticity_pct_per_100bps,
                                            results$grade)
  attr(results, "segment_table") <- fit_train$segment   # may be NULL
  attr(results, "n_boot")        <- n_boot
  attr(results, "holdout_frac")  <- holdout_frac
  attr(results, "covariates")    <- c(
    "interest_rate_apr", "risk_grade", "partner_channel",
    "borrower_repeat_type", "term_months", "debt_to_income_ratio",
    if (has_amt) "loan_amount" else NULL,
    "fico_score (spline)", "booking_month (FE)",
    if (has_joint) "joint_application_flag" else NULL
  )
  attr(results, "version") <- "tier2"

  n_reliable <- sum(results$reliable, na.rm = TRUE)
  message("calibration: tier-2 elasticity cache built — ",
          paste(sprintf("%s=%.2f [%.2f, %.2f]",
                        results$grade,
                        results$elasticity_pct_per_100bps,
                        results$ci_lower, results$ci_upper),
                collapse = "; "),
          " | ", n_reliable, "/3 reliable | covariates: ",
          length(attr(results, "covariates")))
  results
}


# ══════════════════════════════════════════════════════════════════════════════
# get_calibrated_assumption
# ══════════════════════════════════════════════════════════════════════════════
#
# Three-tier fallback:
#   1. Consultant CSV override (if explicitly set and param present)
#   2. Data-derived calibration cache
#   3. Hardcoded defaults
#
# param_name: "adverse_selection" or "elasticity"
# grade: "High", "Med", or "Low"
# calibration_cache: output of build_adverse_selection_cache or build_elasticity_cache
# consultant_config: list with named vectors (same structure as global consultant_config)
# prefer_data: if TRUE (default), data-derived takes priority over consultant.
#              if FALSE, consultant takes priority (for explicit overrides).
#
# Returns: numeric value (percentage, e.g. 5 means 5%)
#
get_calibrated_assumption <- function(param_name, grade,
                                       calibration_cache = NULL,
                                       consultant_config = NULL,
                                       prefer_data = TRUE) {
  grade <- match.arg(grade, c("High", "Med", "Low"))

  # Helper: extract from consultant config
  .from_consultant <- function() {
    if (is.null(consultant_config)) return(NULL)
    vals <- consultant_config[[param_name]]
    if (is.null(vals)) return(NULL)
    val <- vals[grade]
    if (is.na(val)) return(NULL)
    # CSV values may be character; coerce to numeric
    as.numeric(unname(val))
  }

  # Helper: extract from data-derived cache
  .from_cache <- function() {
    if (is.null(calibration_cache)) return(NULL)

    grade_summary <- attr(calibration_cache, "grade_summary")
    if (is.null(grade_summary)) return(NULL)

    # Determine the value column name based on param type
    val_col <- if (param_name == "adverse_selection") {
      "mean_adverse_pct"
    } else if (param_name == "elasticity") {
      "mean_elasticity_pct"
    } else {
      return(NULL)
    }

    if (!val_col %in% names(grade_summary)) return(NULL)
    row <- grade_summary[grade_summary$grade == grade, ]
    if (nrow(row) == 0 || is.na(row[[val_col]])) return(NULL)
    row[[val_col]]
  }

  # Helper: hardcoded defaults
  .from_defaults <- function() {
    defaults <- .CALIBRATION_DEFAULTS[[param_name]]
    if (is.null(defaults)) return(NA_real_)
    unname(defaults[grade])
  }

  # Apply fallback chain
  if (prefer_data) {
    # Data-derived first, then consultant, then defaults
    val <- .from_cache()
    if (!is.null(val)) return(val)
    val <- .from_consultant()
    if (!is.null(val)) return(val)
    return(.from_defaults())
  } else {
    # Consultant first (explicit override), then data, then defaults
    val <- .from_consultant()
    if (!is.null(val)) return(val)
    val <- .from_cache()
    if (!is.null(val)) return(val)
    return(.from_defaults())
  }
}


# ══════════════════════════════════════════════════════════════════════════════
# build_calibration_cache  (convenience wrapper)
# ══════════════════════════════════════════════════════════════════════════════
#
# Builds both caches in one call. Intended to be called once at app startup
# after credit_data is loaded.
#
# Returns: list with:
#   $adverse_selection — output of build_adverse_selection_cache()
#   $elasticity        — output of build_elasticity_cache()
#
build_calibration_cache <- function(credit_data,
                                     cache_path = NULL,
                                     refresh    = FALSE,
                                     n_boot     = 100L) {

  # Default cache location: alongside the other RDS caches
  if (is.null(cache_path)) {
    base <- if (dir.exists("inst/data")) "inst/data" else
            if (dir.exists("../inst/data")) "../inst/data" else "."
    cache_path <- file.path(base, "calibration_cache.rds")
  }

  # Fingerprint of the input — small, fast, changes when data meaningfully changes
  fp <- list(
    nrow         = nrow(credit_data),
    n_approved   = sum(credit_data$offered_decision_flag == "APPR", na.rm = TRUE),
    n_issued     = sum(credit_data$issued_flag == 1, na.rm = TRUE),
    apr_sum      = round(sum(credit_data$interest_rate_apr, na.rm = TRUE), 4),
    fico_sum     = round(sum(credit_data$fico_score,        na.rm = TRUE), 0),
    grade_table  = if ("risk_grade" %in% names(credit_data))
                     as.list(table(credit_data$risk_grade)) else NULL,
    n_boot       = n_boot,
    schema_ver   = "tier2_v1"
  )
  fp_hash <- if (requireNamespace("digest", quietly = TRUE))
    digest::digest(fp) else paste(unlist(fp), collapse = "_")

  # Try cache first
  if (!refresh && file.exists(cache_path)) {
    cached <- tryCatch(readRDS(cache_path), error = function(e) NULL)
    if (!is.null(cached) && identical(attr(cached, "fp_hash"), fp_hash)) {
      message("calibration: loaded cached calibration from ", cache_path,
              " (built at ", format(attr(cached, "built_at")), ")")
      return(cached)
    }
    if (!is.null(cached)) {
      message("calibration: cache fingerprint mismatch — rebuilding")
    }
  }

  message("calibration: building data-derived calibration caches ",
          "(full rebuild — bootstrap may take several minutes)...")
  t0 <- Sys.time()

  adv  <- build_adverse_selection_cache(credit_data)
  l1   <- build_elasticity_cache(credit_data)
  obs  <- build_observational_elasticity_cache(credit_data)
  tier1 <- tryCatch(
    build_continuous_elasticity_cache(credit_data, n_boot = n_boot),
    error = function(e) { message("calibration: tier-1 failed: ", e$message); NULL })
  tier2 <- tryCatch(
    build_tier2_elasticity_cache(credit_data, n_boot = n_boot),
    error = function(e) { message("calibration: tier-2 failed: ", e$message); NULL })

  # Pick the best observational source for downstream consumers:
  # Tier 2 (rich interactions) > Tier 1 (continuous APR) > original observational
  best_obs <- if (!is.null(tier2) && nrow(tier2) > 0) tier2 else
              if (!is.null(tier1) && nrow(tier1) > 0) tier1 else obs

  result <- list(
    adverse_selection = adv,
    elasticity        = l1,                   # L1 (V4 test/control attribute match)
    obs_elasticity    = best_obs,             # used by get_segment_elasticity Layer 2
    obs_legacy        = obs,                  # original variant-dummy logistic, kept for transparency
    tier1_elasticity  = tier1,                # continuous APR + spline + bootstrap
    tier2_elasticity  = tier2                 # rich interactions + month FE + bootstrap
  )
  attr(result, "fp_hash")  <- fp_hash
  attr(result, "built_at") <- Sys.time()
  attr(result, "active_obs_version") <-
    attr(best_obs, "version", exact = TRUE) %||% "legacy_observational"

  # Persist
  tryCatch({
    saveRDS(result, cache_path)
    message("calibration: cache saved to ", cache_path,
            " (build took ", round(as.numeric(Sys.time() - t0, units = "secs"), 1),
            "s; active obs source = ", attr(result, "active_obs_version"), ")")
  }, error = function(e) message("calibration: cache save failed: ", e$message))

  result
}


# ══════════════════════════════════════════════════════════════════════════════
# get_segment_elasticity — 2-layer waterfall with provenance
# ══════════════════════════════════════════════════════════════════════════════
#
# Layer 1: Direct test/control match (V4 arms, attribute + dominant grade)
# Layer 2: Observational model (pricing variant quasi-experiment, by grade)
#
# All values in pp of take rate reduction per 100bps.
#
# Inputs:
#   primary_var: chr, segment's dominant attribute name (e.g., "N_HARD_INQ_6M"),
#                or NA_character_ if unknown (legacy callers)
#   grade_mix:   named numeric vector of proportions, e.g., c(High=0.8, Med=0.15, Low=0.05)
#   calibration_cache: the L1 elasticity cache (data.frame with attribute column)
#   obs_elasticity_cache: the L2 observational cache (from build_observational_elasticity_cache)
#   consultant_config: unused (retained for backward compatibility)
#
# Returns: list(value, source, layer, confidence, n, detail)
#
# Internal helper: Layer 1 direct match. Returns a list or NULL.
.elasticity_layer1 <- function(primary_var, dominant_grade, calibration_cache) {
  if (is.null(calibration_cache) || !is.data.frame(calibration_cache)) return(NULL)
  if (!"attribute" %in% colnames(calibration_cache)) return(NULL)
  if (is.na(primary_var) || !nzchar(primary_var)) return(NULL)

  # Case-insensitive match — caller may pass "n_hard_inq_6m" while the cache
  # stores "N_HARD_INQ_6M" (uppercased during build_elasticity_cache()).
  pv_norm    <- toupper(primary_var)
  attr_norm  <- toupper(calibration_cache$attribute)

  matches <- calibration_cache[
    !is.na(calibration_cache$attribute) &
    attr_norm == pv_norm &
    calibration_cache$grade == dominant_grade &
    calibration_cache$reliable == TRUE &
    !is.na(calibration_cache$elasticity_pct_per_100bps), ]

  if (nrow(matches) == 0) return(NULL)

  val <- mean(matches$elasticity_pct_per_100bps)
  list(
    value  = val,
    n      = sum(matches$n_test),
    detail = sprintf("Observed on V4 %s segment, %s grade (N=%d across %d test arm%s)",
                     pv_norm, dominant_grade, sum(matches$n_test),
                     nrow(matches), if (nrow(matches) == 1) "" else "s")
  )
}

# Internal helper: Layer 2 observational model. Returns a list or NULL.
# Schema-tolerant: works with the original variant-dummy cache (which has
# n_std/n_va/n_vb columns) AND with the Tier-1/Tier-2 caches (which have
# ci_lower/ci_upper instead). The detail string adapts to whichever is present.
.elasticity_layer2 <- function(dominant_grade, obs_elasticity_cache) {
  if (is.null(obs_elasticity_cache) || !is.data.frame(obs_elasticity_cache)) return(NULL)
  if (!"grade" %in% colnames(obs_elasticity_cache) ||
      !"reliable" %in% colnames(obs_elasticity_cache) ||
      !"elasticity_pct_per_100bps" %in% colnames(obs_elasticity_cache)) return(NULL)

  row <- obs_elasticity_cache[obs_elasticity_cache$grade == dominant_grade &
                               obs_elasticity_cache$reliable == TRUE, ]
  if (nrow(row) == 0) return(NULL)

  val <- row$elasticity_pct_per_100bps[1]
  if (is.na(val)) return(NULL)

  n_total <- if ("n" %in% colnames(row)) as.integer(row$n[1]) else NA_integer_
  cols <- colnames(row)

  # Build a detail string appropriate to whichever schema we got
  detail <- if (all(c("ci_lower", "ci_upper") %in% cols)) {
    method_label <- if ("method" %in% cols) row$method[1] else "rich-interaction model"
    sprintf("%s, %s grade (N=%d, 95%% CI [%.2f, %.2f])",
            method_label, dominant_grade,
            n_total %||% 0L,
            row$ci_lower[1], row$ci_upper[1])
  } else if (all(c("n_std", "n_va", "n_vb") %in% cols)) {
    sprintf("Observational model (pricing variant quasi-experiment), %s grade (N=%d, Std/VA/VB=%d/%d/%d)",
            dominant_grade, n_total %||% 0L,
            row$n_std[1], row$n_va[1], row$n_vb[1])
  } else {
    sprintf("Observational elasticity, %s grade (N=%d)",
            dominant_grade, n_total %||% 0L)
  }

  list(value = val, n = n_total, detail = detail)
}

get_segment_elasticity <- function(primary_var, grade_mix,
                                    calibration_cache = NULL,
                                    obs_elasticity_cache = NULL,
                                    consultant_config = NULL) {

  # Determine dominant grade (must be >= 50% of mix)
  dominant_grade <- if (length(grade_mix) > 0 && sum(grade_mix) > 0) {
    idx <- which.max(grade_mix)
    if (grade_mix[idx] >= 0.5) names(grade_mix)[idx] else "Med"
  } else {
    "Med"
  }

  # ── Layer 1: Direct test/control match ────────────────────────────────
  l1 <- tryCatch(
    .elasticity_layer1(primary_var, dominant_grade, calibration_cache),
    error = function(e) NULL
  )
  if (!is.null(l1)) {
    return(list(value = as.numeric(l1$value),
                source = "direct_test_control", layer = 1L,
                confidence = "high",
                n = as.integer(l1$n), detail = l1$detail))
  }

  # ── Layer 2: Observational model (pricing variant quasi-experiment) ───
  l2 <- tryCatch(
    .elasticity_layer2(dominant_grade, obs_elasticity_cache),
    error = function(e) NULL
  )
  if (!is.null(l2)) {
    return(list(value = as.numeric(l2$value),
                source = "observational_model", layer = 2L,
                confidence = "medium",
                n = l2$n, detail = l2$detail))
  }

  # ── Fallback: hardcoded defaults (should not reach here if data is loaded) ──
  defaults <- c(High = 3, Med = 5, Low = 8)
  val <- as.numeric(defaults[dominant_grade])

  list(value = val,
       source = "hardcoded_fallback", layer = 99L,
       confidence = "low",
       n = NA_integer_,
       detail = sprintf("Hardcoded fallback for %s grade (obs cache unavailable)", dominant_grade))
}

# Internal: map a segment's primary attribute to get_neighbor_metrics() decline_reason
.attribute_to_decline_reason <- function(primary_var) {
  if (is.na(primary_var) || !nzchar(primary_var)) return(NA_character_)
  # get_neighbor_metrics supports: "01. Fico", "02. Thin file",
  # "03. Delinquency decline", "04. HighDTI", "05. HardInquiry"
  switch(primary_var,
    "fico_score"                   = "01. Fico",
    "FICO"                         = "01. Fico",
    "credit_file_age_months"       = "02. Thin file",
    "delinquent_accounts_count"    = "03. Delinquency decline",
    "debt_to_income_ratio"         = "04. HighDTI",
    "N_HARD_INQ_6M"                = "05. HardInquiry",
    NA_character_  # no mapping
  )
}

# Inverse of .attribute_to_decline_reason — used by swap-in UI to build
# a segment_def for the waterfall-backed projection/elasticity lookup.
.decline_reason_to_attribute <- function(decline_reason) {
  if (is.null(decline_reason) || is.na(decline_reason)) return(NA_character_)
  switch(decline_reason,
    "01. Fico"                = "fico_score",
    "02. Thin file"           = "credit_file_age_months",
    "03. Delinquency decline" = "delinquent_accounts_count",
    "04. HighDTI"             = "debt_to_income_ratio",
    "05. HardInquiry"         = "N_HARD_INQ_6M",
    NA_character_
  )
}

# ══════════════════════════════════════════════════════════════════════════════
# get_segment_bad_rate_projection — 4-layer waterfall with provenance
# ══════════════════════════════════════════════════════════════════════════════
#
# Layer 1: Direct test/control observed bad rate (attribute + dominant grade)
# Layer 2: Neighbor band via get_neighbor_metrics() from R/engine/irr.R
# Layer 3: Grade-weighted observed bad rate from credit_data
# Layer 4: Vintage-curve projection via compute_segment_projected_bad_rate()
#

# Internal helper: Layer 1 direct test/control bad rate. Returns a list or NULL.
.bad_rate_layer1 <- function(primary_var, dominant_grade, credit_data,
                              calibration_cache) {
  elast_cache <- if (is.list(calibration_cache) && !is.data.frame(calibration_cache))
    calibration_cache$elasticity else calibration_cache
  if (is.null(elast_cache) || !is.data.frame(elast_cache)) return(NULL)
  if (!"attribute" %in% colnames(elast_cache)) return(NULL)
  if (is.na(primary_var) || !nzchar(primary_var)) return(NULL)

  matches <- elast_cache[
    !is.na(elast_cache$attribute) &
    elast_cache$attribute == primary_var &
    elast_cache$grade == dominant_grade &
    elast_cache$reliable == TRUE, ]

  if (nrow(matches) == 0) return(NULL)

  test_ids <- unique(matches$test_group)
  obs_loans <- credit_data[
    !is.na(credit_data$policy_id) &
    credit_data$policy_id %in% test_ids &
    credit_data$issued_flag == 1 &
    !is.na(credit_data$risk_grade) &
    credit_data$risk_grade == dominant_grade, ]

  if (nrow(obs_loans) < 30) return(NULL)

  val <- mean(obs_loans$performance_delinquency_flag, na.rm = TRUE)
  list(value = val, n = nrow(obs_loans),
       detail = sprintf("Observed bad rate on V4 %s segment, %s grade (N=%d)",
                        primary_var, dominant_grade, nrow(obs_loans)))
}

# Internal helper: Layer 2 neighbor band. Returns a list or NULL.
.bad_rate_layer2 <- function(primary_var, segment_def, credit_data, perf_tape) {
  decline_reason <- .attribute_to_decline_reason(primary_var)
  if (is.na(decline_reason)) return(NULL)
  if (is.null(perf_tape)) return(NULL)
  threshold <- segment_def$cutoff_val
  if (is.null(threshold) || is.na(threshold)) return(NULL)

  nb <- get_neighbor_metrics(perf_tape, credit_data, decline_reason, threshold)
  if (is.null(nb) || is.null(nb$n_loans) || nb$n_loans < 200) return(NULL)
  if (is.na(nb$delinq_rate)) return(NULL)

  list(value = nb$delinq_rate, n = nb$n_loans,
       detail = sprintf("Neighbor band just above %s cutoff of %s (N=%d)",
                        primary_var, format(threshold), nb$n_loans))
}

# Internal helper: Layer 3 grade-weighted observed. Returns a list or NULL.
.bad_rate_layer3 <- function(dominant_grade, credit_data) {
  issued_grade <- credit_data[
    !is.na(credit_data$issued_flag) & credit_data$issued_flag == 1 &
    !is.na(credit_data$risk_grade) & credit_data$risk_grade == dominant_grade, ]
  if (nrow(issued_grade) < 100) return(NULL)
  val <- mean(issued_grade$performance_delinquency_flag, na.rm = TRUE)
  if (is.na(val)) return(NULL)
  list(value = val, n = nrow(issued_grade),
       detail = sprintf("Portfolio mean bad rate for %s grade (N=%d)",
                        dominant_grade, nrow(issued_grade)))
}

# Internal helper: Layer 4 vintage curve model. Returns a list or NULL.
.bad_rate_layer4 <- function(segment_def, credit_data, perf_tape) {
  loan_ids <- segment_def$loan_ids
  if (is.null(loan_ids) || length(loan_ids) == 0) return(NULL)
  if (is.null(perf_tape)) return(NULL)
  proj <- compute_segment_projected_bad_rate(as.character(loan_ids),
                                              perf_tape, credit_data)
  if (is.null(proj) || is.na(proj$segment_mean_projected)) return(NULL)
  list(value = proj$segment_mean_projected,
       n = length(loan_ids),
       detail = sprintf("Vintage-curve projection from performance tape (N=%d)",
                        length(loan_ids)))
}

get_segment_bad_rate_projection <- function(segment_def, credit_data,
                                             perf_tape = NULL,
                                             calibration_cache = NULL,
                                             consultant_config = NULL) {

  primary_var <- segment_def$primary_var %||% NA_character_
  grade_mix   <- segment_def$grade_mix %||% c(High = 0, Med = 1, Low = 0)

  dominant_grade <- if (length(grade_mix) > 0 && sum(grade_mix) > 0) {
    idx <- which.max(grade_mix)
    if (grade_mix[idx] >= 0.5) names(grade_mix)[idx] else "Med"
  } else {
    "Med"
  }

  # ── Layer 1: Direct test/control bad rate ─────────────────────────────
  l1 <- tryCatch(
    .bad_rate_layer1(primary_var, dominant_grade, credit_data, calibration_cache),
    error = function(e) NULL
  )
  if (!is.null(l1)) {
    return(list(value = as.numeric(l1$value),
                source = "direct_test_control", layer = 1L,
                confidence = "high",
                n = as.integer(l1$n), detail = l1$detail))
  }

  # ── Layer 2: Neighbor band ────────────────────────────────────────────
  l2 <- tryCatch(
    .bad_rate_layer2(primary_var, segment_def, credit_data, perf_tape),
    error = function(e) NULL
  )
  if (!is.null(l2)) {
    return(list(value = as.numeric(l2$value),
                source = "neighbor_band", layer = 2L,
                confidence = "medium",
                n = as.integer(l2$n), detail = l2$detail))
  }

  # ── Layer 3: Grade-weighted observed bad rate ─────────────────────────
  l3 <- tryCatch(
    .bad_rate_layer3(dominant_grade, credit_data),
    error = function(e) NULL
  )
  if (!is.null(l3)) {
    return(list(value = as.numeric(l3$value),
                source = "closest_segment", layer = 3L,
                confidence = "medium_low",
                n = as.integer(l3$n), detail = l3$detail))
  }

  # ── Layer 4: Vintage-curve model projection ───────────────────────────
  l4 <- tryCatch(
    .bad_rate_layer4(segment_def, credit_data, perf_tape),
    error = function(e) NULL
  )
  if (!is.null(l4)) {
    return(list(value = as.numeric(l4$value),
                source = "model_projection", layer = 4L,
                confidence = "low",
                n = as.integer(l4$n), detail = l4$detail))
  }

  # Final fallback: portfolio mean
  val <- mean(credit_data$performance_delinquency_flag[credit_data$issued_flag == 1],
              na.rm = TRUE)
  if (is.na(val)) val <- 0.04
  list(value = as.numeric(val),
       source = "portfolio_mean", layer = 4L,
       confidence = "low",
       n = NA_integer_,
       detail = "Portfolio-wide issued bad rate (final fallback)")
}
