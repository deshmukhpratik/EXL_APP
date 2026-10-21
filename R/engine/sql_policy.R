# ============================================================
# SQL Policy Code Generator
# Translates L10-L90 policy parameters into readable SQL
# Uses CTE-per-layer so aliases are resolved before reference.
# ============================================================

generate_policy_sql <- function(params,
                                label        = "Baseline Policy",
                                segment_cond = NULL,
                                action       = NULL,
                                bps          = 0) {

  # --- header ---
  hdr <- paste0(
    "-- =============================================================\n",
    "-- Credit Policy: ", label, "\n",
    "-- Generated: ", Sys.Date(), "\n",
    "-- =============================================================\n"
  )

  # --- Build the segment WHERE clause once (used in L90 and Pricing) ---
  # NOTE: CART segment conditions may reference any column from the input

  # `applications` table (e.g. internal_file_age_months, fico_score,
  # partner_channel).  These columns are carried through every CTE layer
  # via SELECT *, so they remain available in layer90 even though they
  # are never explicitly aliased in the CTE chain.
  sql_where <- NULL
  if (!is.null(segment_cond) && !is.null(action)) {
    sql_where <- r_condition_to_sql(segment_cond)
  }

  # ================================================================
  # LAYER 10: Feature Derivation
  # ================================================================
  l10 <- paste0(
    "\n-- LAYER 10: Feature Derivation\n",
    "-- (derived columns added to every applicant row)\n",
    "layer10 AS (\n",
    "  SELECT *,\n",
    "    CASE\n",
    "      WHEN monthly_rent IS NOT NULL THEN GREATEST(monthly_rent, 300)\n",
    "      ELSE NULL\n",
    "    END AS rent_est,\n",
    "\n",
    "    CASE\n",
    "      WHEN annual_income > 0\n",
    "           AND monthly_debt_payment IS NOT NULL\n",
    "           AND monthly_rent IS NOT NULL\n",
    "      THEN ROUND(\n",
    "             ((monthly_debt_payment + GREATEST(monthly_rent, 300))\n",
    "              / annual_income) * 100, 2)\n",
    "      ELSE NULL\n",
    "    END AS dti5,\n",
    "\n",
    "    CASE\n",
    "      WHEN annual_income < 140000 THEN annual_income * 0.06\n",
    "      ELSE 0\n",
    "    END AS social_tax,\n",
    "\n",
    "    annual_income * 0.04 AS med_tax,\n",
    "\n",
    "    annual_income\n",
    "      - CASE WHEN annual_income < 140000 THEN annual_income * 0.06 ELSE 0 END\n",
    "      - annual_income * 0.04\n",
    "    AS post_tax_income\n",
    "\n",
    "  FROM applications\n",
    "),\n"
  )

  # ================================================================
  # LAYER 20: Risk Grading (3-model waterfall)
  # Split into sub-CTEs because each step references the prior alias.
  # ================================================================

  # Step 20a: Model 1 Risk Band
  l20a <- paste0(
    "\n-- LAYER 20: Risk Grading (3-model waterfall)\n",
    "-- Step 20a: Model 1 Risk Band\n",
    "layer20a AS (\n",
    "  SELECT *,\n",
    "    CASE\n",
    "      WHEN risk_model_score_1 IS NULL THEN 3\n",
    "      WHEN risk_model_score_1 = 0     THEN 1\n",
    "      WHEN risk_model_score_1 < ", fmt(params$model_score_1_cutoff), " THEN 2\n",
    "      WHEN risk_model_score_1 < ", fmt(params$model_score_1_upper),  " THEN 3\n",
    "      ELSE 4\n",
    "    END AS model_1_risk_band\n",
    "  FROM layer10\n",
    "),\n"
  )

  # Step 20b: Model 2 Risk Band
  l20b <- paste0(
    "-- Step 20b: Model 2 Risk Band\n",
    "layer20b AS (\n",
    "  SELECT *,\n",
    "    CASE\n",
    "      WHEN risk_model_score_2 IS NULL THEN 2\n",
    "      WHEN risk_model_score_2 < ", fmt(params$model_score_2_band1),
         " AND model_1_risk_band IN (1, 2) THEN 1\n",
    "      WHEN risk_model_score_2 < ", fmt(params$model_score_2_band2),
         " AND model_1_risk_band IN (1, 2) THEN 2\n",
    "      WHEN risk_model_score_2 < ", fmt(params$model_score_2_band3),
         " AND model_1_risk_band IN (1, 3) THEN 3\n",
    "      WHEN risk_model_score_2 < ", fmt(params$model_score_2_band4),
         " AND model_1_risk_band IN (1, 4) THEN 4\n",
    "      ELSE 2\n",
    "    END AS model_2_risk_band\n",
    "  FROM layer20a\n",
    "),\n"
  )

  # Step 20c: Intermediate Risk Rank (M1 x M2 matrix)
  l20c <- paste0(
    "-- Step 20c: Intermediate Risk Rank (M1 x M2 matrix)\n",
    "-- Matrix:  B1  B2  B3  B4\n",
    "--   M1=1:   1   1   2   2\n",
    "--   M1=2:   1   2   2   3\n",
    "--   M1=3:   2   2   3   3\n",
    "--   M1=4:   2   3   3   4\n",
    "layer20c AS (\n",
    "  SELECT *,\n",
    "    CASE\n",
    "      WHEN model_1_risk_band = 1 AND model_2_risk_band IN (1,2) THEN 1\n",
    "      WHEN model_1_risk_band = 1 AND model_2_risk_band IN (3,4) THEN 2\n",
    "      WHEN model_1_risk_band = 2 AND model_2_risk_band = 1      THEN 1\n",
    "      WHEN model_1_risk_band = 2 AND model_2_risk_band IN (2,3) THEN 2\n",
    "      WHEN model_1_risk_band = 2 AND model_2_risk_band = 4      THEN 3\n",
    "      WHEN model_1_risk_band = 3 AND model_2_risk_band IN (1,2) THEN 2\n",
    "      WHEN model_1_risk_band = 3 AND model_2_risk_band IN (3,4) THEN 3\n",
    "      WHEN model_1_risk_band = 4 AND model_2_risk_band = 1      THEN 2\n",
    "      WHEN model_1_risk_band = 4 AND model_2_risk_band IN (2,3) THEN 3\n",
    "      WHEN model_1_risk_band = 4 AND model_2_risk_band = 4      THEN 4\n",
    "      ELSE 2\n",
    "    END AS intermediate_risk_rank\n",
    "  FROM layer20b\n",
    "),\n"
  )

  # Step 20d: Model 3 Final Rank Adjustment
  l20d <- paste0(
    "-- Step 20d: Model 3 Final Rank Adjustment\n",
    "-- Tight/loose thresholds for IntRank 1 produce step-up tiers (1 vs 2),\n",
    "-- not redundant same-rank assignments.\n",
    "layer20d AS (\n",
    "  SELECT *,\n",
    "    CASE\n",
    "      WHEN risk_model_score_3 IS NULL THEN 2\n",
    "      WHEN intermediate_risk_rank = 1 AND risk_model_score_3 < ", fmt(params$model_score_3_r1_tight), " THEN 1\n",
    "      WHEN intermediate_risk_rank = 1 AND risk_model_score_3 < ", fmt(params$model_score_3_r1_loose), " THEN 2\n",
    "      WHEN intermediate_risk_rank = 2 AND risk_model_score_3 < ", fmt(params$model_score_3_r2_keep),  " THEN 1\n",
    "      WHEN intermediate_risk_rank = 2 AND risk_model_score_3 < ", fmt(params$model_score_3_r2_upgrade), " THEN 2\n",
    "      WHEN intermediate_risk_rank = 3 AND risk_model_score_3 < ", fmt(params$model_score_3_r3), " THEN 3\n",
    "      WHEN intermediate_risk_rank = 4 AND risk_model_score_3 < ", fmt(params$model_score_3_r4), " THEN 4\n",
    "      ELSE 2\n",
    "    END AS final_model_adjusted_rank\n",
    "  FROM layer20c\n",
    "),\n"
  )

  # Step 20e: Branch Tier Mapping
  l20e <- paste0(
    "-- Step 20e: Branch Tier Mapping\n",
    "layer20e AS (\n",
    "  SELECT *,\n",
    "    CASE\n",
    "      WHEN internal_file_age_months >= ", params$branch_file_age_threshold,
           "\n           AND months_since_last_delinquency >= ", params$branch_delinq_months, " THEN\n",
    "        CASE final_model_adjusted_rank\n",
    "          WHEN 1 THEN 1   WHEN 2 THEN 2   WHEN 3 THEN 3   WHEN 4 THEN 3\n",
    "        END\n",
    "      ELSE\n",
    "        CASE final_model_adjusted_rank\n",
    "          WHEN 1 THEN 1   WHEN 2 THEN 3   WHEN 3 THEN 4   WHEN 4 THEN 4\n",
    "        END\n",
    "    END AS underwriting_risk_tier\n",
    "  FROM layer20d\n",
    "),\n"
  )

  # Step 20f: Risk Grade
  l20f <- paste0(
    "-- Step 20f: Risk Grade\n",
    "layer20 AS (\n",
    "  SELECT *,\n",
    "    CASE\n",
    "      WHEN underwriting_risk_tier = 1         THEN 'High'\n",
    "      WHEN underwriting_risk_tier = 2         THEN 'Med'\n",
    "      WHEN underwriting_risk_tier IN (3, 4)   THEN 'Low'\n",
    "    END AS risk_grade\n",
    "  FROM layer20e\n",
    "),\n"
  )

  # ================================================================
  # LAYER 30: Base Pricing
  # ================================================================
  l30 <- paste0(
    "\n-- LAYER 30: Base Pricing (APR by credit quality grade)\n",
    "layer30 AS (\n",
    "  SELECT *,\n",
    "    CASE risk_grade\n",
    "      WHEN 'High' THEN ", fmt(params$apr_high), "\n",
    "      WHEN 'Med'  THEN ", fmt(params$apr_med),  "\n",
    "      WHEN 'Low'  THEN ", fmt(params$apr_low),  "\n",
    "    END AS base_apr\n",
    "  FROM layer20\n",
    "),\n"
  )

  # ================================================================
  # LAYER 40: Channel Variant Pricing
  # ================================================================
  l40 <- paste0(
    "\n-- LAYER 40: Channel / Variant Pricing\n",
    "layer40 AS (\n",
    "  SELECT *,\n",
    "    CASE\n",
    "      WHEN partner_channel = 'Direct'\n",
    "           AND fico_score > ", params$variant_fico_floor, "\n",
    "           AND pricing_test_variant = 'A'\n",
    "        THEN base_apr + ", fmt(params$variant_a_add), "\n",
    "      WHEN partner_channel = 'Direct'\n",
    "           AND fico_score > ", params$variant_fico_floor, "\n",
    "           AND pricing_test_variant = 'B'\n",
    "        THEN base_apr + ", fmt(params$variant_b_add), "\n",
    "      ELSE base_apr\n",
    "    END AS adjusted_apr_pre_term_cap\n",
    "  FROM layer30\n",
    "),\n"
  )

  # ================================================================
  # LAYER 50: Maximum Approved Amount
  # Split: 50a (final_apr + exposure cap) → 50b (income limit) → 50 (max approved)
  # ================================================================

  # FIX: Use formatC WITHOUT big.mark to avoid "50,000" breaking SQL
  l50a <- paste0(
    "\n-- LAYER 50: Maximum Approved Amount\n",
    "-- Step 50a: APR cap for 72-month terms + exposure limit\n",
    "layer50a AS (\n",
    "  SELECT *,\n",
    "    CASE\n",
    "      WHEN term_months = 72 THEN LEAST(adjusted_apr_pre_term_cap, ", fmt(params$term72_apr_cap), ")\n",
    "      ELSE adjusted_apr_pre_term_cap\n",
    "    END AS final_apr,\n",
    "\n",
    "    CASE risk_grade\n",
    "      WHEN 'High' THEN ", formatC(params$exposure_cap_high, format = "f", big.mark = "", digits = 0), "\n",
    "      ELSE             ", formatC(params$exposure_cap_other, format = "f", big.mark = "", digits = 0), "\n",
    "    END AS exposure_limit_cap\n",
    "  FROM layer40\n",
    "),\n"
  )

  l50b <- paste0(
    "-- Step 50b: Income-based limit\n",
    "layer50b AS (\n",
    "  SELECT *,\n",
    "    (post_tax_income - 12 * COALESCE(monthly_debt_payment, 0))\n",
    "      / NULLIF(1 - final_apr, 0)\n",
    "    AS income_based_limit_cap\n",
    "  FROM layer50a\n",
    "),\n"
  )

  l50 <- paste0(
    "-- Step 50c: Maximum approved amount\n",
    "layer50 AS (\n",
    "  SELECT *,\n",
    "    LEAST(income_based_limit_cap, exposure_limit_cap) AS maximum_approved_amount\n",
    "  FROM layer50b\n",
    "),\n"
  )

  # ================================================================
  # LAYER 60: Base Decline Decision
  # ================================================================
  l60 <- paste0(
    "\n-- LAYER 60: Base Decline Decision (ordered waterfall)\n",
    "layer60 AS (\n",
    "  SELECT *,\n",
    "    CASE\n",
    "      WHEN fico_score IS NULL\n",
    "           OR fico_score < ", params$fico_min,
             " OR fico_score > ", params$fico_max, "\n",
    "        THEN '01. Fico'\n",
    "      WHEN credit_file_age_months <= ", params$thin_file_months, "\n",
    "        THEN '02. Thin file'\n",
    "      WHEN delinquent_accounts_count > ", params$delinquency_max, "\n",
    "        THEN '03. Delinquency decline'\n",
    "      WHEN debt_to_income_ratio > ", fmt(params$dti_cutoff), "\n",
    "        THEN '04. HighDTI'\n",
    "      WHEN maximum_approved_amount < 1\n",
    "        THEN '05. max_amount_limit'\n",
    "      ELSE 'APPR'\n",
    "    END AS decision_flag\n",
    "  FROM layer50\n",
    "),\n"
  )

  # ================================================================
  # LAYER 70: Term Assignment
  # Split: 70a (base_term) → 70 (term_months + offered_term_months)
  # ================================================================
  l70a <- paste0(
    "\n-- LAYER 70: Term / Tenure Assignment\n",
    "-- Step 70a: Base terms by grade\n",
    "layer70a AS (\n",
    "  SELECT *,\n",
    "    CASE risk_grade\n",
    "      WHEN 'High' THEN ", params$term_base_high, "\n",
    "      WHEN 'Med'  THEN ", params$term_base_med, "\n",
    "      WHEN 'Low'  THEN ", params$term_base_low, "\n",
    "      ELSE 36\n",
    "    END AS base_term\n",
    "  FROM layer60\n",
    "),\n"
  )

  # Note: applications already has term_months (the applicant's requested term).
  # L50 uses that input term_months for the 72-month APR cap.
  # Here, L70 computes the POLICY-assigned term (policy_term_months) which may
  # differ from the requested term.  offered_term_months = min(requested, policy)
  # is used by L80 for affordability.
  # We use distinct names to avoid duplicate-column errors with SELECT *.
  term_case <- paste0(
    "    CASE\n",
    "      WHEN risk_grade = 'High'\n",
    "           AND credit_file_age_months >= ", params$term_ext_file_age_upper, "\n",
    "           AND months_since_last_delinquency >= ", params$term_ext_delinq_free, "\n",
    "           AND debt_to_income_ratio <= ", fmt(params$term_ext_dti_upper), "\n",
    "        THEN base_term + 24\n",
    "      WHEN risk_grade = 'High'\n",
    "           AND credit_file_age_months >= ", params$term_ext_file_age_lower, "\n",
    "           AND months_since_last_delinquency >= ", params$term_ext_delinq_free, "\n",
    "           AND debt_to_income_ratio <= ", fmt(params$term_ext_dti_lower), "\n",
    "        THEN base_term + 12\n",
    "      WHEN risk_grade = 'Med'\n",
    "           AND credit_file_age_months >= ", params$term_ext_file_age_upper, "\n",
    "           AND months_since_last_delinquency >= ", params$term_ext_delinq_free, "\n",
    "           AND debt_to_income_ratio <= ", fmt(params$term_ext_dti_upper), "\n",
    "        THEN base_term + 24\n",
    "      WHEN risk_grade = 'Med'\n",
    "           AND credit_file_age_months >= ", params$term_ext_file_age_lower, "\n",
    "           AND months_since_last_delinquency >= ", params$term_ext_delinq_free, "\n",
    "           AND debt_to_income_ratio <= ", fmt(params$term_ext_dti_lower), "\n",
    "        THEN base_term + 12\n",
    "      WHEN risk_grade = 'Low'\n",
    "           AND credit_file_age_months >= ", params$term_ext_file_age_upper, "\n",
    "           AND months_since_last_delinquency >= ", params$term_ext_delinq_free, "\n",
    "           AND debt_to_income_ratio <= ", fmt(params$term_ext_dti_upper), "\n",
    "        THEN base_term + 12\n",
    "      ELSE base_term\n",
    "    END")

  l70 <- paste0(
    "-- Step 70b: Policy term + offered term\n",
    "layer70 AS (\n",
    "  SELECT *,\n",
    "    -- Policy-assigned term (may differ from applicant's requested term_months)\n",
    term_case, " AS policy_term_months,\n",
    "\n",
    "    -- offered_term_months = MIN(requested, policy) for affordability\n",
    "    LEAST(\n",
    "      COALESCE(term_months, ", term_case, "),\n",
    "      ", term_case, "\n",
    "    ) AS offered_term_months\n",
    "  FROM layer70a\n",
    "),\n"
  )

  # ================================================================
  # LAYER 80: Affordability & Offered Amount
  # Split: 80a (offered_final_apr + pti_cap) → 80b (max_new_installment)
  #      → 80c (affordability_limit_cap) → 80 (maximum_offered_amount)
  # ================================================================
  l80a <- paste0(
    "\n-- LAYER 80: Affordability & Offered Amount\n",
    "-- Step 80a: Offered APR + PTI cap\n",
    "layer80a AS (\n",
    "  SELECT *,\n",
    "    CASE\n",
    "      WHEN offered_term_months = 72\n",
    "        THEN LEAST(adjusted_apr_pre_term_cap, ", fmt(params$term72_apr_cap), ")\n",
    "      ELSE adjusted_apr_pre_term_cap\n",
    "    END AS offered_final_apr,\n",
    "\n",
    "    CASE risk_grade\n",
    "      WHEN 'High' THEN ", fmt(params$pti_high), "\n",
    "      WHEN 'Med'  THEN ", fmt(params$pti_med), "\n",
    "      WHEN 'Low'  THEN ", fmt(params$pti_low), "\n",
    "      ELSE 0.20\n",
    "    END AS pti_cap\n",
    "  FROM layer70\n",
    "),\n"
  )

  l80b <- paste0(
    "-- Step 80b: Max new monthly installment\n",
    "layer80b AS (\n",
    "  SELECT *,\n",
    "    (pti_cap * (post_tax_income / 12))\n",
    "      - COALESCE(monthly_debt_payment, 0)\n",
    "    AS max_new_installment\n",
    "  FROM layer80a\n",
    "),\n"
  )

  l80c <- paste0(
    "-- Step 80c: Affordability limit (PV annuity)\n",
    "layer80c AS (\n",
    "  SELECT *,\n",
    "    CASE\n",
    "      WHEN max_new_installment <= 0 THEN 0\n",
    "      WHEN offered_final_apr / 12 = 0\n",
    "        THEN max_new_installment * offered_term_months\n",
    "      ELSE\n",
    "        max_new_installment\n",
    "        * (POWER(1 + offered_final_apr / 12, offered_term_months) - 1)\n",
    "        / ((offered_final_apr / 12) * POWER(1 + offered_final_apr / 12, offered_term_months))\n",
    "    END AS affordability_limit_cap\n",
    "  FROM layer80b\n",
    "),\n"
  )

  l80 <- paste0(
    "-- Step 80d: Maximum offered amount\n",
    "layer80 AS (\n",
    "  SELECT *,\n",
    "    LEAST(maximum_approved_amount, affordability_limit_cap)\n",
    "      AS maximum_offered_amount\n",
    "  FROM layer80c\n",
    "),\n"
  )

  # ================================================================
  # LAYER 90: Offered Decision
  # Segment decline (if any) is integrated directly as a WHEN clause
  # so no UPDATE is needed.
  # ================================================================

  # Build the segment decline WHEN clause for Decline action
  segment_when <- ""
  if (!is.null(action) && action == "Decline" && !is.null(sql_where)) {
    # Comment-safe version: prefix every line with "--" so multi-line
    # conditions don't leave orphan AND keywords in active SQL
    sql_where_commented <- gsub("\n", "\n      -- ", sql_where)
    segment_when <- paste0(
      "\n      -- SEGMENT OVERRIDE: Decline ALL loans in CART segment\n",
      "      -- Segment: ", sql_where_commented, "\n",
      "      WHEN (", sql_where, ")\n",
      "        THEN 'DECL_SEGMENT'\n"
    )
  }

  l90 <- paste0(
    "\n-- LAYER 90: Offered Decision (final decline on offered amounts)\n",
    if (!is.null(action) && action == "Decline" && !is.null(sql_where)) {
      paste0(
        "-- SEGMENT OVERRIDE: Decline loans matching CART segment\n",
        "-- Condition: ", gsub("\n", "\n-- ", sql_where), "\n"
      )
    } else "",
    "layer90 AS (\n",
    "  SELECT *,\n",
    "    CASE\n",
    segment_when,
    "      WHEN decision_flag <> 'APPR' THEN decision_flag\n",
    "      WHEN maximum_offered_amount < 1\n",
    "        THEN '90. Affordability'\n",
    "      ELSE 'APPR'\n",
    "    END AS offered_decision_flag\n",
    "  FROM layer80\n",
    ")\n"
  )

  # --- Final SELECT with issued_flag ---
  final_select <- paste0(
    "\n-- Final output\n",
    "SELECT *,\n",
    "  CASE WHEN offered_decision_flag = 'APPR' THEN 1 ELSE 0 END AS issued_flag\n",
    "FROM layer90;\n"
  )

  # --- Segment override for Pricing (uses UPDATE — requires staging table) ---
  seg_block <- ""
  if (!is.null(segment_cond) && !is.null(action)) {
    if (action == "Pricing") {
      apr_delta <- fmt(bps / 10000)
      apr_cap   <- fmt(params$term72_apr_cap)
      pti_sql   <- paste0(
        "CASE risk_grade\n",
        "           WHEN 'High' THEN ", fmt(params$pti_high), "\n",
        "           WHEN 'Med'  THEN ", fmt(params$pti_med), "\n",
        "           WHEN 'Low'  THEN ", fmt(params$pti_low), "\n",
        "           ELSE 0.20\n",
        "         END")
      seg_block <- paste0(
        "\n-- =============================================================\n",
        "-- SEGMENT OVERRIDE: Reprice loans matching CART segment\n",
        "-- =============================================================\n",
        "-- Segment condition: ", gsub("\n", "\n-- ", sql_where), "\n",
        "-- Pricing adjustment: +", bps, " basis points (+", apr_delta, ")\n",
        "--\n",
        "-- After changing APR, we must replay L50/L80/L90 so all\n",
        "-- downstream columns stay consistent:\n",
        "--   L50: 72-month APR cap, income_based_limit_cap, max_approved\n",
        "--   L80: offered_final_apr, affordability_limit_cap, max_offered\n",
        "--   L90: offered_decision_flag\n",
        "-- =============================================================\n",
        "\n",
        "-- NOTE: These UPDATEs require a staging table.\n",
        "-- CREATE TABLE policy_output AS <the query above without the trailing semicolon>;\n",
        "\n",
        "-- Stage 1: Bump policy APR (adjusted_apr_pre_term_cap) + recompute\n",
        "--          final_apr (72mo cap on term_months) and\n",
        "--          offered_final_apr (72mo cap on offered_term_months)\n",
        "UPDATE policy_output\n",
        "SET    adjusted_apr_pre_term_cap = adjusted_apr_pre_term_cap + ", apr_delta, ",\n",
        "       final_apr = CASE\n",
        "         WHEN term_months = 72\n",
        "           THEN LEAST(adjusted_apr_pre_term_cap + ", apr_delta, ", ", apr_cap, ")\n",
        "         ELSE adjusted_apr_pre_term_cap + ", apr_delta, "\n",
        "       END,\n",
        "       offered_final_apr = CASE\n",
        "         WHEN offered_term_months = 72\n",
        "           THEN LEAST(adjusted_apr_pre_term_cap + ", apr_delta, ", ", apr_cap, ")\n",
        "         ELSE adjusted_apr_pre_term_cap + ", apr_delta, "\n",
        "       END\n",
        "WHERE  offered_decision_flag = 'APPR'\n",
        "  AND  (", sql_where, ");\n",
        "\n",
        "-- Stage 2: Recompute L50 income_based_limit_cap + maximum_approved_amount\n",
        "UPDATE policy_output\n",
        "SET    income_based_limit_cap = (post_tax_income\n",
        "         - 12 * COALESCE(monthly_debt_payment, 0))\n",
        "         / NULLIF(1 - final_apr, 0),\n",
        "       maximum_approved_amount = LEAST(\n",
        "         (post_tax_income - 12 * COALESCE(monthly_debt_payment, 0))\n",
        "           / NULLIF(1 - final_apr, 0),\n",
        "         exposure_limit_cap)\n",
        "WHERE  offered_decision_flag = 'APPR'\n",
        "  AND  (", sql_where, ");\n",
        "\n",
        "-- Stage 3: Recompute L80 affordability (PV annuity using offered_final_apr\n",
        "--          and offered_term_months) + maximum_offered_amount\n",
        "UPDATE policy_output\n",
        "SET    max_new_installment = (", pti_sql, "\n",
        "         * post_tax_income / 12)\n",
        "         - COALESCE(monthly_debt_payment, 0)\n",
        "         - COALESCE(rent_est, 0),\n",
        "       affordability_limit_cap = CASE\n",
        "         WHEN (", pti_sql, "\n",
        "               * post_tax_income / 12)\n",
        "              - COALESCE(monthly_debt_payment, 0)\n",
        "              - COALESCE(rent_est, 0) <= 0\n",
        "           THEN 0\n",
        "         WHEN offered_final_apr = 0\n",
        "           THEN ((", pti_sql, "\n",
        "                  * post_tax_income / 12)\n",
        "                 - COALESCE(monthly_debt_payment, 0)\n",
        "                 - COALESCE(rent_est, 0))\n",
        "                * offered_term_months\n",
        "         ELSE\n",
        "           ((", pti_sql, "\n",
        "             * post_tax_income / 12)\n",
        "            - COALESCE(monthly_debt_payment, 0)\n",
        "            - COALESCE(rent_est, 0))\n",
        "           * (POWER(1 + offered_final_apr / 12,\n",
        "                    offered_term_months) - 1)\n",
        "           / ((offered_final_apr / 12)\n",
        "              * POWER(1 + offered_final_apr / 12,\n",
        "                      offered_term_months))\n",
        "       END,\n",
        "       maximum_offered_amount = LEAST(\n",
        "         maximum_approved_amount,\n",
        "         CASE\n",
        "           WHEN (", pti_sql, "\n",
        "                 * post_tax_income / 12)\n",
        "                - COALESCE(monthly_debt_payment, 0)\n",
        "                - COALESCE(rent_est, 0) <= 0\n",
        "             THEN 0\n",
        "           WHEN offered_final_apr = 0\n",
        "             THEN ((", pti_sql, "\n",
        "                    * post_tax_income / 12)\n",
        "                   - COALESCE(monthly_debt_payment, 0)\n",
        "                   - COALESCE(rent_est, 0))\n",
        "                  * offered_term_months\n",
        "           ELSE\n",
        "             ((", pti_sql, "\n",
        "               * post_tax_income / 12)\n",
        "              - COALESCE(monthly_debt_payment, 0)\n",
        "              - COALESCE(rent_est, 0))\n",
        "             * (POWER(1 + offered_final_apr / 12,\n",
        "                      offered_term_months) - 1)\n",
        "             / ((offered_final_apr / 12)\n",
        "                * POWER(1 + offered_final_apr / 12,\n",
        "                        offered_term_months))\n",
        "         END)\n",
        "WHERE  offered_decision_flag = 'APPR'\n",
        "  AND  (", sql_where, ");\n",
        "\n",
        "-- Stage 4: Re-evaluate L90 offered decision\n",
        "-- Loans whose offered amount dropped below $1 are declined\n",
        "UPDATE policy_output\n",
        "SET    offered_decision_flag = '90. Affordability',\n",
        "       issued_flag = 0\n",
        "WHERE  offered_decision_flag = 'APPR'\n",
        "  AND  maximum_offered_amount < 1\n",
        "  AND  (", sql_where, ");\n"
      )
    } else if (action == "Swap-In") {
      seg_block <- paste0(
        "\n-- =============================================================\n",
        "-- POLICY CHANGE: Swap-In (Relax Decline Threshold)\n",
        "-- =============================================================\n",
        "-- Changed parameters are reflected in L60 above.\n",
        "-- Previously declined applicants matching the relaxed\n",
        "-- thresholds are now eligible for approval.\n"
      )
    }
    # Note: Decline action has no seg_block — it's integrated into L90 above
  }

  paste0(hdr, "\nWITH\n", l10, l20a, l20b, l20c, l20d, l20e, l20f,
         l30, l40, l50a, l50b, l50, l60, l70a, l70, l80a, l80b, l80c, l80,
         l90, final_select, seg_block)
}


# ---- Helpers ----

# Format numeric with trailing zeros preserved
fmt <- function(x) {
  if (is.null(x) || is.na(x)) return("NULL")
  # Use enough decimal places to show the value clearly
  if (abs(x) < 1 && x != 0) {
    formatC(x, format = "f", digits = max(3, nchar(sub("^0?\\.0*", "", as.character(x)))))
  } else {
    formatC(x, format = "f", digits = 2, big.mark = "")
  }
}

# Convert R-style CART condition to SQL WHERE clause
r_condition_to_sql <- function(cond) {
  if (is.null(cond) || cond == "") return("1 = 1")
  # Validate condition contains only safe characters (prevent SQL injection)
  safe_pattern <- "^[a-zA-Z0-9_. <>!=&|()%\"'\\s,\\-+]+$"
  if (!grepl(safe_pattern, cond, perl = TRUE)) {
    return("1=0 /* unsafe condition filtered */")
  }
  sql <- cond
  # R logical operators -> SQL
  sql <- gsub("\\s*&\\s*", "\n        AND ", sql)
  sql <- gsub("\\s*\\|\\s*", "\n         OR ", sql)
  # R-style membership: %in% c(...) -> IN (...)
  sql <- gsub("%(in|IN)%\\s*c\\(([^)]+)\\)", "IN (\\2)", sql)
  # R-style string quotes -> SQL single quotes
  sql <- gsub('"', "'", sql)
  # >= and <= are already valid SQL
  sql
}

# Simplify redundant CART conditions
# e.g. "fico_score< 809.5 & fico_score< 749.5" -> "fico_score< 749.5"
# Keeps only the tightest bound per variable per direction.
simplify_cart_condition <- function(cond) {
  if (is.null(cond) || cond == "") return(cond)

  # Split on &
  parts <- trimws(strsplit(cond, "\\s*&\\s*")[[1]])
  if (length(parts) <= 1) return(cond)

  # Parse each term: variable, operator, value
  # Patterns: "var>=val", "var< val", "var = val", etc.
  pattern <- "^([A-Za-z_][A-Za-z0-9_.]*)\\s*(>=|<=|>|<)\\s*(.+)$"

  parsed <- list()
  other  <- character(0)  # terms we can't parse (e.g., %in%, ==)


  for (p in parts) {
    m <- regmatches(p, regexec(pattern, p))[[1]]
    if (length(m) == 4) {
      parsed[[length(parsed) + 1]] <- list(
        var = m[2], op = m[3], val = as.numeric(m[4]), raw = p
      )
    } else {
      other <- c(other, p)
    }
  }

  if (length(parsed) == 0) return(cond)

  # Group by (variable, direction)
  # Upper bounds: < and <=  -> keep smallest value
  # Lower bounds: > and >=  -> keep largest value
  keep <- list()
  for (term in parsed) {
    key <- paste0(term$var, "|", if (term$op %in% c("<", "<=")) "upper" else "lower")
    if (is.null(keep[[key]])) {
      keep[[key]] <- term
    } else {
      existing <- keep[[key]]
      if (term$op %in% c("<", "<=")) {
        # Upper bound: keep the smaller (tighter) value
        if (term$val < existing$val ||
            (term$val == existing$val && term$op == "<")) {
          keep[[key]] <- term
        }
      } else {
        # Lower bound: keep the larger (tighter) value
        if (term$val > existing$val ||
            (term$val == existing$val && term$op == ">")) {
          keep[[key]] <- term
        }
      }
    }
  }

  # Rebuild
  kept_terms <- vapply(keep, function(t) t$raw, character(1))
  all_terms  <- c(kept_terms, other)
  paste(all_terms, collapse = " & ")
}
