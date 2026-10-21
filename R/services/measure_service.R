# ============================================================
# R/services/measure_service.R
#
# Measurement + IRR service — feeds Portfolio Home and IRR Engine.
# Pure functions. No Shiny imports. No side effects beyond reading
# the passed arguments.
#
# Week 2 scope: Home + Catalog KPI functions.
# Week 3 adds: loan_cashflows, loan_xirr, load_curves.
# Week 5 adds: validate_tape_schema, load_tape, rebuild_cache.
# ============================================================

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# ---- filter_by_product ------------------------------------------------------
#' Filter a loan_data frame to a single product.
#' @param loan_data The full loan_data_global data.frame
#' @param product "Personal Loan" or "Top-Up" (or "all" for no filter)
filter_by_product <- function(loan_data, product = "Personal Loan - Prime") {
  if (is.null(loan_data) || nrow(loan_data) == 0) return(loan_data)
  if (identical(product, "all")) return(loan_data)
  if (!"product_type" %in% names(loan_data)) return(loan_data)
  # Both "Personal Loan - Prime" and "Personal Loan - Near Prime" are facades
  # over the same underlying "Personal Loan" tape — keep them in lockstep so
  # the demo can show multi-product coverage without splitting the data.
  if (product %in% c("Personal Loan - Prime", "Personal Loan - Near Prime")) {
    return(loan_data[loan_data$product_type == "Personal Loan", , drop = FALSE])
  }
  loan_data[loan_data$product_type == product, , drop = FALSE]
}

# ---- portfolio_kpis ---------------------------------------------------------
#' Headline metrics for Portfolio Home.
#'
#' @param loan_data Full loan data frame (must have offered_decision_flag,
#'                  issued_flag, performance_delinquency_flag, issued_amount)
#' @param perf_tape Performance tape list from build_perf_tape_v2, or NULL
#' @param product   Product filter ("Personal Loan", "Top-Up", or "all")
#' @param policy_id Optional policy_id filter (default = all policies within product)
#' @return named list: n_loans, approval_rate, portfolio_irr, avg_loss_rate,
#'                     at_risk_usd, policies_count
portfolio_kpis <- function(loan_data, perf_tape = NULL,
                           product = "Personal Loan",
                           policy_id = NULL) {
  df <- filter_by_product(loan_data, product)
  if (!is.null(policy_id) && "policy_id" %in% names(df)) {
    df <- df[df$policy_id == policy_id, , drop = FALSE]
  }

  n_total <- nrow(df)
  if (n_total == 0) {
    return(list(
      n_loans = 0, approval_rate = NA_real_, portfolio_irr = NA_real_,
      avg_loss_rate = NA_real_, at_risk_usd = NA_real_, policies_count = 0
    ))
  }

  # Approval rate = offered decisions that are APPR / all applications
  appr_mask <- !is.na(df$offered_decision_flag) & df$offered_decision_flag == "APPR"
  approval_rate <- sum(appr_mask) / n_total

  # Avg loss rate = delinquent issued loans / total issued
  issued_mask <- !is.na(df$issued_flag) & df$issued_flag == 1
  n_issued <- sum(issued_mask)
  avg_loss_rate <- if (n_issued > 0) {
    sum(df$performance_delinquency_flag[issued_mask] == 1, na.rm = TRUE) / n_issued
  } else {
    NA_real_
  }

  # $ at risk = sum of issued amounts for delinquent loans (proxy for EL)
  at_risk_usd <- if (n_issued > 0) {
    delinq_mask <- issued_mask &
      !is.na(df$performance_delinquency_flag) &
      df$performance_delinquency_flag == 1
    sum(df$issued_amount[delinq_mask], na.rm = TRUE)
  } else {
    NA_real_
  }

  # Portfolio IRR — needs perf tape; compute over issued loans of this product/policy
  portfolio_irr <- NA_real_
  if (!is.null(perf_tape) && !is.null(perf_tape$rows) &&
      exists("compute_portfolio_xirr_v2", mode = "function") &&
      n_issued > 0) {
    loan_ids <- as.character(df$loan_id[issued_mask])
    portfolio_irr <- tryCatch(
      compute_portfolio_xirr_v2(perf_tape, loan_ids = loan_ids),
      error = function(e) { message("cpt: portfolio_irr failed: ", conditionMessage(e)); NA_real_ }
    )
  }

  # Count of distinct policies represented in this product slice
  policies_count <- if ("policy_id" %in% names(df)) {
    length(unique(df$policy_id[!is.na(df$policy_id)]))
  } else {
    0L
  }

  list(
    n_loans        = n_total,
    approval_rate  = approval_rate,
    portfolio_irr  = portfolio_irr,
    avg_loss_rate  = avg_loss_rate,
    at_risk_usd    = at_risk_usd,
    policies_count = policies_count
  )
}

# ---- vintage_irr_series -----------------------------------------------------
#' Per-vintage IRR time series for the Home chart.
#'
#' @param perf_tape Performance tape list
#' @param loan_data Loan data frame (to join product filter on loan_id)
#' @param product   Product filter
#' @return data.frame(vintage, n_loans, irr, highlight) ordered by vintage
vintage_irr_series <- function(perf_tape, loan_data,
                               product = "Personal Loan") {
  if (is.null(perf_tape) || is.null(perf_tape$rows) ||
      is.null(perf_tape$vintages)) {
    return(data.frame(vintage = character(0), n_loans = integer(0),
                      irr = numeric(0), highlight = logical(0)))
  }
  if (!exists("compute_portfolio_xirr_v2", mode = "function")) {
    return(data.frame(vintage = character(0), n_loans = integer(0),
                      irr = numeric(0), highlight = logical(0)))
  }

  # Prefer loan_view for lookups — it's one row per loan and has both
  # vintage_quarter and product_type directly. Fall back to tape$rows if
  # loan_view is missing (older tape builds).
  lv <- perf_tape$loan_view
  has_lv <- !is.null(lv) && "vintage_quarter" %in% names(lv) &&
            "product_type" %in% names(lv)

  # Pre-filter to the right product once
  if (has_lv) {
    lv_prod <- lv[lv$product_type == product, , drop = FALSE]
  } else if ("vintage_quarter" %in% names(perf_tape$rows) &&
             "product_type" %in% names(perf_tape$rows)) {
    lv_prod <- unique(perf_tape$rows[perf_tape$rows$product_type == product,
                                     c("loan_id", "vintage_quarter"), drop = FALSE])
  } else {
    lv_prod <- NULL
  }

  vintages <- perf_tape$vintages

  rows <- lapply(vintages, function(vq) {
    if (is.null(lv_prod)) {
      vq_ids <- character(0)
    } else {
      vq_ids <- as.character(lv_prod$loan_id[lv_prod$vintage_quarter == vq])
    }
    n <- length(vq_ids)
    if (n == 0) {
      return(data.frame(vintage = vq, n_loans = 0L, irr = NA_real_, highlight = FALSE))
    }
    irr <- tryCatch(
      compute_portfolio_xirr_v2(perf_tape, loan_ids = vq_ids),
      error = function(e) NA_real_
    )
    data.frame(vintage = vq, n_loans = as.integer(n), irr = irr, highlight = FALSE)
  })
  out <- do.call(rbind, rows)

  # Highlight top-3 vintages by IRR
  if (nrow(out) > 0 && any(!is.na(out$irr))) {
    ranked <- order(out$irr, decreasing = TRUE, na.last = TRUE)
    out$highlight[ranked[seq_len(min(3, length(ranked)))]] <- TRUE
  }
  out
}

# ---- approval_funnel --------------------------------------------------------
#' Stages of the application funnel for the Home "Approval funnel" panel.
#'
#' @param loan_data Loan data frame
#' @param product   Product filter
#' @return data.frame(stage, count, pct)
approval_funnel <- function(loan_data, product = "Personal Loan") {
  df <- filter_by_product(loan_data, product)
  if (nrow(df) == 0) {
    return(data.frame(stage = character(0), count = integer(0), pct = numeric(0)))
  }

  stages <- c(
    "Applications"   = "application_received_flag",
    "Complete apps"  = "application_complete_flag",
    "Offers made"    = "offer_generated_flag",
    "Offers accepted"= "offer_accepted_flag",
    "Issued"         = "issued_flag"
  )

  counts <- vapply(stages, function(col) {
    if (col %in% names(df)) sum(df[[col]] == 1, na.rm = TRUE) else NA_integer_
  }, integer(1))

  total_apps <- counts[["Applications"]]
  if (is.na(total_apps) || total_apps == 0) total_apps <- nrow(df)

  data.frame(
    stage = names(stages),
    count = as.integer(counts),
    pct   = as.numeric(counts) / as.numeric(total_apps),
    stringsAsFactors = FALSE
  )
}

# ---- recent_policy_changes --------------------------------------------------
#' Last N policy changes for the Home "Recent changes" panel.
#'
#' Sorted by effective_from descending, ties broken by policy ID order.
#'
#' @param strategy_overrides The loaded manifest list
#' @param product             Filter (NULL = all products)
#' @param n                   Max rows (default 5)
#' @return data.frame(policy_id, name, product, status, effective_from)
recent_policy_changes <- function(strategy_overrides, product = NULL, n = 5) {
  if (length(strategy_overrides) == 0) {
    return(data.frame(policy_id = character(0), name = character(0),
                      product = character(0), status = character(0),
                      effective_from = as.Date(character(0))))
  }
  rows <- lapply(names(strategy_overrides), function(pid) {
    s <- strategy_overrides[[pid]]
    data.frame(
      policy_id      = pid,
      name           = s$name %||% pid,
      product        = s$product %||% "Unknown",
      status         = s$status %||% "unknown",
      effective_from = suppressWarnings(as.Date(s$effective_from %||% NA)),
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, rows)

  if (!is.null(product)) df <- df[df$product == product, , drop = FALSE]
  df <- df[order(df$effective_from, decreasing = TRUE, na.last = TRUE), , drop = FALSE]
  head(df, n)
}

# ---- top_flagged_segments ---------------------------------------------------
#' Map a scan result's top segments to a data frame the Home panel renders.
#'
#' Week 3: now takes the scan_result list from discover_service::auto_scan.
#' Week 2 placeholder signature (no arg) is preserved via the default.
top_flagged_segments <- function(scan_result = NULL, top = 3) {
  empty <- data.frame(rule_text = character(0), n = integer(0),
                      irr = numeric(0), loss = numeric(0),
                      status = character(0), stringsAsFactors = FALSE)
  if (is.null(scan_result) || length(scan_result$segments) == 0) return(empty)

  segs <- head(scan_result$segments, top)
  data.frame(
    rule_text = vapply(segs, function(s) s$display_rule %||% s$rule, character(1)),
    n         = vapply(segs, function(s) as.integer(s$n %||% 0L), integer(1)),
    irr       = vapply(segs, function(s) as.numeric(s$mean_irr %||% NA_real_), numeric(1)),
    loss      = vapply(segs, function(s) as.numeric(s$bad_rate %||% NA_real_), numeric(1)),
    status    = rep("alert", length(segs)),
    stringsAsFactors = FALSE
  )
}

# ---- loan_cashflows (single-loan walkthrough in IRR Engine) ----------------
#' Cashflow rows for a single loan, pulled from the performance tape.
#'
#' The performance tape ALREADY contains the disbursement at
#' `month_on_books == 0` as a negative `adj_cf` (verified against the
#' loan_irr cache: passing tape rows directly to xirr() reproduces the
#' cached IRR exactly). We pass them through without prepending another
#' synthetic disbursement row, which previously double-counted and
#' produced wildly negative XIRR values.
#'
#' @return data.frame(month, date, principal, interest, co_amount, cashflow)
loan_cashflows <- function(loan_id, perf_tape) {
  if (is.null(perf_tape) || is.null(perf_tape$rows) || is.null(loan_id)) return(NULL)
  lid <- as.character(loan_id)
  rows <- perf_tape$rows[as.character(perf_tape$rows$loan_id) == lid, , drop = FALSE]
  if (nrow(rows) == 0) return(NULL)
  rows <- rows[order(rows$month_on_books), , drop = FALSE]
  first_row <- rows[1, ]

  # `adj_cf` is the post-projection cashflow from build_perf_tape_v2.
  # Negative on the disbursement row (mob=0), positive on payment rows.
  cf_col <- if ("adj_cf" %in% names(rows)) "adj_cf" else "cashflows"

  # The principal column gets the disbursement marker on the mob=0 row,
  # zero on the payment rows — used for the waterfall display.
  principal <- ifelse(rows$month_on_books == 0,
                      -(first_row$issued_amount %||% 0), 0)

  data.frame(
    month       = rows$month_on_books,
    date        = rows$payment_date,
    principal   = principal,
    interest    = rows$interest_payment %||% rep(0, nrow(rows)),
    co_amount   = rows$co_amount %||% rep(0, nrow(rows)),
    cashflow    = rows[[cf_col]],
    stringsAsFactors = FALSE
  )
}

#' Compute XIRR from a cashflow data frame.
loan_xirr <- function(cashflows) {
  if (is.null(cashflows) || nrow(cashflows) < 2) return(NA_real_)
  if (!exists("xirr", mode = "function")) return(NA_real_)
  tryCatch(xirr(cashflows$cashflow, cashflows$date), error = function(e) NA_real_)
}

#' Return the DCOR/DPOR curves + product params from the tape for display.
load_curves <- function(perf_tape) {
  if (is.null(perf_tape)) return(NULL)
  list(
    curves         = perf_tape$curves,
    product_params = perf_tape$params,
    products       = perf_tape$products %||% character(0)
  )
}

# ---- benchmark_for ----------------------------------------------------------
#' Industry benchmark subtext for each KPI (Addition 7 — benchmark context).
#'
#' Hardcoded defaults for Week 2. Week 5.5 replaces with a config load
#' from ../consultant_assumptions.csv.
#'
#' @param kpi One of "approval_rate", "portfolio_irr", "avg_loss_rate", "at_risk_usd"
#' @return character(1) benchmark text or NULL
benchmark_for <- function(kpi) {
  switch(kpi,
    approval_rate = "vs. industry 62.0%",
    portfolio_irr = "vs. industry 11.2%",
    avg_loss_rate = "vs. industry 4.1%",
    at_risk_usd   = "peer median $3.8M",
    NULL
  )
}

# ---- load_consultant_config ------------------------------------------------
#' Load consultant assumptions from CSV and parse into a config list.
#'
#' Looks for inst/data/consultant_assumptions.csv. Returns the parsed config
#' list or NULL if the file is not found / unparseable.
#'
#' @param config_path Path to the CSV (default: "inst/data/consultant_assumptions.csv")
#' @return list(elasticity, adverse_selection, anchor_fico_floor, donor_product, donor_stress) or NULL
load_consultant_config <- function(config_path = "inst/data/consultant_assumptions.csv") {
  cfg <- if (file.exists(config_path)) {
    tryCatch(readr::read_csv(config_path, show_col_types = FALSE),
             error = function(e) {
               message("WARNING: Failed to parse consultant_assumptions.csv: ", conditionMessage(e))
               NULL
             })
  } else NULL

  if (is.null(cfg) || nrow(cfg) == 0) {
    message("WARNING: consultant_assumptions.csv not found or invalid - using hardcoded defaults")
    return(list(
      elasticity        = c(High = 3, Med = 5, Low = 8),
      adverse_selection = c(High = 2, Med = 5, Low = 8),
      anchor_fico_floor = 760,
      donor_product     = character(0),
      donor_stress      = numeric(0)
    ))
  }

  # Parse donor product mappings: parameter=="donor_product", grade=target, value=donor
  donor_rows <- cfg[cfg$parameter == "donor_product", ]
  donor_map <- if (nrow(donor_rows) > 0) {
    setNames(as.character(donor_rows$value), donor_rows$grade)
  } else {
    character(0)
  }

  # Parse donor stress factors: parameter=="donor_stress", grade=target, value=multiplier
  stress_rows <- cfg[cfg$parameter == "donor_stress", ]
  stress_map <- if (nrow(stress_rows) > 0) {
    setNames(as.numeric(stress_rows$value), stress_rows$grade)
  } else {
    numeric(0)
  }

  list(
    elasticity        = setNames(cfg$value[cfg$parameter == "elasticity"],
                                 cfg$grade[cfg$parameter == "elasticity"]),
    adverse_selection = setNames(cfg$value[cfg$parameter == "adverse_selection"],
                                 cfg$grade[cfg$parameter == "adverse_selection"]),
    anchor_fico_floor = cfg$value[cfg$parameter == "anchor_fico_floor"][1],
    donor_product     = donor_map,
    donor_stress      = stress_map
  )
}
