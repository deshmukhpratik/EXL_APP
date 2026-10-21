# ============================================================================
# irr_cashflow_engine.R
# Standalone IRR / cashflow projection engine
# Extracted from app.R — build_perf_tape() + XIRR helpers
#
# DEPENDENCY: dplyr (>= 1.0)
# SEE ALSO: irr_cashflow_engine_v2.R — DCOR/DPOR curve-based projections
#           (compute_portfolio_xirr_v2)
#
# USAGE:
#   source("irr_cashflow_engine.R")
#   tape_raw <- read.csv("irr_performance_tape_synthetic_mob_asof_2026_02_28.csv",
#                         stringsAsFactors = FALSE, check.names = FALSE)
#   tape <- build_perf_tape(tape_raw)
#   # tape$rows      — loan-level monthly rows with adj_cf, ever_del
#   # tape$loan_view — one-row-per-loan summary with fico_band, first_dq_date
#   # tape$max_date  — observation cut-off date
#   # tape$vintages  — sorted vintage quarter labels
#   # tape$products  — sorted product types
#
#   # Portfolio-level XIRR:
#   port <- compute_portfolio_xirr(tape)
#
#   # Grouped XIRR (by fico_band, partner_channel, etc.):
#   by_fico <- compute_grouped_xirr(tape, group_col = "fico_band")
#
# ============================================================================
# DIAGNOSIS — issues found in original app.R code (lines 200-418)
# ============================================================================
#
# [BUG-1] Terminal value formula subtracts cumulative P+I, not just principal
#   Original (line 388):
#     terminal_value = pmax(0, tv_issued - last_scheduled - cum_co)
#   `schedule_payment` is PER-PERIOD (monthly P+I, not cumulative).  We sum()
#   across all observed months to get total principal paid, then subtract from
#   issued_amount.  The [month_on_books > 0] filter excludes MOB 0 (disbursement
#   row) where the generator writes non-zero values that don't represent payments.
#
# [BUG-2] Pre-payments omitted from terminal value
#   Pre-payments reduce outstanding principal but are not subtracted.
#   `pre_payments` is a per-period column in the tape.  sum(pre_payments) must
#   be deducted alongside scheduled principal.
#   FIX: terminal_value = issued - cum_sched_principal - cum_prepay - cum_co.
#
# [BUG-3] `schedule_payment` missing from required_cols validation
#   build_perf_tape() validates 15 required columns (line 288-294) but does NOT
#   include `schedule_payment` or `interest_payment`, both of which are used at
#   line 384.  A tape missing these columns produces a cryptic R error instead
#   of the friendly "Missing required columns" message.
#   FIX: add schedule_payment and interest_payment to required_cols.
#
# [BUG-4] first_dq_date for flag-only delinquencies uses loan's first payment
#   Lines 360-369: for loans with performance_delinquency_flag=1 but zero
#   co_amount, the code sets first_dq_date = min(payment_date) — the loan's
#   EARLIEST payment date, not the first date the flag actually turned on.
#   FIX: filter to rows where flag=1 first, then take min(payment_date).
#
# [PERF-1] Redundant ever_del recomputation
#   build_perf_tape() pre-computes ever_del (line 398-402), but the
#   cumulativeView renderer (line 3380-3384) recomputes the exact same
#   arrange/group_by/cummax chain on tape$rows.
#   FIX: consumer code should use the pre-computed ever_del column.
#
# [MINOR-1] xnpv uses 365-day year (not 365.25 or actual/actual)
#   Standard for most financial software; acceptable but worth noting.
#
# [MINOR-2] loan_static uses first() without prior arrange()
#   Lines 329-341: summarise(... = first(...)) picks whichever row appears
#   first in the dataframe.  If the CSV is not sorted by loan_id, the
#   value depends on input ordering.  Harmless when per-loan values are
#   truly constant, but fragile.
#   FIX: add arrange(loan_id, month_on_books) before the summarise.
#
# ============================================================================

library(dplyr)

# ---------------------------------------------------------------------------
# XNPV / XIRR
# ---------------------------------------------------------------------------

xnpv <- function(rate, cashflows, dates) {
  t0    <- min(dates)
  years <- as.numeric(difftime(dates, t0, units = "days")) / 365
  sum(cashflows / ((1 + rate) ^ years))
}

xirr <- function(cashflows, dates) {
  if (length(cashflows) < 2) return(NA_real_)
  if (!(any(cashflows > 0) && any(cashflows < 0))) return(NA_real_)

  f   <- function(r) xnpv(r, cashflows, dates)
  lo  <- -0.9999
  hi  <- 10
  flo <- f(lo)
  fhi <- f(hi)

  if (!is.finite(flo) || !is.finite(fhi)) return(NA_real_)

  # Widen upper bound if no sign change yet

  if (flo * fhi > 0) {
    for (cand_hi in c(20, 50, 100)) {
      fhi <- f(cand_hi)
      if (is.finite(fhi) && flo * fhi <= 0) {
        hi <- cand_hi
        break
      }
    }
    if (flo * fhi > 0) return(NA_real_)
  }

  tryCatch(uniroot(f, lower = lo, upper = hi)$root,
           error = function(e) NA_real_)
}

# ---------------------------------------------------------------------------
# Date utilities
# ---------------------------------------------------------------------------

add_months_base <- function(dt, months_n) {
  y <- as.integer(format(dt, "%Y"))
  m <- as.integer(format(dt, "%m"))
  d <- as.integer(format(dt, "%d"))
  new_y <- y + (m - 1 + months_n) %/% 12
  new_m <- ((m - 1 + months_n) %% 12) + 1
  first_next <- as.Date(sprintf("%04d-%02d-01", new_y, new_m))
  if (new_m == 12) {
    month_after <- as.Date(sprintf("%04d-01-01", new_y + 1))
  } else {
    month_after <- as.Date(sprintf("%04d-%02d-01", new_y, new_m + 1))
  }
  max_day <- as.integer(month_after - first_next)
  as.Date(sprintf("%04d-%02d-%02d", new_y, new_m, min(d, max_day)))
}

floor_month_base <- function(dt) {
  as.Date(format(dt, "%Y-%m-01"))
}

floor_quarter_base <- function(dt) {
  y <- as.integer(format(dt, "%Y"))
  m <- as.integer(format(dt, "%m"))
  q_start_month <- ((m - 1) %/% 3) * 3 + 1
  as.Date(sprintf("%04d-%02d-01", y, q_start_month))
}

quarter_end_base <- function(dt) {
  q_start <- floor_quarter_base(dt)
  add_months_base(q_start, 3) - 1
}

vintage_quarter_label <- function(dt) {
  y <- as.integer(format(dt, "%Y"))
  m <- as.integer(format(dt, "%m"))
  # Half-yearly for pre-2025 (reduces chart clutter), quarterly for 2025+
  ifelse(y >= 2025,
    paste0(y, "-Q", (m - 1) %/% 3 + 1),
    paste0(y, "-H", ifelse(m <= 6, 1, 2))
  )
}

quarter_end_seq <- function(max_date, months_back = 12) {
  start_date <- add_months_base(max_date, -(months_back - 1))
  q_start    <- floor_quarter_base(start_date)
  q_points   <- c()
  cur <- q_start
  while (cur <= max_date) {
    q_end <- quarter_end_base(cur)
    if (q_end <= max_date) q_points <- c(q_points, q_end)
    cur <- add_months_base(cur, 3)
  }
  as.Date(q_points, origin = "1970-01-01")
}

# ---------------------------------------------------------------------------
# build_perf_tape()  —  with BUG-1 through BUG-4 fixed
# ---------------------------------------------------------------------------

build_perf_tape <- function(df_raw) {

  # --- Column validation (BUG-3 fix: added schedule_payment, interest_payment)
  required_cols <- c(
    "loan_id", "issued_flag", "issued_amount", "booking_vintage",
    "product_type", "partner_channel", "borrower_repeat_type",
    "performance_delinquency_flag", "co_amount", "payment_date",
    "cashflows", "interest_rate_apr", "fico_score", "month_on_books",
    "term_months", "schedule_payment", "interest_payment",
    "pre_payments"
  )
  names(df_raw) <- trimws(names(df_raw))
  miss <- setdiff(required_cols, names(df_raw))
  if (length(miss) > 0) {
    stop(sprintf("Missing required columns: %s", paste(miss, collapse = ", ")))
  }

  # --- Type coercion & derived fields
  df <- df_raw %>%
    mutate(
      issued_flag    = as.integer(issued_flag),
      issued_amount  = as.numeric(issued_amount),
      co_amount      = as.numeric(co_amount),
      cashflows      = as.numeric(cashflows),
      interest_rate_apr   = as.numeric(interest_rate_apr),
      fico_score     = as.numeric(fico_score),
      month_on_books = as.integer(month_on_books),
      term_months    = as.integer(term_months),
      schedule_payment  = as.numeric(schedule_payment),
      interest_payment  = as.numeric(interest_payment),
      pre_payments      = as.numeric(pre_payments),
      payment_date   = as.Date(payment_date),
      booking_vintage_date = suppressWarnings(
        as.Date(booking_vintage, format = "%d/%m/%Y")
      )
    ) %>%
    mutate(
      booking_vintage_date = if_else(
        is.na(booking_vintage_date),
        suppressWarnings(as.Date(booking_vintage, format = "%d-%m-%Y")),
        booking_vintage_date
      ),
      vintage_quarter = vintage_quarter_label(booking_vintage_date),
      payment_month   = floor_month_base(payment_date)
    ) %>%
    filter(issued_flag == 1)

  if (nrow(df) == 0) stop("No issued records found after filtering issued_flag == 1.")
  if (anyNA(df$payment_date)) stop("payment_date contains invalid values.")
  if (anyNA(df$booking_vintage_date)) stop("booking_vintage contains invalid values.")

  # --- Loan-level statics (MINOR-2 fix: explicit arrange before first())
  loan_static <- df %>%
    arrange(loan_id, month_on_books) %>%
    group_by(loan_id) %>%
    summarise(
      vintage_quarter     = first(vintage_quarter),
      product_type        = first(product_type),
      partner_channel     = first(partner_channel),
      borrower_repeat_type = first(borrower_repeat_type),
      issued_amount       = first(issued_amount),
      interest_rate_apr   = first(interest_rate_apr),
      fico_score          = first(fico_score),
      term_months         = first(term_months),
      .groups = "drop"
    ) %>%
    mutate(
      fico_band = case_when(
        fico_score <  600 ~ "<600",
        fico_score <  650 ~ "600-649",
        fico_score <  700 ~ "650-699",
        fico_score <  750 ~ "700-749",
        fico_score >= 750 ~ "750+",
        TRUE              ~ "Unknown"
      )
    )

  # --- First delinquency date
  first_dq_from_co <- df %>%
    filter(co_amount > 0) %>%
    group_by(loan_id) %>%
    summarise(first_dq_date = min(payment_date), .groups = "drop")

  # BUG-4 fix: filter to rows where the flag is actually 1 before taking min()
  flagged_rows <- df %>%
    filter(performance_delinquency_flag == 1) %>%
    anti_join(first_dq_from_co, by = "loan_id")

  if (nrow(flagged_rows) > 0) {
    flagged_no_co <- flagged_rows %>%
      group_by(loan_id) %>%
      summarise(first_dq_date = min(payment_date), .groups = "drop")
    first_dq <- bind_rows(first_dq_from_co, flagged_no_co)
  } else {
    first_dq <- first_dq_from_co
  }
  loan_view <- loan_static %>% left_join(first_dq, by = "loan_id")

  # --- Terminal value & adjusted cashflows for XIRR
  #     BUG-1 fix: subtract cumulative principal, not cumulative P+I
  #     BUG-2 fix: include cumulative pre-payments in the deduction
  loan_tv <- df %>%
    arrange(loan_id, month_on_books) %>%
    group_by(loan_id) %>%
    summarise(
      tv_issued      = first(issued_amount),
      last_date      = max(payment_date),
      last_sched_cum = sum(schedule_payment[month_on_books > 0], na.rm = TRUE),   # sum of per-period P+I
      last_int_cum   = sum(interest_payment[month_on_books > 0], na.rm = TRUE),   # sum of per-period interest
      cum_prepay     = sum(pre_payments, na.rm = TRUE),  # per-period, so sum
      cum_co         = sum(co_amount, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      cum_principal  = last_sched_cum - last_int_cum,          # scheduled principal only
      terminal_value = pmax(0, tv_issued - cum_principal - cum_prepay - cum_co)
    )

  df <- df %>%
    left_join(loan_tv %>% select(loan_id, terminal_value, last_date),
              by = "loan_id") %>%
    mutate(adj_cf = ifelse(payment_date == last_date,
                           cashflows + terminal_value, cashflows)) %>%
    select(-last_date, -terminal_value)

  # --- Pre-compute cumulative delinquency flag (ever_del)
  df <- df %>%
    arrange(loan_id, month_on_books) %>%
    group_by(loan_id) %>%
    mutate(ever_del = cummax(performance_delinquency_flag)) %>%
    ungroup()

  # --- Join fico_band into row-level data for fast grouped XIRR
  df <- df %>%
    left_join(loan_static %>% select(loan_id, fico_band), by = "loan_id")

  df$loan_id <- as.character(df$loan_id)

  list(
    rows     = df,
    loan_view = loan_view,
    max_date  = max(df$payment_date, na.rm = TRUE),
    vintages  = sort(unique(loan_view$vintage_quarter)),
    products  = sort(unique(loan_view$product_type)),
    segment_fields = c("partner_channel", "borrower_repeat_type")
  )
}

# ---------------------------------------------------------------------------
# Convenience XIRR wrappers (extracted from app.R render blocks)
# ---------------------------------------------------------------------------

#' Compute portfolio-level XIRR from a tape object.
#' @param tape  Output of build_perf_tape().
#' @param loan_ids  Optional character vector of loan IDs to subset.
#'                  If NULL (default), all loans in the tape are used.
#' @return Scalar numeric XIRR (annualised), or NA_real_ on failure.
compute_portfolio_xirr <- function(tape, loan_ids = NULL) {
  rows <- tape$rows
  if (!is.null(loan_ids)) {
    rows <- rows[rows$loan_id %in% as.character(loan_ids), ]
  }
  if (nrow(rows) == 0) return(NA_real_)

  port <- rows %>%
    group_by(payment_date) %>%
    summarise(cf = sum(adj_cf, na.rm = TRUE), .groups = "drop") %>%
    arrange(payment_date)

  tryCatch(xirr(port$cf, port$payment_date), error = function(e) NA_real_)
}

#' Compute XIRR broken out by a grouping column.
#' @param tape       Output of build_perf_tape().
#' @param group_col  Column name in tape$rows to group by
#'                   (e.g. "fico_band", "partner_channel", "vintage_quarter").
#' @param loan_ids   Optional character vector of loan IDs to subset.
#' @return Data frame with columns: group, xirr_value.
compute_grouped_xirr <- function(tape, group_col, loan_ids = NULL) {
  rows <- tape$rows
  if (!is.null(loan_ids)) {
    rows <- rows[rows$loan_id %in% as.character(loan_ids), ]
  }
  if (nrow(rows) == 0 || !group_col %in% names(rows)) {
    return(data.frame(group = character(), xirr_value = numeric(),
                      stringsAsFactors = FALSE))
  }

  rows %>%
    filter(!is.na(.data[[group_col]])) %>%
    group_by(grp = .data[[group_col]]) %>%
    group_modify(~ {
      port <- .x %>%
        group_by(payment_date) %>%
        summarise(cf = sum(adj_cf, na.rm = TRUE), .groups = "drop") %>%
        arrange(payment_date)
      data.frame(xirr_value = tryCatch(xirr(port$cf, port$payment_date),
                                       error = function(e) NA_real_))
    }) %>%
    ungroup() %>%
    transmute(group = grp, xirr_value)
}

# ---------------------------------------------------------------------------
# Loan-level XIRR computation
# ---------------------------------------------------------------------------

#' Compute per-loan XIRR from the performance tape.
#' @param tape  Output of build_perf_tape().
#' @return data.frame with columns: loan_id (character), loan_irr (numeric).
compute_loan_irr <- function(tape) {
  rows <- tape$rows
  if (nrow(rows) == 0) {
    return(data.frame(loan_id = character(), loan_irr = numeric(),
                      stringsAsFactors = FALSE))
  }

  # Pre-aggregate cashflows by loan_id + payment_date for speed
  agg <- rows[, c("loan_id", "payment_date", "adj_cf")]
  agg <- agg[order(agg$loan_id, agg$payment_date), ]
  agg <- aggregate(adj_cf ~ loan_id + payment_date, data = agg, FUN = sum)
  agg <- agg[order(agg$loan_id, agg$payment_date), ]

  loan_ids <- unique(agg$loan_id)
  irr_vals <- vapply(split(agg, agg$loan_id), function(port) {
    tryCatch(xirr(port$adj_cf, port$payment_date),
             error = function(e) NA_real_)
  }, numeric(1))

  data.frame(loan_id = names(irr_vals), loan_irr = unname(irr_vals),
             stringsAsFactors = FALSE)
}

# ---------------------------------------------------------------------------
# Neighbor band metrics for Swap-In analysis
# ---------------------------------------------------------------------------

#' For a given decline reason, find issued loans in the neighbor band just
#' above the cutoff and compute their observed performance metrics.
#'
#' @param tape           Output of build_perf_tape().
#' @param credit_data    Full application-level data.frame with all columns.
#' @param decline_reason One of "01. Fico", "02. Thin file",
#'                       "03. Delinquency decline", "04. HighDTI",
#'                       "05. HardInquiry".
#' @param threshold      The current policy threshold for this rule. For
#'                       HardInquiry, inferred from data if NULL.
#' @param neighbor_width How far above the cutoff to look (optional).
#' @return Named list: n_loans, observed_irr, delinq_rate,
#'         avg_months_to_default, avg_lgd, avg_amount, avg_apr, avg_term,
#'         neighbor_loan_ids.
get_neighbor_metrics <- function(tape, credit_data, decline_reason,
                                  threshold, neighbor_width = NULL) {

  # Identify the neighbor band based on decline reason
  issued <- credit_data[credit_data$issued_flag == 1 &
    !is.na(credit_data$offered_decision_flag) &
    credit_data$offered_decision_flag == "APPR", ]

  if (decline_reason == "01. Fico") {
    width <- if (!is.null(neighbor_width)) neighbor_width else 40
    neighbor <- issued[!is.na(issued$fico_score) &
      issued$fico_score >= threshold &
      issued$fico_score < (threshold + width), ]
  } else if (decline_reason == "02. Thin file") {
    width <- if (!is.null(neighbor_width)) neighbor_width else 18
    neighbor <- issued[!is.na(issued$credit_file_age_months) &
      issued$credit_file_age_months > threshold &
      issued$credit_file_age_months <= (threshold + width), ]
  } else if (decline_reason == "03. Delinquency decline") {
    # Neighbor = loans at exactly the max allowed delinquency count
    neighbor <- issued[!is.na(issued$delinquent_accounts_count) &
      issued$delinquent_accounts_count == threshold, ]
  } else if (decline_reason == "04. HighDTI") {
    width <- if (!is.null(neighbor_width)) neighbor_width else 0.10
    neighbor <- issued[!is.na(issued$debt_to_income_ratio) &
      issued$debt_to_income_ratio <= threshold &
      issued$debt_to_income_ratio > (threshold - width), ]
  } else if (decline_reason == "05. HardInquiry") {
    # Neighbor = issued loans at inquiry count = threshold - 1 (highest count
    # still approved). If threshold is missing or inactive (>=99), default to
    # 4 — the standard industry cutoff that produces a demo-sized neighbor band
    # (2,000+ loans at the 3-inquiry boundary).
    if (is.null(threshold) || is.na(threshold) || threshold >= 99) {
      threshold <- 4L
    }
    neighbor <- issued[!is.na(issued$N_HARD_INQ_6M) &
      issued$N_HARD_INQ_6M == (threshold - 1L), ]
  } else {
    return(list(n_loans = 0, observed_irr = NA_real_, delinq_rate = NA_real_,
                avg_months_to_default = 12, avg_lgd = 0.60,
                avg_amount = NA_real_, avg_apr = NA_real_, avg_term = NA_real_,
                neighbor_loan_ids = character()))
  }

  n <- nrow(neighbor)
  if (n == 0) {
    return(list(n_loans = 0, observed_irr = NA_real_, delinq_rate = NA_real_,
                avg_months_to_default = 12, avg_lgd = 0.60,
                avg_amount = NA_real_, avg_apr = NA_real_, avg_term = NA_real_,
                neighbor_loan_ids = character()))
  }

  ids <- as.character(neighbor$loan_id)

  # Observed IRR from tape
  obs_irr <- compute_portfolio_xirr_v2(tape, loan_ids = ids)

  # Delinquency rate
  delinq_rate <- mean(neighbor$performance_delinquency_flag, na.rm = TRUE)

  # Avg months to default from tape$loan_view
  lv <- tape$loan_view
  lv_dq <- lv[lv$loan_id %in% ids & !is.na(lv$first_dq_date), ]
  if (nrow(lv_dq) > 0) {
    # Join booking date from tape rows
    booking_dates <- tape$rows %>%
      filter(loan_id %in% lv_dq$loan_id) %>%
      group_by(loan_id) %>%
      summarise(booking_date = min(payment_date) - 30, .groups = "drop")
    lv_dq <- merge(lv_dq, booking_dates, by = "loan_id")
    avg_mtd <- mean(as.numeric(difftime(lv_dq$first_dq_date,
      lv_dq$booking_date, units = "days")) / 30.44, na.rm = TRUE)
    avg_mtd <- max(avg_mtd, 3)  # floor at 3 months
  } else {
    avg_mtd <- 12
  }

  # Average LGD (chargeoff amount / issued amount for defaulted loans)
  tape_dq <- tape$rows[tape$rows$loan_id %in% ids, ]
  total_co <- sum(tape_dq$co_amount, na.rm = TRUE)
  total_issued <- sum(neighbor$issued_amount, na.rm = TRUE)
  avg_lgd <- if (total_issued > 0 && delinq_rate > 0) {
    min(total_co / (total_issued * delinq_rate), 1.0)
  } else {
    0.60
  }

  list(
    n_loans               = n,
    observed_irr          = obs_irr,
    delinq_rate           = delinq_rate,
    avg_months_to_default = avg_mtd,
    avg_lgd               = avg_lgd,
    avg_amount            = mean(neighbor$issued_amount, na.rm = TRUE),
    avg_apr               = mean(neighbor$interest_rate_apr, na.rm = TRUE),
    avg_term              = mean(neighbor$term_months, na.rm = TRUE),
    neighbor_loan_ids     = ids
  )
}

# ---------------------------------------------------------------------------
# Synthetic cashflow builder for Swap-In IRR projection
# ---------------------------------------------------------------------------

#' Build a projected portfolio-level cashflow series for a swap-in cohort
#' using a constant hazard model for defaults.
#'
#' @param n_loans              Number of loans in the swap-in cohort.
#' @param avg_amount           Average issued amount per loan.
#' @param apr                  Annual percentage rate (decimal, e.g. 0.16).
#' @param term                 Loan term in months.
#' @param delinq_rate          Assumed cumulative delinquency rate (0 to 1).
#' @param avg_months_to_default Average months until default for those who do.
#' @param observation_months   Observation window in months (default 36).
#' @param lgd                  Loss given default as fraction (default 0.60).
#' @param origination_date     Date of origination for XIRR dating.
#' @return data.frame with columns: date, cashflow, performing_cf, loss_cf.
build_synthetic_cashflows <- function(n_loans, avg_amount, apr, term,
                                      delinq_rate, avg_months_to_default,
                                      observation_months = 36,
                                      lgd = 0.60,
                                      origination_date = as.Date("2025-01-15")) {

  if (n_loans <= 0 || avg_amount <= 0 || apr <= 0 || term <= 0) {
    return(data.frame(date = origination_date, cashflow = 0,
                      performing_cf = 0, loss_cf = 0))
  }

  monthly_rate <- apr / 12
  # Standard amortisation payment
  pmt <- avg_amount * monthly_rate * (1 + monthly_rate)^term /
    ((1 + monthly_rate)^term - 1)

  # Constant hazard model
  # If delinq_rate is 0, no defaults at all
  if (delinq_rate <= 0 || avg_months_to_default <= 0) {
    monthly_hazard <- 0
  } else {
    # Clamp delinq_rate to avoid log(0)
    dr <- min(delinq_rate, 0.9999)
    monthly_hazard <- -log(1 - dr) / avg_months_to_default
  }

  obs <- min(term, observation_months)
  dates <- numeric(obs + 1)
  cfs   <- numeric(obs + 1)
  perf_cfs <- numeric(obs + 1)
  loss_cfs <- numeric(obs + 1)

  # Month 0: disbursement
  dates[1] <- as.numeric(origination_date)
  cfs[1]   <- -n_loans * avg_amount
  perf_cfs[1] <- -n_loans * avg_amount
  loss_cfs[1] <- 0

  for (t in seq_len(obs)) {
    dt <- as.numeric(add_months_base(origination_date, t))
    dates[t + 1] <- dt

    # Survival probability at month t
    survival_t <- exp(-monthly_hazard * t)

    # Expected performing cashflow
    performing <- n_loans * pmt * survival_t

    # Default density at month t (new defaults this month)
    # default_density = hazard * survival(t-1)
    survival_prev <- exp(-monthly_hazard * (t - 1))
    defaults_this_month <- n_loans * monthly_hazard * survival_prev

    # Outstanding balance approximation at month t
    # (fraction of principal remaining on a standard amortisation)
    if (monthly_rate > 0) {
      balance_fraction <- ((1 + monthly_rate)^term - (1 + monthly_rate)^t) /
        ((1 + monthly_rate)^term - 1)
    } else {
      balance_fraction <- (term - t) / term
    }
    balance_fraction <- max(balance_fraction, 0)

    # Loss this month
    loss <- defaults_this_month * avg_amount * balance_fraction * lgd

    perf_cfs[t + 1] <- performing
    loss_cfs[t + 1] <- -loss
    cfs[t + 1]      <- performing - loss
  }

  # Terminal value: surviving loans' remaining balance
  survival_final <- exp(-monthly_hazard * obs)
  if (obs < term) {
    if (monthly_rate > 0) {
      remaining_frac <- ((1 + monthly_rate)^term - (1 + monthly_rate)^obs) /
        ((1 + monthly_rate)^term - 1)
    } else {
      remaining_frac <- (term - obs) / term
    }
    remaining_frac <- max(remaining_frac, 0)
    terminal_value <- n_loans * survival_final * avg_amount * remaining_frac
    cfs[obs + 1]      <- cfs[obs + 1] + terminal_value
    perf_cfs[obs + 1] <- perf_cfs[obs + 1] + terminal_value
  }

  data.frame(
    date          = as.Date(dates, origin = "1970-01-01"),
    cashflow      = cfs,
    performing_cf = perf_cfs,
    loss_cf       = loss_cfs
  )
}

# ---------------------------------------------------------------------------
# Synthetic IRR wrapper
# ---------------------------------------------------------------------------

#' Compute projected IRR for a swap-in cohort using synthetic cashflows.
#'
#' @param ... All arguments passed to build_synthetic_cashflows().
#' @return Scalar numeric XIRR (annualised), or NA_real_ on failure.
compute_synthetic_irr <- function(n_loans, avg_amount, apr, term,
                                   delinq_rate, avg_months_to_default,
                                   observation_months = 36, lgd = 0.60) {
  cfs <- build_synthetic_cashflows(n_loans, avg_amount, apr, term,
                                    delinq_rate, avg_months_to_default,
                                    observation_months, lgd)
  if (nrow(cfs) < 2) return(NA_real_)
  tryCatch(xirr(cfs$cashflow, cfs$date), error = function(e) NA_real_)
}
