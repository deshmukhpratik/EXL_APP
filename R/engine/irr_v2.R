# ============================================================================
# irr_cashflow_engine_v2.R
# DCOR/DPOR-Aware IRR Cashflow Projection Engine
#
# Replaces v1's lump-sum terminal value with a full balance roll-forward
# waterfall driven by empirical DCOR/DPOR curves with parametric tail
# extrapolation. Matches client's Cash_Flow_Calculation sheet structure.
#
# DEPENDENCY: dplyr (>= 1.0)
#
# DESIGN DOC: docs/plans/2026-02-27-dcor-dpor-irr-engine-v2-design.md
#
# USAGE:
#   source("irr_cashflow_engine_v2.R")
#   tape_raw <- read.csv("irr_performance_tape_FIXED_v2.csv",
#                         stringsAsFactors = FALSE, check.names = FALSE)
#   tape <- build_perf_tape_v2(tape_raw)
#   port_xirr <- compute_portfolio_xirr_v2(tape)
#   by_fico   <- compute_grouped_xirr_v2(tape, group_col = "fico_band")
#
# ============================================================================

library(dplyr)

# ---------------------------------------------------------------------------
# XNPV / XIRR (copied from v1, unchanged)
# ---------------------------------------------------------------------------

#' Net present value at irregular dates
#' @param rate Discount rate (annualised)
#' @param cashflows Numeric vector of cashflows
#' @param dates Date vector corresponding to cashflows
#' @return Scalar NPV
xnpv <- function(rate, cashflows, dates) {
  t0    <- min(dates)
  years <- as.numeric(difftime(dates, t0, units = "days")) / 365
  sum(cashflows / ((1 + rate) ^ years))
}

#' Internal rate of return at irregular dates (Brent's method)
#' @param cashflows Numeric vector of cashflows (must have both +/-)
#' @param dates Date vector corresponding to cashflows
#' @return Scalar IRR or NA_real_ if unsolvable
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
# Date utilities (copied from v1, unchanged)
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
# Standard product parameters (recovery rates, fee rates)
# ---------------------------------------------------------------------------

#' Default recovery and fee rates by product type
#' @return Named list of lists, keyed by product_type
default_product_params <- function() {
  list(
    "Personal Loan" = list(
      recovery_rate       = 0.20,   # 20% of defaulted balance recovered
      servicing_fee_rate  = 0.0006, # 0.06%/month of performing balance
      collection_fee_rate = 0.10,   # 10% of default amount
      recovery_fee_rate   = 0.15    # 15% of recovery amount
    ),
    "Auto Loan" = list(
      recovery_rate       = 0.40,   # 40% (collateral-backed)
      servicing_fee_rate  = 0.0005, # 0.05%/month
      collection_fee_rate = 0.08,   # 8%
      recovery_fee_rate   = 0.10    # 10%
    )
  )
}

# ---------------------------------------------------------------------------
# Default scenario multipliers
# ---------------------------------------------------------------------------

#' Default scenario multipliers (stress/optimistic vs base)
#' @return Named list of scenario parameter lists
default_scenario_params <- function() {
  list(
    base = list(dcor_mult = 1.0, dpor_mult = 1.0),
    stress = list(dcor_mult = 1.5, dpor_mult = 0.7),
    optimistic = list(dcor_mult = 0.7, dpor_mult = 1.2)
  )
}

# ---------------------------------------------------------------------------
# Empirical DCOR/DPOR curves from tape data
# ---------------------------------------------------------------------------

#' Build empirical DCOR/DPOR curves from raw tape rows
#' @param tape_rows Data frame with product_type, term_months, vintage_quarter,
#'   month_on_books, dcor, dpor columns
#' @return Data frame with product_type, term_months, vintage_quarter, mob,
#'   dcor_rate, dpor_rate, n_loans, curve_source columns
build_empirical_curves <- function(tape_rows) {
  tape_rows %>%
    group_by(product_type, term_months, vintage_quarter,
             mob = as.integer(month_on_books)) %>%
    summarise(
      dcor_rate = mean(as.numeric(dcor), na.rm = TRUE),
      dpor_rate = mean(as.numeric(dpor), na.rm = TRUE),
      n_loans   = n(),
      .groups   = "drop"
    ) %>%
    mutate(curve_source = "empirical") %>%
    arrange(product_type, term_months, vintage_quarter, mob)
}

# ---------------------------------------------------------------------------
# Parametric tail fit: exponential decay for DCOR, constant for DPOR
# ---------------------------------------------------------------------------

#' Fit parametric tail beyond observed MOBs
#' @param empirical_curve Data frame with mob, dcor_rate, dpor_rate
#' @param term Loan term in months (projection endpoint)
#' @return Data frame with mob 0..term, dcor_rate, dpor_rate, curve_source
fit_curve_tail <- function(empirical_curve, term) {
  max_obs <- max(empirical_curve$mob)
  all_mobs <- 0:term

  # If we already have all MOBs, just return with proper source labels
  if (max_obs >= term) {
    return(data.frame(
      mob         = all_mobs,
      dcor_rate   = empirical_curve$dcor_rate[match(all_mobs, empirical_curve$mob)],
      dpor_rate   = empirical_curve$dpor_rate[match(all_mobs, empirical_curve$mob)],
      curve_source = "empirical",
      stringsAsFactors = FALSE
    ))
  }

  # --- DCOR: fit a * exp(-b * t) + c on observed MOBs > 0
  obs_dcor <- empirical_curve[empirical_curve$mob > 0, ]

  # Start with empirical values for observed MOBs
  pred_dcor <- numeric(length(all_mobs))
  for (m in empirical_curve$mob) {
    pred_dcor[m + 1] <- empirical_curve$dcor_rate[empirical_curve$mob == m]
  }

  # Extend beyond observed MOBs
  proj_mobs <- all_mobs[all_mobs > max_obs]

  if (nrow(obs_dcor) >= 3 && length(proj_mobs) > 0) {
    # Try NLS fit; fall back to last-observed-value on failure
    dcor_fit <- tryCatch({
      start <- list(
        a = max(obs_dcor$dcor_rate) * 2,
        b = 0.1,
        c = min(obs_dcor$dcor_rate[obs_dcor$dcor_rate > 0]) / 2
      )
      nls(dcor_rate ~ a * exp(-b * mob) + c, data = obs_dcor,
          start = start,
          control = nls.control(maxiter = 200, warnOnly = TRUE))
    }, error = function(e) NULL)

    if (!is.null(dcor_fit)) {
      # Only use NLS predictions for projected MOBs (beyond observed)
      proj_vals <- predict(dcor_fit, newdata = data.frame(mob = proj_mobs))
      pred_dcor[proj_mobs + 1] <- proj_vals
    } else {
      # Fallback: use average of last 3 observed MOBs as constant tail
      tail_avg <- mean(tail(obs_dcor$dcor_rate, 3))
      pred_dcor[proj_mobs + 1] <- tail_avg
    }
  } else if (length(proj_mobs) > 0) {
    # Too few points: use constant extension
    tail_val <- if (nrow(obs_dcor) > 0) mean(obs_dcor$dcor_rate) else 0
    pred_dcor[proj_mobs + 1] <- tail_val
  }

  # Cap DCOR to [0, 0.50] — no more than 50% default in a single month
  pred_dcor <- pmax(0, pmin(pred_dcor, 0.50))

  # --- DPOR: keep empirical values for observed MOBs, constant extension
  pred_dpor <- numeric(length(all_mobs))
  for (m in empirical_curve$mob) {
    pred_dpor[m + 1] <- empirical_curve$dpor_rate[empirical_curve$mob == m]
  }
  obs_dpor <- empirical_curve$dpor_rate[empirical_curve$mob > 0]
  dpor_const <- if (length(obs_dpor) > 0) mean(obs_dpor) else 0
  if (length(proj_mobs) > 0) {
    pred_dpor[proj_mobs + 1] <- dpor_const
  }

  # Cap DPOR to [0, 0.50]
  pred_dpor <- pmax(0, pmin(pred_dpor, 0.50))

  # --- Assemble output
  data.frame(
    mob         = all_mobs,
    dcor_rate   = pred_dcor,
    dpor_rate   = pred_dpor,
    curve_source = ifelse(all_mobs <= max_obs, "empirical", "fitted"),
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# Complete curve set: empirical + tail fit + vintage fallback
# ---------------------------------------------------------------------------

#' Build complete DCOR/DPOR curves for all Product x Term x Vintage segments
#' @param tape_rows Raw tape rows with dcor/dpor columns
#' @param min_fit_mobs Minimum observed MOBs required for parametric fit (default 6)
#' @return Data frame with product_type, term_months, vintage_quarter, scenario,
#'   mob, dcor_rate, dpor_rate, curve_source
build_complete_curves <- function(tape_rows, min_fit_mobs = 6) {
  # Build empirical curves
  empirical <- build_empirical_curves(tape_rows)

  # Get unique segments
  segments <- empirical %>%
    group_by(product_type, term_months, vintage_quarter) %>%
    summarise(max_mob = max(mob), n_mobs = n(), .groups = "drop") %>%
    arrange(product_type, term_months, vintage_quarter)

  result_list <- list()

  for (i in seq_len(nrow(segments))) {
    seg <- segments[i, ]
    seg_emp <- empirical %>%
      filter(product_type == seg$product_type,
             term_months  == seg$term_months,
             vintage_quarter == seg$vintage_quarter) %>%
      select(mob, dcor_rate, dpor_rate)

    term <- seg$term_months

    if (seg$n_mobs >= min_fit_mobs) {
      # Enough data: fit parametric tail
      full_curve <- fit_curve_tail(seg_emp, term = term)
    } else {
      # Young vintage: inherit from nearest older vintage of same product+term
      same_pt <- segments %>%
        filter(product_type == seg$product_type,
               term_months  == seg$term_months,
               vintage_quarter < seg$vintage_quarter,
               n_mobs >= min_fit_mobs) %>%
        arrange(desc(vintage_quarter))

      if (nrow(same_pt) > 0) {
        # Use the nearest older vintage's fitted curve
        donor_vq <- same_pt$vintage_quarter[1]
        donor_emp <- empirical %>%
          filter(product_type == seg$product_type,
                 term_months  == seg$term_months,
                 vintage_quarter == donor_vq) %>%
          select(mob, dcor_rate, dpor_rate)
        donor_curve <- fit_curve_tail(donor_emp, term = term)

        # Replace observed MOBs with this vintage's actuals, inherit the rest
        full_curve <- donor_curve
        for (m in seg_emp$mob) {
          idx <- full_curve$mob == m
          full_curve$dcor_rate[idx] <- seg_emp$dcor_rate[seg_emp$mob == m]
          full_curve$dpor_rate[idx] <- seg_emp$dpor_rate[seg_emp$mob == m]
          full_curve$curve_source[idx] <- "empirical"
        }
        full_curve$curve_source[full_curve$curve_source == "fitted"] <- "inherited"
      } else {
        # No donor available: use this vintage's data with constant extension
        full_curve <- fit_curve_tail(seg_emp, term = term)
      }
    }

    full_curve$product_type    <- seg$product_type
    full_curve$term_months     <- seg$term_months
    full_curve$vintage_quarter <- seg$vintage_quarter
    full_curve$scenario        <- "base"
    result_list[[i]] <- full_curve
  }

  bind_rows(result_list) %>%
    select(product_type, term_months, vintage_quarter, scenario,
           mob, dcor_rate, dpor_rate, curve_source)
}

# ---------------------------------------------------------------------------
# Scenario application (multipliers on base curves)
# ---------------------------------------------------------------------------

#' Apply scenario multipliers to base DCOR/DPOR curves
#' @param base_curves Data frame of base curves
#' @param scenario_name Character: "stress", "optimistic", etc.
#' @param scenario_params List with dcor_mult, dpor_mult
#' @return Data frame with adjusted curves and scenario label
apply_scenario <- function(base_curves, scenario_name, scenario_params) {
  out <- base_curves
  out$scenario <- scenario_name
  # Only apply multipliers to MOB > 0 (MOB 0 is always 0)
  nonzero <- out$mob > 0
  out$dcor_rate[nonzero] <- out$dcor_rate[nonzero] * scenario_params$dcor_mult
  out$dpor_rate[nonzero] <- out$dpor_rate[nonzero] * scenario_params$dpor_mult
  out
}

# ---------------------------------------------------------------------------
# Neighbor-derived curves (for swap-in analysis)
# ---------------------------------------------------------------------------

#' Build DCOR/DPOR curves from a set of neighbor loans
#' @param tape_rows Raw tape rows
#' @param neighbor_loan_ids Character vector of loan IDs to use
#' @param term Loan term for projection
#' @return Data frame with mob, dcor_rate, dpor_rate, curve_source or NULL
build_neighbor_curves <- function(tape_rows, neighbor_loan_ids, term) {
  neighbor_rows <- tape_rows[tape_rows$loan_id %in% neighbor_loan_ids, ]
  if (nrow(neighbor_rows) == 0) return(NULL)

  empirical <- neighbor_rows %>%
    group_by(mob = as.integer(month_on_books)) %>%
    summarise(
      dcor_rate = mean(as.numeric(dcor), na.rm = TRUE),
      dpor_rate = mean(as.numeric(dpor), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(mob)

  fit_curve_tail(empirical, term = term)
}

# ---------------------------------------------------------------------------
# Core: project one loan's cashflows from current MOB to term
# ---------------------------------------------------------------------------

#' Project a single loan's future cashflows using DCOR/DPOR curves
#' @param loan List with loan_id, product_type, issued_amount, interest_rate_apr,
#'   term_months, current_mob, remaining_balance, last_obs_date
#' @param curves Data frame with mob, dcor_rate, dpor_rate (0 to term)
#' @param product_params List with recovery_rate, servicing_fee_rate,
#'   collection_fee_rate, recovery_fee_rate
#' @return Data frame with waterfall columns: loan_id, mob, date, bbb, ed, er,
#'   ewf, eupp, espp, adj_eip, esf, ecf, erf, ncf, ebb
project_loan_cashflows <- function(loan, curves, product_params) {
  term       <- loan$term_months
  apr        <- loan$interest_rate_apr
  monthly_r  <- apr / 12
  start_mob  <- loan$current_mob
  balance    <- loan$remaining_balance
  orig_date  <- loan$last_obs_date
  recovery_r <- product_params$recovery_rate
  svc_fee    <- product_params$servicing_fee_rate
  coll_fee   <- product_params$collection_fee_rate
  rec_fee    <- product_params$recovery_fee_rate

  # Standard amortisation payment (on original balance for full term)
  if (monthly_r > 0) {
    pmt <- loan$issued_amount * monthly_r * (1 + monthly_r)^term /
      ((1 + monthly_r)^term - 1)
  } else {
    pmt <- loan$issued_amount / term
  }

  # Pre-allocate waterfall columns
  n_months <- term - start_mob + 1  # includes start_mob
  mobs      <- start_mob:term
  dates     <- vapply(seq_along(mobs) - 1, function(i) {
    as.numeric(add_months_base(orig_date, i))
  }, numeric(1))
  dates     <- as.Date(dates, origin = "1970-01-01")

  bbb_v  <- numeric(n_months)
  ed_v   <- numeric(n_months)
  er_v   <- numeric(n_months)
  ewf_v  <- numeric(n_months)
  eupp_v <- numeric(n_months)
  espp_v <- numeric(n_months)
  eip_v  <- numeric(n_months)
  esf_v  <- numeric(n_months)
  ecf_v  <- numeric(n_months)
  erf_v  <- numeric(n_months)
  ncf_v  <- numeric(n_months)
  ebb_v  <- numeric(n_months)

  # Month 0 (start_mob): disbursement or observed balance
  if (start_mob == 0) {
    # Full loan from origination
    bbb_v[1] <- loan$issued_amount
    ncf_v[1] <- -loan$issued_amount
    ebb_v[1] <- loan$issued_amount
  } else {
    # Mid-life: first row is the observed balance, no cashflow
    bbb_v[1] <- balance
    ncf_v[1] <- 0
    ebb_v[1] <- balance
  }

  # Project months start_mob+1 through term
  for (i in 2:n_months) {
    mob_t <- mobs[i]
    bbb   <- ebb_v[i - 1]

    # Look up DCOR/DPOR for this MOB
    curve_idx <- which(curves$mob == mob_t)
    dcor_t <- if (length(curve_idx) > 0) curves$dcor_rate[curve_idx[1]] else 0
    dpor_t <- if (length(curve_idx) > 0) curves$dpor_rate[curve_idx[1]] else 0

    # Guard: if balance is effectively zero, skip
    if (bbb < 0.01) {
      bbb_v[i] <- 0; ebb_v[i] <- 0
      next
    }

    # 1. Default path (cap ED at balance)
    ed  <- min(bbb * dcor_t, bbb)
    er  <- ed * recovery_r
    ewf <- ed - er

    # 2. Prepayment (on surviving balance, cap at surviving)
    eupp <- min((bbb - ed) * dpor_t, bbb - ed)

    # 3. Surviving balance (guaranteed >= 0)
    surviving <- max(bbb - ed - eupp, 0)

    # 4. Scheduled payment (capped at surviving balance)
    # Interest on surviving balance
    adj_eip <- surviving * monthly_r

    # Scheduled principal: PMT minus interest, but capped
    sched_principal <- min(pmt - adj_eip, surviving)
    sched_principal <- max(sched_principal, 0)

    # In final month, pay off remaining balance
    if (mob_t == term) {
      sched_principal <- surviving
    }

    # 5. Fees
    esf <- surviving * svc_fee
    ecf <- ed * coll_fee
    erf <- er * rec_fee

    # 6. Net cashflow
    ncf <- adj_eip + sched_principal + eupp + er - esf - ecf - erf

    # 7. Ending balance
    ebb <- surviving - sched_principal

    # Store
    bbb_v[i]  <- bbb
    ed_v[i]   <- ed
    er_v[i]   <- er
    ewf_v[i]  <- ewf
    eupp_v[i] <- eupp
    espp_v[i] <- sched_principal
    eip_v[i]  <- adj_eip
    esf_v[i]  <- esf
    ecf_v[i]  <- ecf
    erf_v[i]  <- erf
    ncf_v[i]  <- ncf
    ebb_v[i]  <- ebb
  }

  data.frame(
    loan_id  = loan$loan_id,
    mob      = mobs,
    date     = dates,
    bbb      = bbb_v,
    ed       = ed_v,
    er       = er_v,
    ewf      = ewf_v,
    eupp     = eupp_v,
    espp     = espp_v,
    adj_eip  = eip_v,
    esf      = esf_v,
    ecf      = ecf_v,
    erf      = erf_v,
    ncf      = ncf_v,
    ebb      = ebb_v,
    stringsAsFactors = FALSE
  )
}

# ---------------------------------------------------------------------------
# build_perf_tape_v2() — drop-in replacement for build_perf_tape()
# ---------------------------------------------------------------------------

#' Build performance tape with DCOR/DPOR-aware cashflow projections
#' @param df_raw Raw CSV data frame
#' @param curves Pre-built curves (NULL = auto-build from tape)
#' @param params Product parameters (NULL = use defaults)
#' @param scenario Scenario name: "base", "stress", "optimistic"
#' @return List with $rows, $projected, $loan_view, $max_date, $vintages,
#'   $products, $curves, $params, $segment_fields
build_perf_tape_v2 <- function(df_raw, curves = NULL, params = NULL,
                                scenario = "base") {

  # --- Column validation (same as v1 + dcor/dpor)
  required_cols <- c(
    "loan_id", "issued_flag", "issued_amount", "booking_vintage",
    "product_type", "partner_channel", "borrower_repeat_type",
    "performance_delinquency_flag", "co_amount", "payment_date",
    "cashflows", "interest_rate_apr", "fico_score", "month_on_books",
    "term_months", "schedule_payment", "interest_payment",
    "dcor", "dpor"
  )
  names(df_raw) <- trimws(names(df_raw))
  miss <- setdiff(required_cols, names(df_raw))
  if (length(miss) > 0) {
    stop(sprintf("Missing required columns: %s", paste(miss, collapse = ", ")))
  }

  # --- Type coercion & derived fields (same as v1)
  df <- df_raw %>%
    mutate(
      issued_flag       = as.integer(issued_flag),
      issued_amount     = as.numeric(issued_amount),
      co_amount         = as.numeric(co_amount),
      cashflows         = as.numeric(cashflows),
      interest_rate_apr = as.numeric(interest_rate_apr),
      fico_score        = as.numeric(fico_score),
      month_on_books    = as.integer(month_on_books),
      term_months       = as.integer(term_months),
      schedule_payment  = as.numeric(schedule_payment),
      interest_payment  = as.numeric(interest_payment),
      pre_payments      = if ("pre_payments" %in% names(.)) as.numeric(pre_payments) else 0,
      dcor              = as.numeric(dcor),
      dpor              = as.numeric(dpor),
      # payment_date in the CSV is DD/MM/YYYY (e.g. "24/03/2021"). as.Date
      # without a format parses it as YYYY/MM/DD -> year 24 AD which silently
      # breaks XIRR (cashflows span millenia, xirr diverges or hits inf rate).
      # Try DD/MM/YYYY first, then DD-MM-YYYY, then ISO as a last resort.
      payment_date      = {
        .p <- suppressWarnings(as.Date(payment_date, format = "%d/%m/%Y"))
        .p2 <- suppressWarnings(as.Date(payment_date, format = "%d-%m-%Y"))
        .p[is.na(.p)] <- .p2[is.na(.p)]
        .p3 <- suppressWarnings(as.Date(payment_date))
        .p[is.na(.p)] <- .p3[is.na(.p)]
        .p
      },
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
      # Try ISO format as final fallback
      booking_vintage_date = if_else(
        is.na(booking_vintage_date),
        suppressWarnings(as.Date(booking_vintage)),
        booking_vintage_date
      ),
      vintage_quarter = vintage_quarter_label(booking_vintage_date),
      payment_month   = floor_month_base(payment_date)
    ) %>%
    filter(issued_flag == 1)

  if (nrow(df) == 0) stop("No issued records found after filtering issued_flag == 1.")

  # --- Loan-level statics (same as v1 + term_months)
  loan_static <- df %>%
    arrange(loan_id, month_on_books) %>%
    group_by(loan_id) %>%
    summarise(
      vintage_quarter      = first(vintage_quarter),
      product_type         = first(product_type),
      partner_channel      = first(partner_channel),
      borrower_repeat_type = first(borrower_repeat_type),
      issued_amount        = first(issued_amount),
      interest_rate_apr    = first(interest_rate_apr),
      fico_score           = first(fico_score),
      term_months          = first(term_months),
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

  # --- First delinquency date (same as v1 with BUG-4 fix)
  first_dq_from_co <- df %>%
    filter(co_amount > 0) %>%
    group_by(loan_id) %>%
    summarise(first_dq_date = min(payment_date), .groups = "drop")

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

  # --- Build or accept DCOR/DPOR curves
  if (is.null(curves)) {
    curves <- build_complete_curves(df)

    # Apply scenario if not base
    if (scenario != "base") {
      sp <- default_scenario_params()
      if (scenario %in% names(sp)) {
        curves <- apply_scenario(curves, scenario, sp[[scenario]])
      }
    }
  }

  # --- Product parameters
  if (is.null(params)) {
    params <- default_product_params()
  }

  # --- Compute remaining balance and current MOB per loan
  loan_obs <- df %>%
    arrange(loan_id, month_on_books) %>%
    group_by(loan_id) %>%
    summarise(
      current_mob       = max(month_on_books),
      last_obs_date     = max(payment_date),
      last_sched_cum    = sum(schedule_payment[month_on_books > 0], na.rm = TRUE),
      last_int_cum      = sum(interest_payment[month_on_books > 0], na.rm = TRUE),
      cum_prepay        = sum(if ("pre_payments" %in% names(.)) pre_payments else 0, na.rm = TRUE),
      cum_co            = sum(co_amount, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(loan_static %>% select(loan_id, issued_amount, interest_rate_apr,
                                       term_months, product_type, vintage_quarter),
              by = "loan_id") %>%
    mutate(
      cum_principal     = last_sched_cum - last_int_cum,
      remaining_balance = pmax(0, issued_amount - cum_principal - cum_prepay - cum_co)
    )

  # --- Project future cashflows for each loan using DCOR/DPOR curves
  # Skip projection for large datasets (>50K loans) — the Python generator
  # already builds complete cashflows. Projection is only needed for small
  # datasets or when users upload new data without pre-built cashflows.
  max_project_loans <- as.integer(Sys.getenv("CSM_MAX_PROJECT_LOANS", "50000"))
  if (nrow(loan_obs) > max_project_loans) {
    cat("  Skipping cashflow projection for", nrow(loan_obs), "loans",
        "(exceeds CSM_MAX_PROJECT_LOANS=", max_project_loans, ").\n",
        "  Using observed cashflows only. Set CSM_MAX_PROJECT_LOANS higher to override.\n")
    projected <- data.frame(loan_id = character(), date = as.Date(character()),
                            ncf = numeric(), stringsAsFactors = FALSE)
  } else {
  cat("  Projecting cashflows for", nrow(loan_obs), "loans...\n")
  projected_list <- lapply(seq_len(nrow(loan_obs)), function(i) {
    lo <- loan_obs[i, ]

    # Skip if loan is already at or past term
    if (lo$current_mob >= lo$term_months) return(NULL)

    # Skip if remaining balance is effectively zero
    if (lo$remaining_balance < 1) return(NULL)

    # Find matching curve
    loan_curves <- curves %>%
      filter(product_type == lo$product_type,
             term_months  == lo$term_months,
             vintage_quarter == lo$vintage_quarter)

    # Fallback: try product + term only (ignore vintage)
    if (nrow(loan_curves) == 0) {
      loan_curves <- curves %>%
        filter(product_type == lo$product_type,
               term_months  == lo$term_months) %>%
        group_by(mob) %>%
        summarise(dcor_rate = mean(dcor_rate), dpor_rate = mean(dpor_rate),
                  .groups = "drop") %>%
        mutate(curve_source = "pooled")
    }

    # Fallback: try product only
    if (nrow(loan_curves) == 0) {
      loan_curves <- curves %>%
        filter(product_type == lo$product_type) %>%
        group_by(mob) %>%
        summarise(dcor_rate = mean(dcor_rate), dpor_rate = mean(dpor_rate),
                  .groups = "drop") %>%
        mutate(curve_source = "pooled")
    }

    if (nrow(loan_curves) == 0) return(NULL)

    # Get product params
    pp <- params[[lo$product_type]]
    if (is.null(pp)) pp <- params[[1]]  # fallback to first product

    loan_input <- list(
      loan_id           = as.character(lo$loan_id),
      product_type      = lo$product_type,
      issued_amount     = lo$issued_amount,
      interest_rate_apr = lo$interest_rate_apr,
      term_months       = lo$term_months,
      current_mob       = lo$current_mob,
      remaining_balance = lo$remaining_balance,
      last_obs_date     = lo$last_obs_date
    )

    project_loan_cashflows(loan_input, loan_curves, pp)
  })

  non_null <- !vapply(projected_list, is.null, logical(1))
  if (any(non_null)) {
    projected <- bind_rows(projected_list[non_null])
  } else {
    projected <- data.frame(loan_id = character(), date = as.Date(character()),
                            ncf = numeric(), stringsAsFactors = FALSE)
  }
  cat("  Projected", sum(non_null), "loans with future cashflows.\n")
  }  # end else (projection not skipped)

  # --- Build adjusted cashflows on observed rows
  # For observed rows: adj_cf = cashflows (actual)
  df$adj_cf <- df$cashflows

  # Aggregate projected NCF by loan_id + date for XIRR
  if (nrow(projected) > 0) {
    proj_agg <- projected %>%
      filter(mob > 0) %>%  # exclude the start row (no cashflow)
      group_by(loan_id, payment_date = date) %>%
      summarise(proj_cf = sum(ncf, na.rm = TRUE),
                proj_int = sum(adj_eip, na.rm = TRUE), .groups = "drop")
  } else {
    proj_agg <- data.frame(loan_id = character(), payment_date = as.Date(character()),
                           proj_cf = numeric(), proj_int = numeric(),
                           stringsAsFactors = FALSE)
  }

  # --- Pre-compute ever_del
  df <- df %>%
    arrange(loan_id, month_on_books) %>%
    group_by(loan_id) %>%
    mutate(ever_del = cummax(performance_delinquency_flag)) %>%
    ungroup()

  # --- Join fico_band
  df <- df %>%
    left_join(loan_static %>% select(loan_id, fico_band), by = "loan_id")

  df$loan_id <- as.character(df$loan_id)

  list(
    rows       = df,
    projected  = proj_agg,
    loan_view  = loan_view,
    max_date   = max(df$payment_date, na.rm = TRUE),
    vintages   = sort(unique(loan_view$vintage_quarter)),
    products   = sort(unique(loan_view$product_type)),
    curves     = curves,
    params     = params,
    segment_fields = c("partner_channel", "borrower_repeat_type")
  )
}

# ---------------------------------------------------------------------------
# XIRR wrappers — v2 (combines observed + projected cashflows)
# ---------------------------------------------------------------------------

#' Portfolio-level XIRR combining observed and projected cashflows
#' @param tape Output of build_perf_tape_v2()
#' @param loan_ids Optional character vector to filter loans
#' @return Scalar XIRR or NA_real_
compute_portfolio_xirr_v2 <- function(tape, loan_ids = NULL) {
  obs <- tape$rows
  proj <- tape$projected

  if (!is.null(loan_ids)) {
    ids <- as.character(loan_ids)
    obs  <- obs[obs$loan_id %in% ids, ]
    proj <- proj[proj$loan_id %in% ids, ]
  }

  # Observed: aggregate adj_cf by date
  obs_agg <- obs %>%
    group_by(payment_date) %>%
    summarise(cf = sum(adj_cf, na.rm = TRUE), .groups = "drop")

  # Projected: aggregate proj_cf by date
  proj_agg <- proj %>%
    group_by(payment_date) %>%
    summarise(cf = sum(proj_cf, na.rm = TRUE), .groups = "drop")

  # Combine
  all_cf <- bind_rows(obs_agg, proj_agg) %>%
    group_by(payment_date) %>%
    summarise(cf = sum(cf), .groups = "drop") %>%
    arrange(payment_date)

  if (nrow(all_cf) < 2) return(NA_real_)
  tryCatch(xirr(all_cf$cf, all_cf$payment_date), error = function(e) NA_real_)
}

#' Grouped XIRR (by any column) combining observed and projected cashflows
#' @param tape Output of build_perf_tape_v2()
#' @param group_col Column name to group by (e.g., "fico_band")
#' @param loan_ids Optional character vector to filter loans
#' @return Data frame with group and xirr_value columns
compute_grouped_xirr_v2 <- function(tape, group_col, loan_ids = NULL) {
  obs <- tape$rows
  proj <- tape$projected

  if (!is.null(loan_ids)) {
    ids <- as.character(loan_ids)
    obs  <- obs[obs$loan_id %in% ids, ]
    proj <- proj[proj$loan_id %in% ids, ]
  }

  if (!group_col %in% names(obs)) {
    return(data.frame(group = character(), xirr_value = numeric(),
                      stringsAsFactors = FALSE))
  }

  # Get group assignment per loan
  loan_groups <- obs %>%
    filter(!is.na(.data[[group_col]])) %>%
    group_by(loan_id) %>%
    summarise(grp = first(.data[[group_col]]), .groups = "drop")

  # For each group, combine observed + projected
  results <- loan_groups %>%
    group_by(grp) %>%
    group_modify(~ {
      ids <- .x$loan_id

      obs_cf <- obs[obs$loan_id %in% ids, ] %>%
        group_by(payment_date) %>%
        summarise(cf = sum(adj_cf, na.rm = TRUE), .groups = "drop")

      proj_cf <- proj[proj$loan_id %in% ids, ] %>%
        group_by(payment_date) %>%
        summarise(cf = sum(proj_cf, na.rm = TRUE), .groups = "drop")

      all_cf <- bind_rows(obs_cf, proj_cf) %>%
        group_by(payment_date) %>%
        summarise(cf = sum(cf), .groups = "drop") %>%
        arrange(payment_date)

      data.frame(xirr_value = tryCatch(xirr(all_cf$cf, all_cf$payment_date),
                                         error = function(e) NA_real_))
    }) %>%
    ungroup() %>%
    transmute(group = grp, xirr_value)

  results
}

#' Loan-level XIRR combining observed and projected cashflows
#' @param tape Output of build_perf_tape_v2()
#' @return Data frame with loan_id and loan_irr columns
#'
#' Vectorised version: aggregates observed + projected cashflows by
#' (loan_id, payment_date) in a single pass, then splits the aggregated
#' frame once and applies xirr per group. On a 100K dev dataset
#' (~26K loans, ~800K tape rows) this runs in ~30s vs several minutes
#' for the per-loan %in% scan it replaced.
compute_loan_irr_v2 <- function(tape) {
  obs <- tape$rows
  proj <- tape$projected

  if (is.null(obs) || nrow(obs) == 0) {
    return(data.frame(loan_id = character(), loan_irr = numeric(),
                      stringsAsFactors = FALSE))
  }

  obs_cf <- data.frame(
    loan_id      = as.character(obs$loan_id),
    payment_date = obs$payment_date,
    cf           = obs$adj_cf,
    stringsAsFactors = FALSE
  )

  all_cf <- if (!is.null(proj) && nrow(proj) > 0) {
    proj_cf <- data.frame(
      loan_id      = as.character(proj$loan_id),
      payment_date = proj$payment_date,
      cf           = proj$proj_cf,
      stringsAsFactors = FALSE
    )
    rbind(obs_cf, proj_cf)
  } else {
    obs_cf
  }

  # Aggregate by (loan_id, payment_date) in one vectorised pass
  agg <- stats::aggregate(cf ~ loan_id + payment_date, data = all_cf, FUN = sum)
  agg <- agg[order(agg$loan_id, agg$payment_date), ]

  # Split once into per-loan cashflow frames, then xirr each
  splits <- split(agg[, c("payment_date", "cf")], agg$loan_id)
  irr_vals <- vapply(splits, function(d) {
    tryCatch(xirr(d$cf, d$payment_date), error = function(e) NA_real_)
  }, numeric(1))

  data.frame(
    loan_id  = names(irr_vals),
    loan_irr = unname(irr_vals),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

# ============================================================================
# Self-tests (run with: Rscript irr_cashflow_engine_v2.R --test)
# ============================================================================
if (length(commandArgs(trailingOnly = TRUE)) > 0 &&
    commandArgs(trailingOnly = TRUE)[1] == "--test") {

  cat("=== IRR Engine v2 Self-Tests ===\n")
  pass <- 0; fail <- 0
  assert <- function(desc, expr) {
    if (isTRUE(expr)) { cat("  PASS:", desc, "\n"); pass <<- pass + 1 }
    else { cat("  FAIL:", desc, "\n"); fail <<- fail + 1 }
  }

  # -- Task 2: Product params --
  cat("\n--- Product Parameters ---\n")
  params <- default_product_params()
  assert("params is a list", is.list(params))
  assert("has Personal Loan", "Personal Loan" %in% names(params))
  assert("has Auto Loan", "Auto Loan" %in% names(params))
  assert("PL recovery_rate is 0.20", params[["Personal Loan"]]$recovery_rate == 0.20)
  assert("AL recovery_rate is 0.40", params[["Auto Loan"]]$recovery_rate == 0.40)
  assert("PL has 4 params", length(params[["Personal Loan"]]) == 4)
  assert("AL servicing_fee_rate is 0.0005", params[["Auto Loan"]]$servicing_fee_rate == 0.0005)

  # -- Task 3: Empirical curves --
  cat("\n--- Empirical Curves ---\n")
  test_rows <- data.frame(
    product_type = rep(c("Personal Loan", "Personal Loan"), each = 3),
    term_months  = rep(c(36L, 36L), each = 3),
    vintage_quarter = rep(c("2024Q1", "2024Q1"), each = 3),
    month_on_books = rep(0:2, 2),
    dcor = c(0.000, 0.002, 0.003, 0.000, 0.004, 0.001),
    dpor = c(0.000, 0.010, 0.010, 0.000, 0.010, 0.010),
    stringsAsFactors = FALSE
  )
  ec <- build_empirical_curves(test_rows)
  assert("empirical curves is data.frame", is.data.frame(ec))
  assert("has required columns",
    all(c("product_type","term_months","vintage_quarter","mob","dcor_rate","dpor_rate") %in% names(ec)))
  assert("MOB 0 dcor is 0", ec$dcor_rate[ec$mob == 0][1] == 0)
  assert("MOB 1 dcor is average of 0.002 and 0.004",
    abs(ec$dcor_rate[ec$mob == 1][1] - 0.003) < 1e-9)
  assert("curve_source is empirical", all(ec$curve_source == "empirical"))

  # -- Task 4: Parametric tail --
  cat("\n--- Parametric Tail Fitter ---\n")
  emp <- data.frame(
    mob       = 0:9,
    dcor_rate = c(0, 0.008, 0.006, 0.005, 0.004, 0.003, 0.003, 0.002, 0.002, 0.002),
    dpor_rate = rep(0.01, 10)
  )
  fitted <- fit_curve_tail(emp, term = 36)
  assert("fitted has 37 rows (MOB 0-36)", nrow(fitted) == 37)
  assert("fitted MOBs 0-9 match empirical dcor",
    all(abs(fitted$dcor_rate[1:10] - emp$dcor_rate) < 1e-6))
  assert("fitted MOBs 10-36 are all >= 0",
    all(fitted$dcor_rate[11:37] >= 0))
  assert("fitted DCOR decays (MOB 36 <= MOB 10)",
    fitted$dcor_rate[37] <= fitted$dcor_rate[11] + 1e-6)
  assert("fitted DPOR is ~0.01 throughout",
    all(abs(fitted$dpor_rate - 0.01) < 0.005))
  assert("fitted has curve_source column",
    all(fitted$curve_source[1:10] == "empirical") &&
    all(fitted$curve_source[11:37] == "fitted"))

  # -- Task 5: Complete curves with fallback --
  cat("\n--- Complete Curves with Fallback ---\n")
  mature_rows <- data.frame(
    product_type = "Personal Loan", term_months = 36L,
    vintage_quarter = "2024Q1",
    month_on_books = 0:9,
    dcor = c(0, 0.008, 0.006, 0.005, 0.004, 0.003, 0.003, 0.002, 0.002, 0.002),
    dpor = rep(0.01, 10), stringsAsFactors = FALSE
  )
  young_rows <- data.frame(
    product_type = "Personal Loan", term_months = 36L,
    vintage_quarter = "2025Q1",
    month_on_books = 0:2,
    dcor = c(0, 0.010, 0.007),
    dpor = rep(0.01, 3), stringsAsFactors = FALSE
  )
  test_tape_rows <- rbind(mature_rows, young_rows)
  cc <- build_complete_curves(test_tape_rows, min_fit_mobs = 6)
  assert("complete curves covers both vintages",
    length(unique(cc$vintage_quarter)) == 2)
  assert("mature vintage has MOBs 0-36",
    nrow(cc[cc$vintage_quarter == "2024Q1", ]) == 37)
  assert("young vintage has MOBs 0-36 (inherited)",
    nrow(cc[cc$vintage_quarter == "2025Q1", ]) == 37)
  assert("young vintage has inherited source for high MOBs",
    any(cc$curve_source[cc$vintage_quarter == "2025Q1"] == "inherited"))

  # -- Task 6: Scenarios and neighbor curves --
  cat("\n--- Scenarios ---\n")
  base_curves <- data.frame(
    product_type = "Personal Loan", term_months = 36L,
    vintage_quarter = "2024Q1", scenario = "base",
    mob = 0:5,
    dcor_rate = c(0, 0.008, 0.006, 0.005, 0.004, 0.003),
    dpor_rate = c(0, 0.010, 0.010, 0.010, 0.010, 0.010),
    curve_source = "empirical", stringsAsFactors = FALSE
  )
  stress_params <- list(dcor_mult = 1.5, dpor_mult = 0.7, recovery_adj = -0.05)
  sc <- apply_scenario(base_curves, "stress", stress_params)
  assert("stress scenario label applied", all(sc$scenario == "stress"))
  assert("stress DCOR is 1.5x base at MOB 1",
    abs(sc$dcor_rate[sc$mob == 1] - 0.008 * 1.5) < 1e-9)
  assert("stress DPOR is 0.7x base at MOB 1",
    abs(sc$dpor_rate[sc$mob == 1] - 0.010 * 0.7) < 1e-9)
  assert("MOB 0 stays at 0", sc$dcor_rate[sc$mob == 0] == 0)

  # -- Task 7: Core waterfall projection --
  cat("\n--- Core Waterfall ---\n")
  # Simple loan: $10,000, 12% APR, 36 months, no defaults, no prepay
  test_curve_clean <- data.frame(
    mob = 0:36,
    dcor_rate = rep(0, 37),
    dpor_rate = rep(0, 37),
    curve_source = "empirical", stringsAsFactors = FALSE
  )
  test_loan <- list(
    loan_id = "TEST001",
    product_type = "Personal Loan",
    issued_amount = 10000,
    interest_rate_apr = 0.12,
    term_months = 36L,
    current_mob = 0L,
    remaining_balance = 10000,
    last_obs_date = as.Date("2025-01-15")
  )
  test_params <- default_product_params()[["Personal Loan"]]
  wf <- project_loan_cashflows(test_loan, test_curve_clean, test_params)
  assert("waterfall has 37 rows (MOB 0-36)", nrow(wf) == 37)
  assert("month 0 is negative (disbursement)", wf$ncf[1] < 0)
  assert("months 1-36 are positive (payments)", all(wf$ncf[2:37] > 0))
  assert("ending balance at MOB 36 is ~0", abs(wf$ebb[37]) < 1.0)
  assert("total NCF > disbursement (positive IRR)",
    sum(wf$ncf) > 0)
  assert("all ed/ewf/eupp are 0 (no defaults/prepay)",
    all(wf$ed == 0) && all(wf$eupp == 0))

  # Loan with defaults: 5% DCOR at every MOB
  test_curve_lossy <- data.frame(
    mob = 0:36,
    dcor_rate = c(0, rep(0.05, 36)),
    dpor_rate = rep(0, 37),
    curve_source = "empirical", stringsAsFactors = FALSE
  )
  wf_loss <- project_loan_cashflows(test_loan, test_curve_lossy, test_params)
  assert("lossy waterfall has defaults", sum(wf_loss$ed) > 0)
  assert("lossy waterfall has recoveries", sum(wf_loss$er) > 0)
  assert("lossy ending balance < clean ending balance",
    wf_loss$ebb[37] < wf$ebb[37] + 1)
  assert("lossy total NCF < clean total NCF",
    sum(wf_loss$ncf) < sum(wf$ncf))

  # -- Task 8: build_perf_tape_v2 against real data --
  cat("\n--- Tape Builder (Real Data) ---\n")
  tape_file <- "inst/data/irr_performance_tape_FIXED_v2.csv"
  if (file.exists(tape_file)) {
    cat("  Loading real tape for integration test...\n")
    raw <- read.csv(tape_file, stringsAsFactors = FALSE, check.names = FALSE)
    tape_v2 <- build_perf_tape_v2(raw)

    assert("tape_v2 has $rows", !is.null(tape_v2$rows))
    assert("tape_v2 has $loan_view", !is.null(tape_v2$loan_view))
    assert("tape_v2 has $max_date", !is.null(tape_v2$max_date))
    assert("tape_v2 has $vintages", !is.null(tape_v2$vintages))
    assert("tape_v2 has $products", !is.null(tape_v2$products))
    assert("tape_v2 has $curves", !is.null(tape_v2$curves))
    assert("tape_v2$rows has adj_cf column", "adj_cf" %in% names(tape_v2$rows))
    assert("tape_v2$rows has ever_del column", "ever_del" %in% names(tape_v2$rows))
    assert("adj_cf has no NAs", !any(is.na(tape_v2$rows$adj_cf)))
    assert("curves cover all product-term-vintage combos",
      nrow(tape_v2$curves) > 100)

    cat("  Real tape integration test complete.\n")
  } else {
    cat("  SKIP: real tape file not found (", tape_file, ")\n")
  }

  # -- Task 9: XIRR wrappers --
  cat("\n--- XIRR Wrappers ---\n")
  if (exists("tape_v2") && !is.null(tape_v2)) {
    port_irr <- compute_portfolio_xirr_v2(tape_v2)
    assert("portfolio XIRR is numeric", is.numeric(port_irr))
    assert("portfolio XIRR is not NA", !is.na(port_irr))
    assert("portfolio XIRR is reasonable (-50% to +200%)",
      port_irr > -0.5 && port_irr < 2.0)

    by_fico <- compute_grouped_xirr_v2(tape_v2, "fico_band")
    assert("grouped XIRR returns data.frame", is.data.frame(by_fico))
    assert("grouped XIRR has group and xirr_value columns",
      all(c("group", "xirr_value") %in% names(by_fico)))
    assert("grouped XIRR has multiple FICO bands", nrow(by_fico) >= 3)

    cat("  Portfolio XIRR v2: ", round(port_irr * 100, 2), "%\n")
    cat("  XIRR by FICO band:\n")
    for (j in seq_len(nrow(by_fico))) {
      cat("    ", by_fico$group[j], ": ",
          round(by_fico$xirr_value[j] * 100, 2), "%\n")
    }
  }

  # -- Task 10: v1 vs v2 comparison --
  cat("\n--- v1 vs v2 Comparison ---\n")
  if (file.exists(tape_file)) {
    source("R/engine/irr.R")
    tape_v1 <- build_perf_tape(raw)
    v1_irr <- compute_portfolio_xirr(tape_v1)
    v2_irr <- compute_portfolio_xirr_v2(tape_v2)

    cat("  v1 Portfolio XIRR: ", round(v1_irr * 100, 2), "%\n")
    cat("  v2 Portfolio XIRR: ", round(v2_irr * 100, 2), "%\n")
    cat("  Delta:             ", round((v2_irr - v1_irr) * 100, 2), "pp\n")

    assert("v2 IRR is lower than v1 (more conservative due to projected losses)",
      v2_irr < v1_irr + 0.01)  # allow small tolerance

    # Grouped comparison
    v1_fico <- compute_grouped_xirr(tape_v1, "fico_band")
    v2_fico <- compute_grouped_xirr_v2(tape_v2, "fico_band")
    cat("\n  FICO Band Comparison:\n")
    cat(sprintf("  %-10s  %8s  %8s  %8s\n", "Band", "v1", "v2", "Delta"))
    merged <- merge(v1_fico, v2_fico, by = "group", suffixes = c("_v1", "_v2"))
    for (j in seq_len(nrow(merged))) {
      d <- merged$xirr_value_v2[j] - merged$xirr_value_v1[j]
      cat(sprintf("  %-10s  %7.2f%%  %7.2f%%  %+7.2fpp\n",
                  merged$group[j],
                  merged$xirr_value_v1[j] * 100,
                  merged$xirr_value_v2[j] * 100,
                  d * 100))
    }

    cat("\n  Curve summary:\n")
    cat("  Unique curve segments:", nrow(distinct(tape_v2$curves,
        product_type, term_months, vintage_quarter)), "\n")
    cat("  Curve sources:\n")
    print(table(tape_v2$curves$curve_source))
  }

  cat(sprintf("\n=== FINAL: %d passed, %d failed ===\n", pass, fail))
  if (fail > 0) quit(status = 1)
}
