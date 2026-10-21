# ============================================================
# R/utils/fmt.R — Formatting helpers
#
# Shared by tabs and services. Pure functions, vectorized.
# ============================================================

#' NULL-coalesce
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

#' Format count with K/M suffix: 2287 -> "2.3K"
fmt_k <- function(x) {
  ifelse(is.na(x), "\u2014",
    ifelse(abs(x) >= 1e6, paste0(formatC(x / 1e6, format = "f", digits = 1), "M"),
      ifelse(abs(x) >= 1e3, paste0(formatC(x / 1e3, format = "f", digits = 1), "K"),
             formatC(round(x), format = "d", big.mark = ","))))
}

#' Format dollar with K/M suffix: 148074000 -> "$148.1M"
fmt_dollar_k <- function(x) {
  ifelse(is.na(x), "\u2014",
    ifelse(abs(x) >= 1e6, paste0("$", formatC(x / 1e6, format = "f", digits = 1), "M"),
      ifelse(abs(x) >= 1e3, paste0("$", formatC(x / 1e3, format = "f", digits = 1), "K"),
             paste0("$", formatC(round(x), format = "d", big.mark = ",")))))
}

#' Format percentage: 0.1424 -> "14.24%"
fmt_pct <- function(x, digits = 2) {
  ifelse(is.na(x), "\u2014",
         paste0(formatC(x * 100, format = "f", digits = digits), "%"))
}

#' Format basis points delta: 0.0042 -> "+42 bps"
fmt_bps <- function(x) {
  if (is.na(x)) return("\u2014")
  sign <- if (x >= 0) "+" else "\u2212"
  paste0(sign, abs(round(x * 10000)), " bps")
}

# ---- Functions migrated from 02b_globals_utils.R ----

#' Format CART rule text for display: convert scientific notation to readable numbers
#' Currency fields -> "$X,XXX" format; other fields -> plain comma-separated numbers
format_rule_display <- function(rule_text) {
  if (is.null(rule_text) || is.na(rule_text) || rule_text == "") return(rule_text)

  currency_fields <- c("annual_income", "monthly_debt_payment", "issued_amount",
                        "loan_amount", "income", "amount", "payment")
  result <- rule_text

  # Pass 1: Currency fields with scientific notation -> $X,XXX
  for (field in currency_fields) {
    pattern <- paste0("(", field, "\\s*[<>=!]+\\s*)([0-9]+\\.?[0-9]*[eE][+\\-]?[0-9]+)")
    while (grepl(pattern, result)) {
      m <- regmatches(result, regexec(pattern, result))[[1]]
      if (length(m) >= 3) {
        num_val <- as.numeric(m[3])
        formatted <- paste0("$", formatC(round(num_val), format = "f", digits = 0, big.mark = ","))
        result <- sub(pattern, paste0(m[2], formatted), result, fixed = FALSE)
      } else break
    }
  }

  # Pass 2: Any remaining scientific notation -> plain comma-separated number
  general_pattern <- "([<>=!]+\\s*)([0-9]+\\.?[0-9]*[eE][+\\-]?[0-9]+)"
  while (grepl(general_pattern, result)) {
    m <- regmatches(result, regexec(general_pattern, result))[[1]]
    if (length(m) >= 3) {
      num_val <- as.numeric(m[3])
      digits <- if (abs(num_val - round(num_val)) < 0.01) 0 else 2
      formatted <- formatC(round(num_val, digits), format = "f", digits = digits, big.mark = ",")
      result <- sub(general_pattern, paste0(m[2], formatted), result, fixed = FALSE)
    } else break
  }

  result
}

#' Pretty-printer for variable names: maps column id to human label
pretty_var <- function(v) {
  m <- analysis_vars[analysis_vars == v]
  if (length(m) == 1) names(m) else gsub("_", " ", v)
}

#' Format a decimal rate as percentage string (e.g. 0.1196 -> "11.96%")
roundoff <- function(x, digits = 1) paste0(round(x * 100, digits), "%")

#' Parse vintage date strings (handles mm/dd/yyyy and dd/mm/yyyy)
parse_vintage_date <- function(x) {
  d <- suppressWarnings(as.Date(x, format = "%m/%d/%Y"))
  d[is.na(d)] <- suppressWarnings(as.Date(x[is.na(d)], format = "%d/%m/%Y"))
  d
}

#' Numeric version extractor: "v3.1" -> 3.1
version_num <- function(v) as.numeric(gsub("[^0-9.]", "", v))

#' Map canonical column names to legacy names expected by policy repo code
rename_to_old <- function(df) {
  col_map <- list(
    FICO_Score       = "fico_score",
    tu6              = "credit_file_age_months",
    delinquency_acc  = "delinquent_accounts_count",
    dti              = "debt_to_income_ratio",
    partner          = "partner_channel",
    repeat_type      = "borrower_repeat_type",
    issue_amount     = "issued_amount",
    score_band       = "fico_band",
    take_flag        = "issued_flag",
    delinquency_flag = "performance_delinquency_flag",
    vintage          = "booking_vintage",
    month_on_books   = "months_on_books",
    grade            = "risk_grade"
  )
  for (legacy in names(col_map)) {
    canon <- col_map[[legacy]]
    if (canon %in% names(df) && !legacy %in% names(df))
      df[[legacy]] <- df[[canon]]
  }
  # interest_rate in percentage scale (0-100) for KPI display
  if ("interest_rate_apr" %in% names(df) && !"interest_rate" %in% names(df))
    df$interest_rate <- df$interest_rate_apr * 100
  # offer_generated_flag from approval decision
  if (!"offer_generated_flag" %in% names(df))
    df$offer_generated_flag <- as.integer(
      !is.na(df$offered_decision_flag) & df$offered_decision_flag == "APPR"
    )
  # loan_id
  if (!"loan_id" %in% names(df))
    df$loan_id <- seq_len(nrow(df))
  # vintage as mm/dd/yyyy string (format expected by simulated_results)
  if ("date" %in% names(df))
    df$vintage <- format(df$date, "%m/%d/%Y 00:00")
  # term_months fallback
  if (!"term_months" %in% names(df))
    df$term_months <- 36L
  df
}
