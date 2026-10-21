# Data-ingress helpers.
#   - Today: `get_scoped_data()` filters loan_data by Applications/Issued scope.
#   - Milestone 4 adds `UPLOAD_DATA_CONTRACTS` and `validate_upload()` for the
#     Data Upload VS tab; they share this file because both govern what data
#     enters the app's reactive graph.

# ── Applications/Issued scope helper ────────────────────────────────────
# Returns the loan-data frame filtered by scope.
# scope in {"applications", "issued"}. Unknown values safe-default to issued.
get_scoped_data <- function(scope = "issued", df = NULL) {
  scope <- scope[1]  # tolerate accidental vector input
  if (is.null(df)) {
    df <- tryCatch(get("loan_data", envir = .GlobalEnv), error = function(e) NULL)
  }
  if (is.null(df)) return(data.frame())
  if (identical(scope, "applications")) return(df)
  if (!"issued_flag" %in% names(df)) return(df)  # legacy data — treat as issued
  df[df$issued_flag == TRUE | df$issued_flag == 1, , drop = FALSE]
}

# ── Upload contracts + validator ────────────────────────────────────────

UPLOAD_DATA_CONTRACTS <- list(
  application = list(
    target   = "application_tape",
    required = c("application_id", "app_date", "fico_score", "annual_income",
                 "debt_to_income_ratio", "N_HARD_INQ_6M", "issued_flag"),
    numeric  = c("fico_score", "annual_income", "debt_to_income_ratio",
                 "N_HARD_INQ_6M"),
    ranges   = list(fico_score = c(300, 850),
                    debt_to_income_ratio = c(0, 2),
                    annual_income = c(0, Inf),
                    N_HARD_INQ_6M = c(0, 50))
  ),
  issued = list(
    target   = "loan_data",
    required = c("loan_id", "fico_score", "risk_grade", "apr", "loan_amount",
                 "term_months", "issued_date", "product_code"),
    numeric  = c("fico_score", "apr", "loan_amount", "term_months"),
    ranges   = list(fico_score = c(300, 850),
                    apr = c(0, 0.50),
                    loan_amount = c(0, Inf),
                    term_months = c(6, 120))
  ),
  performance = list(
    target   = "perf_tape",
    required = c("loan_id", "month_on_book", "outstanding_balance",
                 "is_delinquent", "payment", "charge_off_flag"),
    numeric  = c("month_on_book", "outstanding_balance", "payment"),
    ranges   = list(month_on_book = c(0, 240),
                    outstanding_balance = c(0, Inf))
  ),
  irr = list(
    target   = "irr_cache",
    required = c("loan_id", "irr", "apr", "term_months",
                 "issued_amount", "total_int_paid"),
    numeric  = c("irr", "apr", "term_months", "issued_amount", "total_int_paid"),
    ranges   = list(irr = c(-1, 5), apr = c(0, 0.50))
  ),
  bureau = list(
    target   = "bureau_enrichment",
    required = c("application_id"),
    numeric  = character(0),
    ranges   = list()
  )
)

#' Validate an uploaded data frame against the contract for a data_type.
#' Returns list(ok_to_commit = logical(1),
#'              checks = list of lists with fields name/status/detail,
#'              preview = data.frame (first 20 rows))
#' Structural checks (Columns, Types) block commit when they fail.
#' Sanity checks (Completeness, Duplicates, Range) warn but do not block.
validate_upload <- function(df, data_type) {
  contract <- UPLOAD_DATA_CONTRACTS[[data_type]]
  if (is.null(contract)) {
    return(list(ok_to_commit = FALSE,
                checks = list(list(name = "Unknown data type",
                                   status = "fail",
                                   detail = data_type)),
                preview = head(df, 20)))
  }

  checks <- list()

  # 1. Columns present (blocking)
  missing_cols <- setdiff(contract$required, names(df))
  checks[[length(checks) + 1]] <- list(
    name   = "Columns present",
    status = if (length(missing_cols) == 0) "pass" else "fail",
    detail = if (length(missing_cols) == 0)
               paste0(length(contract$required), "/",
                      length(contract$required), " required columns present")
             else paste0("missing: ", paste(missing_cols, collapse = ", "))
  )

  # 2. Types conform (blocking)
  type_fails <- character(0)
  for (col in intersect(contract$numeric, names(df))) {
    if (!is.numeric(df[[col]]) && !all(is.na(df[[col]]))) {
      type_fails <- c(type_fails, col)
    }
  }
  checks[[length(checks) + 1]] <- list(
    name   = "Types conform",
    status = if (length(type_fails) == 0) "pass" else "fail",
    detail = if (length(type_fails) == 0) "all numeric columns are numeric"
             else paste0("non-numeric: ", paste(type_fails, collapse = ", "))
  )

  # 3. Completeness (warn)
  if (nrow(df) > 0) {
    cols_to_check <- intersect(contract$required, names(df))
    if (length(cols_to_check) > 0) {
      miss_pct <- vapply(cols_to_check,
                         function(c) mean(is.na(df[[c]])), numeric(1))
      worst <- max(miss_pct, na.rm = TRUE)
    } else worst <- 0
  } else worst <- 1
  checks[[length(checks) + 1]] <- list(
    name   = "Completeness",
    status = if (worst < 0.05) "pass" else "warn",
    detail = paste0(round((1 - worst) * 100, 1), "% filled on worst required column")
  )

  # 4. Duplicates (warn)
  id_cols <- contract$required[grepl("_id$", contract$required)]
  id_col  <- if (length(id_cols) > 0) id_cols[1] else NA_character_
  dup_count <- if (!is.na(id_col) && id_col %in% names(df))
                 sum(duplicated(df[[id_col]])) else 0
  checks[[length(checks) + 1]] <- list(
    name   = "Duplicates",
    status = if (dup_count == 0) "pass" else "warn",
    detail = paste0(dup_count, " duplicate(s) on ",
                    if (is.na(id_col)) "(no id column)" else id_col)
  )

  # 5. Range sanity (warn)
  range_fails <- character(0)
  for (col in names(contract$ranges)) {
    if (!col %in% names(df) || !is.numeric(df[[col]])) next
    r <- contract$ranges[[col]]
    bad <- sum(df[[col]] < r[1] | df[[col]] > r[2], na.rm = TRUE)
    if (bad > 0) range_fails <- c(range_fails,
                                  paste0(col, ": ", bad, " OOR"))
  }
  checks[[length(checks) + 1]] <- list(
    name   = "Range sanity",
    status = if (length(range_fails) == 0) "pass" else "warn",
    detail = if (length(range_fails) == 0) "all within bounds"
             else paste(range_fails, collapse = "; ")
  )

  structural_ok <- all(vapply(checks[1:2],
                              function(c) c$status == "pass", logical(1)))

  list(ok_to_commit = structural_ok,
       checks = checks,
       preview = head(df, 20))
}
