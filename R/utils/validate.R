# ============================================================
# R/utils/validate.R — Input validators
# ============================================================

#' Assert a value is within range
#' @return invisible TRUE or throws with a helpful message
validate_range <- function(value, lo, hi, name = "value") {
  if (is.null(value) || is.na(value)) {
    stop(name, " is NULL or NA")
  }
  if (value < lo || value > hi) {
    stop(sprintf("%s = %s is outside allowed range [%s, %s]", name, value, lo, hi))
  }
  invisible(TRUE)
}

#' Assert a dataframe has required columns
validate_columns <- function(df, required) {
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop("data.frame is missing required columns: ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}
