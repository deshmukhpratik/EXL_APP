# ============================================================
# One-time setup: Install all required R packages
# Run this once before launching the app:
#   Rscript setup.R
# ============================================================

cat("Installing required R packages...\n")

required_packages <- c(
  "shiny", "shinydashboard", "shinyWidgets", "shinyjs",
  "DT", "plotly",
  "dplyr", "tidyr", "rpart", "rlang",
  "ggplot2", "readr", "lubridate", "scales",
  "tibble", "stringr", "jsonlite"
)

# Optional AI Analytics packages
ai_packages <- c("xgboost", "shapviz")

# Install missing required packages
missing <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(missing) > 0) {
  cat("Installing missing packages:", paste(missing, collapse = ", "), "\n")
  install.packages(missing, repos = "https://cloud.r-project.org")
} else {
  cat("All required packages already installed.\n")
}

# Attempt AI packages (optional, non-fatal)
cat("\nAttempting optional AI Analytics packages (xgboost, shapviz)...\n")
for (pkg in ai_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    tryCatch(
      install.packages(pkg, repos = "https://cloud.r-project.org"),
      error = function(e) cat("  Skipped", pkg, "(optional):", conditionMessage(e), "\n")
    )
  }
}

cat("\nSetup complete! Launch the app with:\n")
cat("  Rscript run.R\n")
