# ============================================================
# Launch the Credit Strategy Manager
#
# Usage:
#   Rscript run.R              # Standard mode (port 7860)
#   Rscript run.R 3838         # Custom port
#
# AI Analytics mode (requires xgboost + shapviz):
#   CSM_AI_MODE=1 Rscript run.R
# ============================================================

args <- commandArgs(trailingOnly = TRUE)
port <- if (length(args) >= 1) as.integer(args[1]) else 7860L

cat("Starting Credit Strategy Manager on http://localhost:", port, "\n", sep = "")
cat("Press Ctrl+C to stop.\n\n")

shiny::runApp("app.R", port = port, host = "0.0.0.0", launch.browser = TRUE)
