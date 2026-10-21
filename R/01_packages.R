# ============================================================
# Package Loading
# This file is named 00_packages.R to ensure it is sourced
# first by Shiny's automatic R/ directory sourcing.
# ============================================================

# Enable cashflow projection so xirr can converge on young vintages.
# PL-CURRENT loans only have ~1.5 years of observed cashflows; without
# projection, xirr sees front-loaded losses with no tail and returns NA.
# 50K threshold accommodates the current ~26K issued loans on dev data
# while still skipping projection on much larger production tapes.
Sys.setenv(CSM_MAX_PROJECT_LOANS = "50000")

suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(shinyjs)
  library(DT)
  library(plotly)
  library(dplyr)
  library(tidyr)
  library(rpart)
  library(rlang)
  library(ggplot2)
  library(readr)
  library(lubridate)
  library(scales)
  library(tibble)
  library(stringr)
  library(jsonlite)
  library(readxl)
})

# --- Engine layer (sourced explicitly, before Shiny auto-sources R/tab_*.R) ---
# Source directly into .GlobalEnv so tab server code can find these functions.
# (When Shiny auto-sources R/*.R, each file gets its own environment; explicit
# local=.GlobalEnv here ensures engine helpers are globally visible.)
for (.eng_file in list.files("R/engine", pattern = "\\.R$", full.names = TRUE)) {
  source(.eng_file, local = .GlobalEnv)
}
rm(.eng_file)

# SHAP is optional — only loaded when Risk Drivers tab's SHAP mode is used
.cpt_shap_available <- requireNamespace("xgboost", quietly = TRUE) &&
                       requireNamespace("shapviz",  quietly = TRUE)
