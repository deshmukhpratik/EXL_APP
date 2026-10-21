# ============================================================
# Credit Strategy Manager – Modular Launcher (Full Edition)
# Credit Strategy Manager | Policy Simulation Platform
#
# This modular version sources all R/*.R files and wires
# together the UI and server from each tab module.
# Includes ALL tabs: Policy Engine (5 sub-tabs), Policy
# Repository, Portfolio Insights, Data Upload, Policy Editor,
# and AI Analytics (4 tabs).
#
# HOW TO OPEN:
#   Rscript -e "shiny::runApp('app.R', port=7860, host='0.0.0.0')"
#
# AI ANALYTICS (optional — needs xgboost + shapviz):
#   CSM_AI_MODE=1 Rscript -e "shiny::runApp('app.R', port=7860, host='0.0.0.0')"
#
# DEPENDENCIES (install once):
#   install.packages(c("shiny","shinydashboard","shinyWidgets",
#     "shinyjs","DT","plotly","dplyr","tidyr","rpart","rlang",
#     "ggplot2","readr","lubridate","scales","tibble","stringr","jsonlite"))
#   # For AI Analytics (optional):
#   install.packages(c("xgboost","shapviz"))
# ============================================================

# Load .env (API keys etc.) into process environment if present
if (file.exists(".env")) readRenviron(".env")

# Serve static assets from inst/www/ at the URL prefix /assets/
shiny::addResourcePath("assets", "inst/www")

# Shiny auto-sources top-level R/*.R files in alphabetical order BEFORE
# evaluating app.R, so by this point these are already loaded:
#   00_theme.R          — design tokens, bs_theme(), ggplot2 theme
#   01_packages.R       — loads packages & credit policy engines (R/engine/)
#   02_globals_data.R   — data loaders (load_loan_data, load_perf_tape, load_irr_cache)
#   03_globals_policies.R — policy function closures
#
# We only need to source nested dirs that Shiny doesn't descend into.

# Services (compose engine, called by tab servers)
for (f in list.files("R/services", pattern = "\\.R$", full.names = TRUE)) source(f, local = .GlobalEnv)
for (f in list.files("R/services/providers", pattern = "\\.R$", full.names = TRUE)) source(f, local = .GlobalEnv)

# Utils
for (f in list.files("R/utils", pattern = "\\.R$", full.names = TRUE)) source(f, local = .GlobalEnv)

# UI building blocks
for (f in list.files("R/ui", pattern = "\\.R$", full.names = TRUE)) source(f, local = .GlobalEnv)

# Tab modules (UI + server factories)
for (f in list.files("R/tabs", pattern = "\\.R$", full.names = TRUE)) source(f, local = .GlobalEnv)

# ---- Build policy repo globals (needs govern_service.R) ----
mockPolicies <- build_mock_policies(strategy_overrides)
message(sprintf("  Built %d policies from strategy_overrides.", nrow(mockPolicies)))

base_loans <- build_base_loans(credit_data)
message("Derived base_loans for policy repository.")

vintage_raw <- build_vintage_data(base_loans, mockPolicies)
message("Built synthetic vintage performance data.")

saved_policies_path <- "inst/data/saved_policies.rds"

persisted <- load_persisted_policies(strategy_overrides, policy_fns)
persisted_policies <- persisted$persisted_policies
persisted_fns      <- persisted$persisted_fns
policy_fns         <- persisted$policy_fns

# Mirror to .GlobalEnv so server code can find them
for (.nm in c("mockPolicies", "base_loans", "vintage_raw",
              "persisted_policies", "persisted_fns", "policy_fns",
              "saved_policies_path")) {
  assign(.nm, get(.nm), envir = .GlobalEnv)
}
rm(.nm, persisted)
message("Policy repository data ready.")

# AI mode: available when xgboost + shapviz are installed (.cpt_shap_available
# is set in 01_packages.R via requireNamespace checks)
ai_mode <- isTRUE(get0(".cpt_shap_available", envir = .GlobalEnv))

# Helper: custom image icon for sidebar (must be <i> tag for shinydashboard)
img_icon <- function(src) {
  tags$i(class = "fa", style = "font-style:normal;",
    tags$img(src = src, style = "width:18px; height:18px; filter:invert(1); vertical-align:middle;")
  )
}

# ============================================================
# UI
# ============================================================
ui <- dashboardPage(
  skin = "black",

  dashboardHeader(
    titleWidth = 270,
    title = tags$span(
      style = "font-weight:bold; color:#fb6e36; font-size:17px; white-space:nowrap;",
      "Risk Strategy Manager"
    )
  ),

  dashboardSidebar(
    width = 270,
    sidebarMenu(
      id = "sidebar_menu",
      # 1. Data Model
      menuItem("Data Model", icon = icon("database"),
        # Original Data Upload tab hidden for now — superseded by Data Upload VS.
        # menuSubItem("Data Upload (legacy)", tabName = "data_upload",
        #              icon = icon("cloud-arrow-up")),
        menuSubItem("Data Upload", tabName = "data_upload_vs",
                     icon = icon("cloud-arrow-up"))
      ),
      # 2. Policy Engine
      menuItem("Policy Engine", icon = icon("gears"), startExpanded = TRUE,
        menuSubItem("1. Risk Agent", tabName = "smart_rec",
                     icon = icon("bullseye"), selected = TRUE),
        menuSubItem("2. Opportunity Agent", tabName = "opp_find",
                     icon = icon("magnifying-glass-dollar")),
        menuSubItem("3. Driver Agent", tabName = "risk_driver",
                     icon = icon("chart-column")),
        # Root Cause Explorer hidden for now — re-enable by un-commenting.
        # menuSubItem("4. Root Cause Explorer", tabName = "root_cause",
        #              icon = icon("diagram-project")),
        menuSubItem("4. Simulation Agent",  tabName = "whatif_tab",
                     icon = icon("sliders"))
      ),
      # 3. Policy Repository (Policy History hidden for demo)
      menuItem("Policy Repository", tabName = "repos_1",
               icon = icon("vault")),
      # menuItem("Policy History", tabName = "policy_history", icon = icon("clock-rotate-left")),
      # 4. Governance
      menuItem("Governance", icon = icon("scale-balanced"),
        menuSubItem("IRR Engine", tabName = "irr_engine",
                     icon = icon("calculator"))
      ),

      # -- AI Insights hidden for demo --
      # menuItem("AI Insights", icon = icon("brain"),
      #   menuSubItem("Counterfactual Explainer", tabName = "counterfactual",
      #                icon = icon("route")),
      #   menuSubItem("Fair Lending Auditor", tabName = "fair_lending",
      #                icon = icon("scale-balanced"))
      # ),

      # AI Analytics group (visible only when AI mode is on)
      if (ai_mode) menuItem("AI Analytics", icon = icon("microchip"),
        menuSubItem("SHAP Explainer",     tabName = "ai_shap",
                     icon = icon("diagram-project")),
        menuSubItem("Variable Discovery", tabName = "ai_variable",
                     icon = icon("microscope"))
      )
    )
  ),

  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", href = "assets/css/app.css"),
      # Suppress Shiny's default grey "recalculating" overlay entirely.
      # The app uses its own spinners, progress bars, and notifications
      # for loading feedback, so the built-in grey overlay is redundant
      # and causes a bad UX when reactive chains fire across hidden tabs.
      tags$style(HTML("
        .recalculating { opacity: 1 !important; }
        /* DO NOT hide .shiny-output-error — previously set to visibility:hidden,
           which silently blanked every renderer that threw. Now style errors so
           they are visible but non-disruptive during demo. */
        .shiny-output-error {
          visibility: visible;
          color: #dc2626;
          font-size: 11px;
          padding: 6px 10px;
          background: #1f2937;
          border-left: 3px solid #dc2626;
          border-radius: 4px;
          font-family: monospace;
        }
        .shiny-output-error:before { content: '\\26A0  '; }
        .progress-striped .progress-bar { transition: width 0.1s ease; }
      ")),
      # Prevent the html.shiny-busy class from ever appearing
      tags$script(HTML("
        $(document).on('shiny:busy', function() {
          setTimeout(function() { $('html').removeClass('shiny-busy'); }, 0);
        });
      ")),
      pipeline_head_tags()
    ),

    # ── NL Search (active on Policy Engine tabs; disabled elsewhere) ────
    nl_bar_ui(),

    do.call(tabItems, Filter(Negate(is.null), list(
      # ---- Core tabs ----
      opp_find_ui(),
      whatif_ui(),
      policy_editor_ui(),
      var_analysis_ui(),
      risk_driver_ui(),
      smart_recommender_ui(),
      policy_repo_ui(),
      policy_history_ui(),
      data_upload_ui(),
      data_upload_vs_ui(),
      pi_acquisition_ui(),
      pi_performance_ui(),
      irr_engine_ui(),
      root_cause_ui(),

      # ---- AI Insights tabs (hidden for demo) ----
      # counterfactual_ui(),
      # fair_lending_ui(),

      # ---- AI Analytics tabs (only when AI mode is on) ----
      if (ai_mode) ai_shap_ui(),
      if (ai_mode) ai_variable_ui()
    )))
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {

  # ---- NL Q&A bar ----
  nl_rv <- reactiveValues(answer = NULL, in_flight = FALSE)

  # Disable the bar on non-Policy-Engine tabs + reset scope on tab change
  observeEvent(input$sidebar_menu, {
    is_enabled <- isTRUE(input$sidebar_menu %in% NL_ENABLED_TABS)
    shinyjs::toggleClass("nl_search_bar", "nl-disabled", condition = !is_enabled)
    shinyjs::toggleState("nl_query", condition = is_enabled)
    # Per spec: scope toggle resets to "Current view" on tab change
    updateRadioButtons(session, "nl_scope", selected = "current")
    # Clear any stale answer from the previous tab
    nl_rv$answer <- NULL
  }, ignoreInit = FALSE)

  # Submission handler — bound to the Enter-key JS which sets input$nl_submit.
  # Debounce: if a previous call is in flight, drop this submission.
  observeEvent(input$nl_submit, {
    if (isTRUE(nl_rv$in_flight)) return()
    if (!isTRUE(input$sidebar_menu %in% NL_ENABLED_TABS)) return()
    query <- trimws(input$nl_query %||% "")
    if (!nzchar(query)) return()
    scope <- input$nl_scope %||% "current"
    tab_id <- input$sidebar_menu
    nl_rv$in_flight <- TRUE
    output$nl_spinner <- renderUI(div(class = "nl-spinner", "Thinking\u2026"))

    # Pick the loans df based on scope
    loans_df <- if (scope == "whole") {
      loan_data
    } else {
      rx <- nl_get_current_view(session, tab_id)
      if (is.null(rx)) loan_data else tryCatch(rx(), error = function(e) loan_data)
    }
    scope_label <- if (scope == "whole") "Whole portfolio" else "Current view"

    answer <- nl_answer(query, loans_df, scope_label)
    answer$tab_id <- tab_id
    nl_rv$answer <- answer

    nl_log_submission(list(
      ts         = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      tab        = tab_id,
      scope      = scope,
      query      = query,
      shape      = answer$shape %||% NA,
      confidence = answer$confidence,
      answered   = identical(answer$confidence, "high"),
      latency_ms = answer$latency_ms
    ))
    output$nl_spinner <- renderUI(NULL)
    nl_rv$in_flight <- FALSE
  })

  observeEvent(input$nl_dismiss, { nl_rv$answer <- NULL })

  output$nl_answer_card <- renderUI({
    if (is.null(nl_rv$answer)) return(NULL)
    nl_answer_card(nl_rv$answer)
  })

  # ---- Server-side upload size limit (50 MB) ----
  options(shiny.maxRequestSize = 50 * 1024^2)

  # Dismiss any open modal (and clean orphaned backdrops) when the user
  # switches tabs — prevents Reprice / other modals from leaking across
  # tabs and blocking clicks on the destination.
  observeEvent(input$sidebar_menu, {
    removeModal()
    shinyjs::runjs(
      "document.querySelectorAll('.modal-backdrop').forEach(function(el){el.remove();});
       document.body.classList.remove('modal-open');
       document.body.style.overflow = '';
       document.body.style.paddingRight = '';"
    )
  }, ignoreInit = TRUE)

  # ---- Shared reactive state (passed to tab modules) ----

  # Performance tape — loaded eagerly at startup by R/02_globals_data.R
  # (CSM_MAX_PROJECT_LOANS=50000 enables projection so xirr converges on
  # young PL-CURRENT vintages). Shared across all tabs via perf_tape reactiveVal.
  perf_tape <- reactiveVal(perf_tape_global)

  # Policy repository — shared between Opp Finder, What-If, Policy Repo
  added_policy <- reactiveVal(persisted_policies)
  policy_fns_rv <- reactiveVal(policy_fns)

  # ---- Policy status overrides -------------------------------------------
  # Map of policyId -> status. Used by the "Set as Active" action in the
  # Policy Repository so that promoting a saved policy (or any manifest
  # policy) flips the active/retired flags app-wide without mutating the
  # underlying mockPolicies frame. Overrides are persisted alongside saved
  # policies in saved_policies.rds (new `status_overrides` field).
  .init_overrides <- tryCatch({
    if (file.exists(saved_policies_path)) {
      .st <- readRDS(saved_policies_path)
      if (is.list(.st) && !is.null(.st$status_overrides)) .st$status_overrides else list()
    } else list()
  }, error = function(e) list())
  policy_status_overrides <- reactiveVal(.init_overrides)

  # Expose shared reactives on session$userData so any tab server can read
  # them without changing every tab signature.
  session$userData$policy_status_overrides <- policy_status_overrides
  session$userData$added_policy            <- added_policy

  # Loan-level IRR cache (pre-loaded from RDS, or computed once per tape load)
  loan_irr_cache <- reactive({
    if (!is.null(irr_cache_global)) return(irr_cache_global)
    tape <- perf_tape()
    if (is.null(tape)) return(NULL)
    compute_loan_irr_v2(tape)
  })

  # ---- Populate vintage quarter picker when tape changes ----
  observeEvent(perf_tape(), {
    tape <- perf_tape()
    if (!is.null(tape) && length(tape$vintages) > 0) {
      vqs <- tape$vintages
      default_vqs <- tail(vqs, min(4, length(vqs)))
      updatePickerInput(session, "opp_vintage_quarters",
                        choices = vqs, selected = default_vqs)
    } else {
      updatePickerInput(session, "opp_vintage_quarters",
                        choices = character(0), selected = character(0))
    }
  }, ignoreNULL = FALSE)

  # ---- Populate filter choices from actual data ----
  # Note: Opportunity Explorer filters (opp_fico, opp_partner, opp_repeat, opp_mob)
  # are set directly in the UI via pre-computed globals from 02_globals_data.R.
  # This observer handles remaining tabs that still need dynamic population.
  observe({
    for (id in c("vs_fico_range", "rd_fico_range", "opp_perf_fico")) {
      updatePickerInput(session, id,
        choices  = c("All" = "all",
                     setNames(filter_fico_bands, filter_fico_bands)),
        selected = "all")
    }
    for (id in c("vs_partner", "rd_partner", "opp_perf_partner")) {
      updatePickerInput(session, id,
        choices  = c("All" = "all",
                     setNames(filter_partners, filter_partners)),
        selected = "all")
    }
    for (id in c("vs_repeat_type", "rd_repeat_type", "opp_perf_repeat")) {
      updatePickerInput(session, id,
        choices  = c("All" = "all",
                     setNames(filter_repeat_types, filter_repeat_types)),
        selected = "all")
    }
    for (id in c("opp_perf_mob")) {
      updatePickerInput(session, id,
        choices  = c("All" = "all",
                     setNames(filter_mob_bins, filter_mob_bins)),
        selected = "all")
    }
  })

  # ---- Call tab server modules ----

  opp_find_server(input, output, session,
                  credit_data, policy_input_data, perf_tape,
                  added_policy, policy_fns_rv, loan_irr_cache)

  whatif_server(input, output, session,
                credit_data, policy_input_data, perf_tape,
                added_policy, policy_fns_rv)

  policy_editor_server(input, output, session,
                       credit_data, perf_tape, policy_input_data,
                       loan_irr_cache)

  var_analysis_server(input, output, session,
                      credit_data, perf_tape, loan_irr_cache)

  risk_driver_server(input, output, session,
                     credit_data, perf_tape, loan_irr_cache)

  smart_recommender_server(input, output, session,
                           credit_data, policy_input_data,
                           loan_irr_cache, perf_tape,
                           added_policy, policy_fns_rv)

  policy_repo_server(input, output, session,
                     credit_data, policy_input_data, perf_tape,
                     added_policy, policy_fns_rv)

  policy_history_server(input, output, session,
                        credit_data, policy_input_data, perf_tape, policy_fns_rv)

  data_upload_server(input, output, session, perf_tape)

  data_upload_vs_server(input, output, session, perf_tape)

  portfolio_insights_server(input, output, session,
                            credit_data, perf_tape)

  irr_engine_server(input, output, session, perf_tape)

  root_cause_server(input, output, session,
                    credit_data, perf_tape, loan_irr_cache)

  # ---- AI Insights tab servers (hidden for demo) ----
  # counterfactual_server(input, output, session,
  #                       credit_data, policy_input_data, loan_irr_cache)
  #
  # fair_lending_server(input, output, session,
  #                     credit_data, policy_input_data,
  #                     added_policy, policy_fns_rv)

  # ---- AI Analytics tab servers (only when AI mode is on) ----
  if (ai_mode) {
    ai_shap_server(input, output, session,
                   credit_data, loan_irr_cache)

    ai_variable_server(input, output, session,
                       credit_data, loan_irr_cache)
  }
}

# ============================================================
# RUN
# ============================================================
shinyApp(ui = ui, server = server)
