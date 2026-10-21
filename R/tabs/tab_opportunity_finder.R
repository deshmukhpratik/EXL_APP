# ============================================================
# Tab: Opportunity Explorer
# Extracted from app.R — UI and server logic
# ============================================================

opp_find_ui <- function() {
      tabItem(tabName = "opp_find",

        tags$style(HTML("
          @keyframes pulse { 0%,100% { opacity:0.6; transform:scale(0.95); } 50% { opacity:1; transform:scale(1.05); } }
        ")),

        header_panel(
          "Opportunity Agent",
          "Zoom into the riskiest segments and act fast: Decline or Price, benchmarked to Baseline.",
          icon_name = "magnifying-glass-dollar",
          data_scope = "issued_perf"
        ),

        # Product selector
        fluidRow(column(12,
          product_selector_ui("opp")
        )),

        # Active policy banner (reflects current active per product)
        active_policy_card_ui("opp"),

        # Data scope toggle (Applications vs Issued)
        fluidRow(column(12,
          div(style = "background:#f3f4f6; padding:10px 15px; border-radius:6px; margin-bottom:12px;",
              tags$b("Data scope:"),
              radioButtons("opp_data_scope", label = NULL,
                           choices = c("Applications" = "applications", "Issued" = "issued"),
                           selected = "issued", inline = TRUE))
        )),

        # Strategy mode selector
        fluidRow(column(12, div(class = "card",
          radioGroupButtons("opp_strategy_mode", "Objective Function",
            # NOTE: "Reprice Premium Low-Take (Growth Scout)" = "growth_scout"
            # is hidden from the picker for now (panel + server logic remain
            # in place, just unreachable from the UI). Re-add the choice to
            # restore.
            choices = c("Minimize Delinquency (Swap-Out)" = "swapout_delinq",
                        "Maximize IRR (Swap-Out)"         = "swapout_irr",
                        "Expand (Swap-In)"                = "swapin"),
            selected = "swapout_delinq",
            justified = TRUE,
            status = "warning")
        ))),

        # Carried-over Scanner segment (shown only after Deeper cuts handoff)
        fluidRow(column(12, uiOutput("opp_carried_segment"))),

        # Filter row — Portfolio Segment definition
        fluidRow(column(12, div(class = "card",
          h4("Portfolio Segment", style = "font-weight:bold; margin-bottom:2px;"),
          p("Define the portfolio universe for analysis. All filters below narrow the population used for segmentation.",
            style = "color:#9ca3af; font-size:12px; margin-bottom:10px;"),
          fluidRow(
            column(2, dateRangeInput("opp_date_range", "Vintage Date Range",
              start = vs_date_min, end = vs_date_max,
              min = vs_date_min, max = vs_date_max,
              format = "dd/mm/yyyy")),
            column(2, pickerInput("opp_fico", "FICO Band",
              choices = c("All" = "all", setNames(filter_fico_bands, filter_fico_bands)),
              selected = "all",
              multiple = TRUE, options = list(`actions-box` = TRUE))),
            column(2, pickerInput("opp_grade", "Grade",
              choices  = c("All" = "all", "High", "Med", "Low"),
              selected = "all",
              multiple = TRUE, options = list(`actions-box` = TRUE))),
            column(2, pickerInput("opp_partner", "Partner",
              choices = c("All" = "all", setNames(filter_partners, filter_partners)),
              selected = "all",
              multiple = TRUE, options = list(`actions-box` = TRUE))),
            column(2, pickerInput("opp_repeat", "Repeat Type",
              choices = c("All" = "all", setNames(filter_repeat_types, filter_repeat_types)),
              selected = "all",
              multiple = TRUE, options = list(`actions-box` = TRUE))),
            column(2, tags$div(
              pickerInput("opp_mob", "Months on Books",
                choices = c("All" = "all", setNames(filter_mob_bins, filter_mob_bins)),
                selected = "all",
                multiple = TRUE, options = list(`actions-box` = TRUE)),
              tags$small("Seasoning filter (issued loans only)",
                style = "color:#6b7280; font-size:11px; margin-top:-8px; display:block;")
            ))
          )
        ))),

        # ML segmentation — always visible
        div(id = "opp_rules_display",
          fluidRow(column(12, div(class = "card",
            fluidRow(
              column(3, h4("Risk Segmentation",
                style = "font-weight:bold; margin:0; padding-top:6px;")),
              column(6, pickerInput("opp_vars",
                NULL,
                choices  = analysis_vars,
                # Default: pre-select variables USED IN THE POLICY (PL-CURRENT decline
                # waterfall + grading + affordability). Users can override via the picker.
                selected = intersect(
                  c(
                    "fico_score",
                    "credit_file_age_months",
                    "delinquent_accounts_count",
                    "debt_to_income_ratio",
                    "N_HARD_INQ_6M",
                    "annual_income",
                    "monthly_debt_payment",
                    "risk_model_score_1"
                  ),
                  analysis_vars
                ),
                multiple = TRUE,
                options  = list(
                  `actions-box`         = TRUE,
                  `live-search`         = TRUE,
                  `selected-text-format` = "count > 3",
                  `count-selected-text`  = "{0} variables selected",
                  `deselect-all-text`    = "None",
                  `select-all-text`      = "All",
                  # bootstrap-select: checkmark indicators in the dropdown list
                  # so each item visibly reads as a checkbox.
                  `show-tick`            = TRUE
                )))
            ),
            # Hero badge — same magnifying-glass-dollar glyph used in the
            # Opportunity Agent sidebar / header so the visual identity is
            # consistent across the tab.
            div(style = "text-align:center; margin-top:14px; margin-bottom:12px;",
              div(style = paste0(
                    "display:inline-flex; align-items:center; justify-content:center;",
                    " width:120px; height:120px; border-radius:50%;",
                    " background:radial-gradient(circle at 30% 30%, #ff8a5a, #fb6e36 65%, #d65117 100%);",
                    " box-shadow:0 8px 24px rgba(251,110,54,0.35), inset 0 -4px 10px rgba(0,0,0,0.18);",
                    " margin-bottom:14px;"),
                icon("magnifying-glass-dollar", style = "color:white; font-size:50px;")
              ),
              br(),
              actionButton("submit_opp", "Run Segmentation",
                icon = icon("wand-magic-sparkles"),
                class = "custom-button")
            ),
            # AI action overlay (pulsing image while segmentation runs)
            shinyjs::hidden(div(id = "opp_action_overlay",
              style = "position:fixed; top:0; left:0; width:100%; height:100%;
                       background:rgba(0,0,0,0.5); z-index:9999;
                       display:flex; align-items:center; justify-content:center;
                       flex-direction:column;",
              div(style = paste0(
                    "display:inline-flex; align-items:center; justify-content:center;",
                    " width:90px; height:90px; border-radius:50%;",
                    " background:radial-gradient(circle at 30% 30%, #ff8a5a, #fb6e36 65%, #d65117 100%);",
                    " box-shadow:0 8px 24px rgba(251,110,54,0.35);",
                    " animation: pulse 1.5s ease-in-out infinite;"),
                icon("magnifying-glass-dollar", style = "color:white; font-size:36px;")
              ),
              tags$p("Analyzing portfolio segments...", style = "color:#fb6e36; font-size:16px; margin-top:16px; font-weight:600;")
            )),
            # Overall portfolio stats bar (shown after segmentation)
            uiOutput("opp_overall_stats"),
            # Drill-down context subtitle (shown only when Deeper cuts active)
            uiOutput("opp_drilldown_subtitle"),
            DTOutput("segment_opp"),
            # Lift explanation footnote (shown after segmentation)
            uiOutput("opp_lift_footnote")
          )))
        ),

        # Swap-In panel (hidden unless strategy mode = swapin)
        hidden(div(id = "opp_swapin_panel",
          fluidRow(column(12, div(class = "card",
            h4("Swap-In Analysis: Expand Into Declined Population",
               style = "font-weight:bold;"),
            p("Identify declined applicants near policy boundaries and project their IRR if approved.",
              style = "color:#9ca3af; margin-bottom:12px;"),
            fluidRow(
              column(4, selectInput("opp_swapin_reason", "Decline Reason",
                choices = c("01. Fico"                = "01. Fico",
                            "02. Thin file"           = "02. Thin file",
                            "03. Delinquency decline" = "03. Delinquency decline",
                            "04. HighDTI"             = "04. HighDTI",
                            "05. HardInquiry"         = "05. HardInquiry"),
                selected = "01. Fico")),
              column(4, uiOutput("opp_swapin_relax_ui")),
              column(4, style = "padding-top:25px;",
                actionButton("opp_swapin_analyze", "Analyze Boundary",
                  icon = icon("magnifying-glass"), class = "custom-button"))
            ),
            fluidRow(
              column(6, style = "padding-top:4px;",
                uiOutput("opp_swapin_n_total_declined")),
              column(6, style = "padding-top:4px;",
                uiOutput("opp_swapin_n_declined"))
            )
          ))),

          # Analysis pipeline (visible during analyze computation)
          pipeline_container("opp_swapin_pipeline"),

          # Neighbor band metrics (shown after analyze)
          hidden(div(id = "opp_swapin_neighbor_panel",
            # Neighbor band definition + explanation
            fluidRow(column(12,
              div(style = "padding:0 15px; margin-bottom:8px;",
                uiOutput("opp_swapin_neighbor_def"),
                tags$details(
                  style = "margin-top:6px; font-size:12px; color:#9ca3af;",
                  tags$summary(style = "cursor:pointer; color:#fb6e36; font-weight:600;",
                    "What is a Neighbor Band?"),
                  tags$div(style = "padding:6px 0;",
                    tags$p("The ", tags$b("neighbor band"), " is the group of ",
                      tags$em("already issued"), " loans whose characteristics are closest to the decline threshold. ",
                      "These loans were approved just above the cutoff and serve as the best available proxy ",
                      "for how newly eligible (swapped-in) applicants would perform."),
                    tags$p("The metrics below (delinquency rate, IRR, LGD) come from this neighbor band. ",
                      "They are used as the default assumption for projecting the swap-in cohort's performance. ",
                      "You can override the assumed delinquency rate in the Projection Settings below.")
                  )
                ))
            )),
            tags$div(style = "display:flex; gap:10px; padding:0 15px; margin-bottom:10px;",
              tags$div(style = "flex:1; min-width:0;", uiOutput("opp_swapin_kpi_n")),
              tags$div(style = "flex:1; min-width:0;", uiOutput("opp_swapin_kpi_apr")),
              tags$div(style = "flex:1; min-width:0;", uiOutput("opp_swapin_kpi_delinq")),
              tags$div(style = "flex:1; min-width:0;", uiOutput("opp_swapin_kpi_elasticity")),
              tags$div(style = "flex:1; min-width:0;", uiOutput("opp_swapin_kpi_irr")),
              tags$div(style = "flex:1; min-width:0;", uiOutput("opp_swapin_kpi_mtd"))
            ),

            fluidRow(column(12, div(class = "card",
              h4("Projection Settings", style = "font-weight:bold;"),
              p("Configure the assumed delinquency rate and optional pricing adjustment for the swap-in cohort.",
                style = "color:#9ca3af; margin-bottom:12px;"),
              fluidRow(
                column(3, numericInput("opp_swapin_delinq_assumed",
                  "Assumed Delinquency Rate (%)", value = 5, min = 0.1,
                  max = 100, step = 0.5)),
                column(3, numericInput("opp_swapin_pricing_bps",
                  "Pricing Adjustment (BPS)", value = 0, min = -500,
                  max = 5000, step = 25)),
                column(3, numericInput("opp_swapin_obs_months",
                  "Observation Window (Months)", value = 36, min = 12,
                  max = 72, step = 6)),
                column(3, style = "padding-top:25px;",
                  actionButton("opp_swapin_simulate", "Simulate IRR",
                    icon = icon("play"), class = "custom-button"))
              )
            ))),

            # Swap-in simulation results
            hidden(div(id = "opp_swapin_results_panel",
              tags$div(style = "display:flex; gap:10px; padding:0 15px; margin-bottom:10px;",
                tags$div(style = "flex:1; min-width:0;", uiOutput("opp_swapin_res_volume")),
                tags$div(style = "flex:1; min-width:0;", uiOutput("opp_swapin_res_apr")),
                tags$div(style = "flex:1; min-width:0;", uiOutput("opp_swapin_res_delinq")),
                tags$div(style = "flex:1; min-width:0;", uiOutput("opp_swapin_res_irr")),
                tags$div(style = "flex:1; min-width:0;", uiOutput("opp_swapin_res_blended"))
              ),
              fluidRow(
                column(12, div(class = "card",
                  h4("Swap-In Cohort Summary", style = "font-weight:bold;"),
                  DTOutput("opp_swapin_summary_table")
                ))
              ),
              # SQL Policy Code Preview for Swap-In
              fluidRow(column(12, div(
                style = "padding: 4px 15px 8px; display:flex; gap:10px;",
                actionButton("opp_swapin_sql_baseline", "Baseline Policy (SQL)",
                  icon  = icon("database"),
                  style = "background-color:#334155; color:white; border:none; padding:6px 14px; border-radius:4px; font-size:13px;"),
                actionButton("opp_swapin_sql_new", "Swap-In Policy (SQL)",
                  icon  = icon("code"),
                  style = "background-color:#f97316; color:white; border:none; padding:6px 14px; border-radius:4px; font-size:13px;")
              ))),
              # Save swap-in policy to repository
              fluidRow(column(12, div(class = "card",
                h4("Save Swap-In Policy to Repository", style = "font-weight:bold;"),
                fluidRow(
                  column(5, textInput("opp_swapin_save_name", "Policy Name",
                    placeholder = "e.g., DTI Relaxation +5pp")),
                  column(3, style = "padding-top:25px;",
                    actionButton("opp_swapin_save_btn", "Save to Repository",
                      icon = icon("save"),
                      style = "background-color:#10b981; color:white; border:none; padding:6px 14px; border-radius:4px; font-size:13px; width:100%;"))
                )
              )))
            ))
          ))
        )),

        # Growth Scout panel — visibility managed by conditionalPanel so the
        # DataTables widget binds to a visible DOM (avoids the well-known
        # "DT in hidden div" init failure where the table never paints rows).
        conditionalPanel(
          condition = "input.opp_strategy_mode == 'growth_scout'",
        div(id = "opp_growth_panel",
          fluidRow(column(12, div(class = "card",
            h4("Growth Scout — Premium Customers Who Didn't Take The Offer",
               style = "font-weight:bold;"),
            p("Strong-credit applicants (high FICO, top quality grade) who declined the offer — they likely got better pricing elsewhere. Drop your price on these segments to win them back. Min Segment IRR is a soft guard: premium loans naturally yield lower than the overall portfolio, so leave it at 0 to see all candidates and dial up only if you want to exclude the thinnest-margin cells.",
              style = "color:#9ca3af; margin-bottom:12px;"),
            fluidRow(
              column(2, numericInput("opp_growth_min_fico",
                "Min FICO",
                value = 700, min = 600, max = 850, step = 10)),
              column(3, pickerInput("opp_growth_grades",
                "Quality grade(s)",
                choices  = c("High", "Med", "Low"),
                selected = "High",
                multiple = TRUE,
                options  = list(`actions-box` = TRUE))),
              column(2, numericInput("opp_growth_min_n",
                "Min issued loans",
                value = 50, min = 10, step = 10)),
              column(2, numericInput("opp_growth_hurdle",
                "Min Segment IRR (%)",
                value = 0, min = 0, max = 50, step = 0.5)),
              column(3, style = "padding-top:25px;",
                actionButton("opp_growth_run",
                  "Find Premium Low-Take Segments",
                  icon = icon("magnifying-glass-chart"),
                  class = "custom-button"))
            ),
            br(),
            DTOutput("opp_growth_table"),
            tags$div(style = "font-size:11px; color:#9ca3af; padding:6px 4px;",
              tags$em("Cross-tab is restricted to applicants meeting your profile criteria. ",
                      "Sorted by take rate ascending — the lowest-take cells are the strongest price-cut candidates. ",
                      "Take Gap vs Premium = how far below the premium-pool average this cell sits. ",
                      "Diagnostic only — no recommended price change, no auto-apply."))
          ))),
          fluidRow(column(12, div(class = "card",
            h4("Discover Hidden Premium Segments (AI)", style = "font-weight:bold;"),
            p("Tree-based search inside the premium pool for non-obvious combinations with the lowest take rate.",
              style = "color:#9ca3af; margin-bottom:12px;"),
            actionButton("opp_growth_discover", "Discover with AI",
              icon = icon("wand-magic-sparkles"),
              class = "custom-button"),
            br(), br(),
            DTOutput("opp_growth_cart_table"),
            tags$div(style = "font-size:11px; color:#9ca3af; padding:6px 4px;",
              tags$em("CART regression tree on the filtered premium pool, predicting probability of taking the offer. Leaves with the lowest take rate surface first."))
          )))
        )  # end div opp_growth_panel
        ),  # end conditionalPanel

        # Apply action (hidden until segment generated)
        hidden(div(id = "opp_action_row",
          fluidRow(column(12, div(class = "card",
            h4("Apply Policy Action to Selected Segment",
               style = "font-weight:bold;"),
            tags$div(style = "display:flex; align-items:flex-end; gap:12px; flex-wrap:wrap;",
              tags$div(style = "flex:0 0 auto; padding-bottom:15px;",
                actionButton("opp_decline_btn", "Decline Segment",
                  icon = icon("ban"), class = "btn-danger",
                  style = "margin-right:4px;")),
              tags$div(style = "flex:0 0 auto; padding-bottom:15px;",
                actionButton("opp_reprice_btn", "Reprice Segment",
                  icon = icon("dollar-sign"), class = "btn-warning")),
              tags$div(id = "opp_pricing_opts",
                style = "display:none; flex:1 1 auto;",
                tags$div(style = "display:flex; align-items:flex-end; gap:12px; flex-wrap:wrap;",
                  tags$div(style = "min-width:130px; flex:0 0 auto;",
                    selectInput("opp_pricing_dec", "Pricing (BPS)",
                      choices = c("None", "50", "100", "150", "Custom"),
                      selected = "None", width = "100%")),
                  conditionalPanel(
                    condition = "input.opp_pricing_dec == 'Custom'",
                    style = "min-width:100px; flex:0 0 auto;",
                    numericInput("opp_pricing_custom", "Custom BPS",
                      value = 75, min = 1, max = 5000, step = 25, width = "100%")),
                  tags$div(style = "flex:0 0 auto; padding-bottom:15px;",
                    actionButton("opp_apply_reprice_btn", "Apply Reprice",
                      icon = icon("check"), class = "btn-warning"))
                )
              )
            ),
            fluidRow(style = "margin-top:8px;",
              column(4, radioGroupButtons("opp_sim_slot", "Save As",
                choices = c("Primary Action" = "A", "Alternative Action" = "B"),
                selected = "A", size = "sm", status = "warning")),
              column(8, style = "padding-top:25px;",
                div(style = "display:flex; gap:16px; align-items:center;",
                  actionLink("opp_clear_A", label = tagList(icon("trash"), " Clear Primary"),
                    style = "color:#dc2626; font-size:12px; display:inline-block;"),
                  actionLink("opp_clear_B", label = tagList(icon("trash"), " Clear Alternative"),
                    style = "color:#dc2626; font-size:12px; display:inline-block;")))
            )
          )))
        )),

        # Root Cause Analysis (UC1) — shown when action panel is visible
        uiOutput("opp_root_cause"),

        # Sample Preview (hidden until action applied)
        hidden(div(id = "opp_sample_preview_panel",
          uiOutput("opp_sample_preview_cards")
        )),

        # Performance (hidden until action applied)
        hidden(div(id = "opp_perf_show",
          uiOutput("opp_scenario_summary"),
          # Action Reasoning (UC4)
          uiOutput("opp_action_reasoning"),
          # Dynamic KPI panel — shows 1 or 2 rows depending on sim slots
          uiOutput("opp_kpi_panel"),
          # Analysis filters
          fluidRow(column(12, div(class = "card",
            h4("Analysis Filters", style = "font-weight:bold;"),
            fluidRow(
              column(2, pickerInput("opp_perf_fico", "FICO Band",
                choices = c("All" = "all"), selected = "all",
                multiple = TRUE, options = list(`actions-box` = TRUE))),
              column(3, pickerInput("opp_perf_partner", "Partner Channel",
                choices = c("All" = "all"), selected = "all",
                multiple = TRUE, options = list(`actions-box` = TRUE))),
              column(2, pickerInput("opp_perf_repeat", "Repeat Type",
                choices = c("All" = "all"), selected = "all",
                multiple = TRUE, options = list(`actions-box` = TRUE))),
              column(2, pickerInput("opp_perf_mob", "Months on Books",
                choices = c("All" = "all"), selected = "all",
                multiple = TRUE, options = list(`actions-box` = TRUE))),
              column(3, pickerInput("opp_perf_vintage", "Vintage Cohort",
                choices = c("All" = "all"), selected = "all",
                multiple = TRUE, options = list(`actions-box` = TRUE)))
            )
          ))),
          # Funnel metrics table + IRR chart side by side
          fluidRow(
            column(6, div(class = "card",
              h4("Funnel Metrics", style = "font-weight:bold; margin-bottom:8px;"),
              uiOutput("opp_funnel_subtitle"),
              uiOutput("opp_funnel_table")
            )),
            column(6, div(class = "card",
              uiOutput("opp_irr_title"),
              selectInput("opp_irr_level", NULL,
                choices = c("Overall", "FICO Band" = "fico_band",
                            "Partner Channel" = "partner_channel",
                            "Grade" = "risk_grade",
                            "Repeat Type" = "borrower_repeat_type"),
                selected = "partner_channel", width = "200px"),
              plotlyOutput("opp_irr_plot", height = "380px")))
          ),
          br(),
          # Vintage curves - side by side per scenario
          fluidRow(
            column(12, div(class = "card",
              h4("Vintage Performance Curves", style = "font-weight:bold;"),
              pickerInput("opp_vintage_quarters", "Select Vintages:",
                choices = NULL, selected = NULL, multiple = TRUE,
                options = list(`actions-box` = TRUE, `live-search` = TRUE,
                               `selected-text-format` = "count > 3",
                               `count-selected-text` = "{0} vintages selected"),
                width = "100%"),
              uiOutput("opp_vintage_layout")
            ))
          ),
          br(),
          # Save Policy to Repository
          fluidRow(column(12, div(class = "card", style = "padding:15px;",
            h4("Save Policy to Repository", style = "font-weight:bold;"),
            p("Save a simulated policy action to the repository for comparison.",
              style = "color:#9ca3af; margin-bottom:12px;"),
            fluidRow(
              column(12,
                textInput("opp_save_name", "Policy Name",
                  placeholder = "e.g., High-Risk Segment Decline")),
              column(12,
                div(style = "display:flex; gap:10px; padding-top:10px;",
                  div(style = "flex:1;",
                    actionButton("opp_save_btn_A", "Save Primary",
                      icon = icon("save"), class = "btn-warning btn-sm", width = "100%")),
                  div(style = "flex:1;",
                    actionButton("opp_save_btn_B", "Save Alternative",
                      icon = icon("save"), class = "btn-info btn-sm", width = "100%")),
                  div(style = "flex:1;",
                    downloadButton("opp_download_policy_B", "SQL Alternative",
                      class = "btn-sm",
                      style = "background-color:#0ea5e9; color:white; width:100%;"))
                ))
            )
          )))
        ))
  )
}

opp_find_server <- function(input, output, session,
                            credit_data, policy_input_data, perf_tape,
                            added_policy, policy_fns_rv, loan_irr_cache) {

  # ---- Active policy banner under product selector ----
  wire_active_policy_card(input, output, session, "opp")

  # ---- Opportunity Finder reactive state ----
  opp_rv <- reactiveValues(
    df_full       = NULL,
    df_proposed   = NULL,
    in_segment    = NULL,
    action        = NULL,
    bl            = NULL,
    pr            = NULL,
    seg_size      = 0,
    seg_appr      = 0,
    bl_seg_issued = 0,
    pr_seg_issued = 0,
    bl_seg_apr    = NA_real_,
    pr_seg_apr    = NA_real_,
    seg_metrics   = NULL,
    bps           = 0,
    segment_condition = NULL,
    strategy_mode     = NULL,
    saved_policy_id   = NULL,
    saved_policy_name = NULL,
    # Swap-In state
    swapin_neighbor     = NULL,
    swapin_declined_n   = 0,
    swapin_cohort       = NULL,
    swapin_projected_cf = NULL,
    swapin_projected_irr = NA_real_,
    # Engine-based swap-in state (full L10-L90 cascade)
    swapin_baseline_df    = NULL,
    swapin_scenario_df    = NULL,
    swapin_newly_eligible = NULL,
    swapin_relaxed_params = NULL,
    # Sample preview state
    sample_slot = "A",
    # Dual simulation comparison
    sim_A = NULL,
    sim_B = NULL
  )

  # ---- Deeper-cuts carried Scanner segment ----
  # Scanner stashes (rule text, loan_ids) into session$userData before
  # navigating here. The rule text is for display only; the loan_ids are the
  # authoritative segment membership (rule text is a simplified CART path and
  # can match MORE loans than the actual leaf). Poll for both on session flush.
  deeper_rule_rv     <- reactiveVal(NULL)
  deeper_loan_ids_rv <- reactiveVal(NULL)

  observe({
    rule <- isolate(session$userData$deeper_cuts_rule)
    ids  <- isolate(session$userData$deeper_cuts_loan_ids)
    if (!is.null(rule) && nzchar(rule)) {
      deeper_rule_rv(rule)
      deeper_loan_ids_rv(ids)
      session$userData$deeper_cuts_rule     <- NULL
      session$userData$deeper_cuts_loan_ids <- NULL
    }
    invalidateLater(600)
  })

  output$opp_carried_segment <- renderUI({
    rule <- deeper_rule_rv()
    if (is.null(rule) || !nzchar(rule)) return(NULL)
    # Prefer exact loan_ids from Scanner (authoritative CART leaf membership);
    # fall back to rule-text parse only if ids weren't stashed. Intersect the
    # carried-segment loan_ids with the page's UI filter mask so that filter
    # changes (repeat-type / FICO / grade / partner / MOB) narrow the carried
    # population. On arrival, all filters default to "all" so the intersection
    # equals the full carried segment and the count still matches Scanner.
    ids <- deeper_loan_ids_rv()
    if (!is.null(ids) && length(ids) > 0 && "loan_id" %in% names(credit_data)) {
      pop_keep <- opp_pop_keep()
      matched  <- credit_data[pop_keep & as.character(credit_data$loan_id) %in% ids, ,
                              drop = FALSE]
      if ("issued_flag" %in% names(matched))
        matched <- matched[matched$issued_flag == 1, , drop = FALSE]
      # Universe for "% of book" = currently filtered, issued universe so the
      # percentage stays meaningful as filters narrow.
      book_mask <- pop_keep
      if ("issued_flag" %in% names(credit_data))
        book_mask <- book_mask & credit_data$issued_flag == 1
      n <- nrow(matched); n_full <- sum(book_mask, na.rm = TRUE)
    } else {
      df <- tryCatch(opp_filtered(), error = function(e) NULL)
      if (is.null(df) || nrow(df) == 0) return(NULL)
      if ("issued_flag" %in% names(df)) df <- df %>% dplyr::filter(issued_flag == 1)
      matched <- tryCatch(df %>% dplyr::filter(eval(parse(text = rule))),
                          error = function(e) df[FALSE, , drop = FALSE])
      n <- nrow(matched); n_full <- nrow(df)
    }
    bad <- if (n > 0 && "performance_delinquency_flag" %in% names(matched))
             mean(as.numeric(matched$performance_delinquency_flag == 1), na.rm = TRUE) else NA_real_
    apr <- if (n > 0 && "interest_rate_apr" %in% names(matched))
             mean(matched$interest_rate_apr, na.rm = TRUE) else NA_real_
    div(class = "card",
      style = "padding:14px; background:#fff7ed; border-left:4px solid #fb6e36;",
      div(style = "display:flex; justify-content:space-between; align-items:center;",
          div(
            tags$b("Drill-down active: Scanner segment carried over"),
            tags$div(style = "font-size:12px; color:#6b7280; margin-top:2px;",
                     "Run Segmentation will split this segment further, not the full book."),
            tags$div(style = "font-family:monospace; font-size:12px; color:#6b7280; margin-top:4px;",
                     rule)),
          actionButton("opp_clear_deeper", "Exit drill-down",
                       class = "btn-sm", icon = icon("xmark"))),
      fluidRow(style = "margin-top:10px;",
        column(3, kpi_box(scales::comma(n), "Loans matched")),
        column(3, kpi_box(scales::percent(n / max(n_full, 1), 0.1), "% of book")),
        column(3, kpi_box(if (!is.na(bad)) scales::percent(bad, 0.1) else "\u2014", "Bad rate")),
        column(3, kpi_box(if (!is.na(apr)) scales::percent(apr, 0.1) else "\u2014", "Avg APR")))
    )
  })
  outputOptions(output, "opp_carried_segment", suspendWhenHidden = FALSE)

  observeEvent(input$opp_clear_deeper, {
    deeper_rule_rv(NULL)
    deeper_loan_ids_rv(NULL)
  })

  # ---- Helper: build baseline params with PL-CURRENT overrides applied ----
  # Matches the PL-CURRENT merge pattern used in Scanner and What-If so that
  # every Opportunity Explorer engine run starts from the same baseline.
  opp_build_baseline_params <- function() {
    dp <- build_default_params(policy_manifest)
    active_pid <- get_active_policy_id(opp_selected_product())
    .so <- get0("strategy_overrides", envir = .GlobalEnv, inherits = FALSE)
    if (!is.null(.so) && !is.null(.so[[active_pid]])) {
      dp <- modifyList(dp, .so[[active_pid]]$params)
    }
    dp
  }

  # NOTE: "Deeper cuts" handoff is now implemented directly in the Scanner
  # (tab_smart_recommender.R) via updatePickerInput() + updateTabItems() —
  # no listener needed here. The previous session$userData reactiveVal
  # approach had an init-order race where this observer subscribed before
  # the Scanner had created the reactiveVal, so nothing invalidated on
  # write. The direct-update pattern mirrors sr_goto_opp and works.

  # ---- Product selector reactiveVal ----
  opp_selected_product <- reactiveVal("Personal Loan")

  observeEvent(input$opp_product, {
    val <- input$opp_product
    if (!is.null(val) && val != "" && val != "All") {
      opp_selected_product(val)
    } else {
      opp_selected_product(NULL)
    }
  }, ignoreInit = TRUE)

  # ---- Active policy label for swap-in panel ----
  output$opp_swapin_active_policy_label <- renderText({
    pid <- get_active_policy_id(opp_selected_product())
    .so <- get0("strategy_overrides", envir = .GlobalEnv, inherits = FALSE)
    if (!is.null(.so) && !is.null(.so[[pid]])) {
      paste0(pid, " \u2014 ", .so[[pid]]$name)
    } else {
      pid
    }
  })

  # Helper: persist added policies and their closures to disk
  persist_policies <- function() {
    pol_df <- added_policy()
    if (is.null(pol_df) || nrow(pol_df) == 0) return()
    all_fns <- policy_fns_rv()
    saved_fns <- list()
    for (pid in pol_df$policyId) {
      if (!is.null(all_fns[[pid]])) saved_fns[[pid]] <- all_fns[[pid]]
    }
    tryCatch({
      saveRDS(list(policies = pol_df, fns = saved_fns), saved_policies_path)
      persisted_policies <<- pol_df
      persisted_fns      <<- saved_fns
      message("[persist] Saved ", nrow(pol_df), " policies to ", saved_policies_path)
    }, error = function(e) {
      message("[persist] Failed to save: ", e$message)
    })
  }

  # TAB 1 – OPPORTUNITY FINDER
  # ============================================================

  # Boolean mask for population filters — usable on both credit_data and
  # policy_input_data (same row order).  Mirrors the What-If pattern.
  opp_pop_keep <- reactive({
    keep <- rep(TRUE, nrow(credit_data))
    # Filter to active policy for the selected product.
    if ("policy_id" %in% names(credit_data)) {
      active_pid <- get_active_policy_id(opp_selected_product())
      keep <- keep & (credit_data$policy_id == active_pid)
    }
    if (!is.null(opp_selected_product()))
      keep <- keep & (credit_data$product_type == opp_selected_product())
    if (!is.null(input$opp_date_range))
      keep <- keep & (is.na(credit_data$date) |
        (credit_data$date >= input$opp_date_range[1] &
         credit_data$date <= input$opp_date_range[2]))
    if (!("all" %in% input$opp_fico) && length(input$opp_fico) > 0)
      keep <- keep & (credit_data$fico_band %in% input$opp_fico)
    if (!("all" %in% input$opp_grade) && length(input$opp_grade) > 0)
      keep <- keep & (credit_data$risk_grade %in% input$opp_grade)
    if (!("all" %in% input$opp_partner) && length(input$opp_partner) > 0)
      keep <- keep & (credit_data$partner_channel %in% input$opp_partner)
    if (!("all" %in% input$opp_repeat) && length(input$opp_repeat) > 0)
      keep <- keep & (credit_data$borrower_repeat_type %in% input$opp_repeat)
    if (!("all" %in% input$opp_mob) && length(input$opp_mob) > 0)
      keep <- keep & (credit_data$mob_bins %in% input$opp_mob)
    keep
  })

  opp_filtered <- reactive({
    df <- credit_data[opp_pop_keep(), , drop = FALSE]
    get_scoped_data(input$opp_data_scope %||% "issued", df = df)
  })

  # Swap-in mode prompts switch to Applications view when on Issued scope
  observeEvent(input$opp_strategy_mode, {
    if (identical(input$opp_strategy_mode, "swapin") &&
        identical(input$opp_data_scope, "issued")) {
      showModal(modalDialog(
        title = "Swap-in needs the Applications view",
        p("Swap-in analyzes applications that were declined. ",
          "The current view only shows issued loans."),
        p("Switch to the Applications view to see the full funnel?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("opp_swap_switch", "Switch to Applications",
                       class = "btn-primary")
        )
      ))
    }
  }, ignoreInit = TRUE)

  observeEvent(input$opp_swap_switch, {
    updateRadioButtons(session, "opp_data_scope", selected = "applications")
    removeModal()
  })

  # CART segmentation
  segments_data <- eventReactive(input$submit_opp, {
    req(input$opp_vars, opp_filtered())
    # Filter selected vars to only those present in the data
    valid_opp_vars <- intersect(input$opp_vars, names(opp_filtered()))
    req(length(valid_opp_vars) > 0)
    mode <- input$opp_strategy_mode %||% "swapout_delinq"

    # ---- Deeper-cuts drill-down ----
    # When the Scanner handed off a segment via session$userData, subset the
    # population to ONLY loans in that segment before running CART. Prefer
    # exact loan_ids (CART leaf membership) over rule text because the
    # displayed rule is a simplified path that can match more loans than the
    # actual leaf. Intersect the carried-segment loan_ids with the page's UI
    # filter mask so filter changes (repeat-type / FICO / grade / partner / MOB)
    # narrow the carried segment too. On arrival all filters default to "all"
    # so the intersection equals the full carried segment and the count still
    # matches Scanner.
    .carried_rule <- deeper_rule_rv()
    .carried_ids  <- deeper_loan_ids_rv()
    opp_pop <- if (!is.null(.carried_ids) && length(.carried_ids) > 0 &&
                   "loan_id" %in% names(credit_data)) {
      pop_keep <- opp_pop_keep()
      credit_data[pop_keep & as.character(credit_data$loan_id) %in% .carried_ids, ,
                  drop = FALSE]
    } else if (!is.null(.carried_rule) && nzchar(.carried_rule)) {
      tryCatch(
        dplyr::filter(opp_filtered(), eval(parse(text = .carried_rule))),
        error = function(e) {
          message("[OppExplorer] carried rule parse failed: ", e$message)
          opp_filtered()
        }
      )
    } else {
      opp_filtered()
    }

    if (mode == "swapout_irr") {
      # --- Swap-Out (IRR) mode: CART regression tree targeting loan-level IRR ---
      withProgress(message = "Building IRR segments...", value = 0, {
        incProgress(0.1, detail = "Loading loan-level IRR...")
        irr_data <- loan_irr_cache()
        shiny::validate(need(!is.null(irr_data) && nrow(irr_data) > 0,
          "Performance tape required for IRR-based segmentation. Please load a tape."))

        incProgress(0.1, detail = "Filtering data...")
        df_base <- opp_pop %>%
          filter(issued_flag == 1)

        # Join loan-level IRR
        df_base$loan_id <- as.character(df_base$loan_id)
        df <- df_base %>%
          inner_join(irr_data, by = "loan_id") %>%
          select(all_of(c(valid_opp_vars, "loan_irr", "performance_delinquency_flag",
                          "interest_rate_apr", "fico_score"))) %>%
          filter(complete.cases(.)) %>%
          filter(is.finite(loan_irr))

        shiny::validate(need(nrow(df) >= 100,
          "Not enough records with IRR data. Widen your filters or check the performance tape."))

        formula <- as.formula(
          paste("loan_irr ~", paste(valid_opp_vars, collapse = "+")))

        incProgress(0.4, detail = "Training CART regression tree...")
        tree <- tryCatch(
          rpart(formula, data = df, method = "anova",
            control = rpart.control(maxdepth = 5, minbucket = 30, cp = 0.001)),
          error = function(e) {
            message("[OppExplorer] CART IRR regression tree: ", e$message)
            NULL
          }
        )
        shiny::validate(need(!is.null(tree), "Could not build IRR regression tree."))

        leaf_ids <- as.integer(
          rownames(tree$frame)[tree$frame$var == "<leaf>"])
        shiny::validate(need(length(leaf_ids) >= 2,
          "Could not find meaningful IRR segments. Try different variables."))

        incProgress(0.2, detail = "Extracting segment rules...")
        paths      <- path.rpart(tree, nodes = leaf_ids, print.it = FALSE)
        conditions <- vapply(paths, function(p) paste(p[-1], collapse = " & "),
                             character(1))
        # Single-leaf trees (common in drill-down on small Scanner segments)
        # produce an empty condition because path.rpart returns only the root
        # label. Fall back to the Scanner's carried rule text -- that's the
        # rule that defines the segment. Outside drill-down use a generic
        # label (non-drill-down single-leaf = truly no split found).
        conditions[!nzchar(conditions)] <-
          if (!is.null(.carried_rule) && nzchar(.carried_rule)) .carried_rule
          else "(no further split)"
        conditions <- vapply(conditions, simplify_cart_condition,
                             character(1), USE.NAMES = TRUE)

        incProgress(0.1, detail = "Computing segment metrics...")
        node_assign <- as.integer(rownames(tree$frame))[tree$where]

        leaf_metrics <- data.frame(node = leaf_ids) %>%
          left_join(
            data.frame(node = node_assign,
                       irr  = df$loan_irr,
                       bad  = as.integer(df$performance_delinquency_flag),
                       apr  = df$interest_rate_apr) %>%
              group_by(node) %>%
              summarise(applications = n(),
                        mean_irr     = mean(irr, na.rm = TRUE),
                        mean_apr     = mean(apr, na.rm = TRUE),
                        bad_rate     = mean(bad, na.rm = TRUE),
                        .groups      = "drop"),
            by = "node"
          )

        overall_irr <- mean(df$loan_irr, na.rm = TRUE)
        overall_bad <- mean(as.integer(df$performance_delinquency_flag), na.rm = TRUE)

        leaf_metrics$condition <- conditions[match(
          as.character(leaf_metrics$node), names(conditions))]

        # Classify by IRR: bottom ~25% = Low IRR, next ~25% = Medium IRR, top ~50% = High IRR
        # Use ceiling for low/medium cutoffs so small leaf counts still produce segments
        n_leaves <- nrow(leaf_metrics)
        ranked   <- rank(leaf_metrics$mean_irr, ties.method = "first")
        low_cut  <- max(1L, ceiling(n_leaves * 0.25))
        med_cut  <- max(low_cut + 1L, ceiling(n_leaves * 0.50))
        leaf_metrics$risk_class <- factor(
          dplyr::case_when(
            ranked <= low_cut ~ "Low IRR",
            ranked <= med_cut ~ "Medium IRR",
            TRUE              ~ "High IRR"
          ),
          levels = c("Low IRR", "Medium IRR", "High IRR")
        )

        # Exclude anchor segments (high-FICO) from IRR recommendations
        if (!is.null(consultant_config$anchor_fico_floor)) {
          leaf_fico <- data.frame(node = node_assign, fico = df$fico_score) %>%
            group_by(node) %>%
            summarise(median_fico = median(fico, na.rm = TRUE), .groups = "drop")
          leaf_metrics <- leaf_metrics %>%
            left_join(leaf_fico, by = "node") %>%
            filter(is.na(median_fico) | median_fico < consultant_config$anchor_fico_floor) %>%
            select(-median_fico)
        }

        tree_vars         <- valid_opp_vars
        leaf_node_ids_vec <- sort(unique(node_assign))

        # Count applications and issued loans per leaf from the FULL (unfiltered)
        # dataset so the segment table matches the KPI boxes downstream.
        incProgress(0.05, detail = "Aligning counts with full dataset...")
        df_full_seg <- opp_filtered()
        full_nodes  <- assign_leaf_nodes(tree, df_full_seg, tree_vars)

        full_dates <- suppressWarnings(
          as.Date(df_full_seg$booking_vintage, format = "%d/%m/%Y"))
        max_date <- max(full_dates, na.rm = TRUE)
        last_month_start <- as.Date(paste0(format(max_date, "%Y-%m"), "-01"))

        counts_per_leaf <- data.frame(
            node   = full_nodes,
            issued = df_full_seg$issued_flag,
            vdate  = full_dates) %>%
          filter(!is.na(node)) %>%
          group_by(node) %>%
          summarise(
            apps_full       = n(),
            issued_full     = sum(issued == 1, na.rm = TRUE),
            apps_last_month = sum(vdate >= last_month_start, na.rm = TRUE),
            .groups = "drop")
        leaf_metrics <- leaf_metrics %>%
          left_join(counts_per_leaf, by = "node") %>%
          mutate(apps_full       = coalesce(apps_full, 0L),
                 issued_full     = coalesce(issued_full, 0L),
                 apps_last_month = coalesce(apps_last_month, 0L))

        incProgress(0.05, detail = "Done.")
        list(
          metrics           = leaf_metrics,
          tree              = tree,
          tree_vars         = tree_vars,
          leaf_node_ids_vec = leaf_node_ids_vec,
          overall_bad       = overall_bad,
          overall_irr       = overall_irr,
          mode              = "swapout_irr"
        )
      })

    } else {
      # --- Swap-Out (Delinquency) mode: existing CART classification tree ---
      withProgress(message = "Building risk segments...", value = 0, {

        incProgress(0.1, detail = "Filtering data...")
        df <- opp_pop %>%
          filter(issued_flag == 1) %>%
          select(all_of(c(valid_opp_vars, "performance_delinquency_flag"))) %>%
          filter(complete.cases(.))

        # In drill-down mode the Scanner segment may be small (<100) but still
        # actionable; CART with surrogates handles 30-99 rows fine. Only bail
        # if we don't even have enough for a single split.
        .min_rows <- if (!is.null(.carried_ids) && length(.carried_ids) > 0) 30 else 100
        shiny::validate(need(nrow(df) >= .min_rows,
          paste0("Scanner segment is too small (", nrow(df), " loans) for further segmentation. ",
                 "Apply a policy action to the whole segment instead.")))

        shiny::validate(need(any(df$performance_delinquency_flag == 1, na.rm = TRUE),
          "No delinquent loans in this population — all loans are performing. Try widening your filters or selecting a different segment."))

        df$performance_delinquency_flag <-
          factor(df$performance_delinquency_flag, levels = c(0, 1))

        formula <- as.formula(
          paste("performance_delinquency_flag ~",
                paste(valid_opp_vars, collapse = "+")))

        incProgress(0.4, detail = "Training CART model...")
        # Match Scanner CART config: information split, scaled minbucket
        # (max(15, min(30, n/20))) so small slices still produce splits.
        mb_val <- max(15L, min(30L, floor(nrow(df) / 20)))
        tree <- tryCatch(
          rpart(formula, data = df, method = "class",
            parms   = list(prior = c(0.3, 0.7), split = "information"),
            control = rpart.control(maxdepth = 5, minbucket = mb_val, cp = 0.001)),
          error = function(e) {
            message("[OppExplorer] CART delinquency classifier: ", e$message)
            NULL
          }
        )

        shiny::validate(need(!is.null(tree), "Could not build decision tree."))

        leaf_ids <- as.integer(
          rownames(tree$frame)[tree$frame$var == "<leaf>"])

        # In drill-down mode a single-leaf tree is still useful -- it means the
        # Scanner segment is homogeneous and should be actioned as a whole. Let
        # the table show that single row instead of validation-blocking.
        .in_drilldown <- !is.null(.carried_ids) && length(.carried_ids) > 0
        shiny::validate(need(length(leaf_ids) >= if (.in_drilldown) 1 else 2,
          "The model could not find meaningful risk segments with these variables. Try adding more variables or widening your filters."))

        incProgress(0.2, detail = "Extracting segment rules...")
        paths      <- path.rpart(tree, nodes = leaf_ids, print.it = FALSE)
        conditions <- vapply(paths, function(p) paste(p[-1], collapse = " & "),
                             character(1))
        # Single-leaf trees (common in drill-down on small Scanner segments)
        # produce an empty condition because path.rpart returns only the root
        # label. Fall back to the Scanner's carried rule text -- that's the
        # rule that defines the segment. Outside drill-down use a generic
        # label (non-drill-down single-leaf = truly no split found).
        conditions[!nzchar(conditions)] <-
          if (!is.null(.carried_rule) && nzchar(.carried_rule)) .carried_rule
          else "(no further split)"
        conditions <- vapply(conditions, simplify_cart_condition,
                             character(1), USE.NAMES = TRUE)

        incProgress(0.2, detail = "Computing segment metrics...")
        node_assign <- as.integer(rownames(tree$frame))[tree$where]

        leaf_metrics <- data.frame(node = leaf_ids) %>%
          left_join(
            data.frame(node = node_assign,
                       bad  = as.integer(as.character(
                         df$performance_delinquency_flag))) %>%
              group_by(node) %>%
              summarise(applications = n(),
                        bads         = sum(bad, na.rm = TRUE),
                        bad_rate     = mean(bad, na.rm = TRUE),
                        .groups      = "drop"),
            by = "node"
          )

        overall_bad <- mean(
          as.integer(as.character(df$performance_delinquency_flag)),
          na.rm = TRUE)

        # Lift denominator: in drill-down mode use the full portfolio (same
        # universe Scanner uses) so sub-leaf risk labels stay consistent with
        # Scanner's framing. Outside drill-down `df` already represents the
        # portfolio so the two reduce to the same number.
        lift_baseline <- overall_bad
        if (!is.null(.carried_ids) && length(.carried_ids) > 0) {
          .prod       <- opp_selected_product()
          .active_pid <- tryCatch(get_active_policy_id(.prod),
                                  error = function(e) NULL)
          .mask <- !is.na(credit_data$issued_flag) &
                   credit_data$issued_flag == 1
          if (!is.null(.prod) && "product_type" %in% names(credit_data))
            .mask <- .mask & credit_data$product_type == .prod
          if (!is.null(.active_pid) && "policy_id" %in% names(credit_data))
            .mask <- .mask & credit_data$policy_id == .active_pid
          .pbad <- mean(
            as.numeric(credit_data$performance_delinquency_flag[.mask] == 1),
            na.rm = TRUE)
          if (is.finite(.pbad) && .pbad > 0) lift_baseline <- .pbad
        }

        leaf_metrics$lift <- round(leaf_metrics$bad_rate / lift_baseline, 2)
        leaf_metrics$condition <- conditions[match(
          as.character(leaf_metrics$node), names(conditions))]
        # Risk class keyed to observed lift magnitude, not rank-within-leaves.
        # Rank-based assignment labeled every top-quartile leaf "High Risk"
        # even at lift 1.1x, making the badge meaningless.
        leaf_metrics$risk_class <- factor(
          dplyr::case_when(
            leaf_metrics$lift >= 1.50 ~ "High Risk",
            leaf_metrics$lift >= 1.15 ~ "Medium Risk",
            TRUE                      ~ "Low Risk"
          ),
          levels = c("Low Risk", "Medium Risk", "High Risk")
        )

        tree_vars         <- valid_opp_vars
        leaf_node_ids_vec <- sort(unique(node_assign))

        # Count applications and issued loans per leaf from the CART population
        # (drill-down-aware) so the segment table matches the KPI boxes
        # downstream and % Portfolio is denominator-consistent with lift.
        df_full_seg <- opp_pop
        full_nodes  <- assign_leaf_nodes(tree, df_full_seg, tree_vars)

        full_dates <- suppressWarnings(
          as.Date(df_full_seg$booking_vintage, format = "%d/%m/%Y"))
        max_date <- max(full_dates, na.rm = TRUE)
        last_month_start <- as.Date(paste0(format(max_date, "%Y-%m"), "-01"))

        counts_per_leaf <- data.frame(
            node   = full_nodes,
            issued = df_full_seg$issued_flag,
            vdate  = full_dates) %>%
          filter(!is.na(node)) %>%
          group_by(node) %>%
          summarise(
            apps_full       = n(),
            issued_full     = sum(issued == 1, na.rm = TRUE),
            apps_last_month = sum(vdate >= last_month_start, na.rm = TRUE),
            .groups = "drop")
        leaf_metrics <- leaf_metrics %>%
          left_join(counts_per_leaf, by = "node") %>%
          mutate(apps_full       = coalesce(apps_full, 0L),
                 issued_full     = coalesce(issued_full, 0L),
                 apps_last_month = coalesce(apps_last_month, 0L))

        incProgress(0.1, detail = "Done.")
        # Diagnostic log for reconciliation debugging -- helps catch cases
        # where leaf-count total diverges from drill-down size.
        .tot_issued_per_leaf <- sum(leaf_metrics$issued_full, na.rm = TRUE)
        .tot_apps_per_leaf   <- sum(leaf_metrics$apps_full,   na.rm = TRUE)
        .n_null_nodes <- sum(is.na(full_nodes))
        message(sprintf(
          "[OppExplorer/segments_data] drilldown=%s carried_ids=%d opp_pop=%d CART_train=%d leaves=%d leaf_issued_sum=%d leaf_apps_sum=%d null_nodes=%d",
          as.character(!is.null(.carried_ids) && length(.carried_ids) > 0),
          length(.carried_ids %||% character(0)),
          nrow(opp_pop),
          nrow(df),
          length(leaf_ids),
          .tot_issued_per_leaf,
          .tot_apps_per_leaf,
          .n_null_nodes))
        list(
          metrics           = leaf_metrics,
          tree              = tree,
          tree_vars         = tree_vars,
          leaf_node_ids_vec = leaf_node_ids_vec,
          overall_bad       = overall_bad,
          lift_baseline     = lift_baseline,
          drilldown         = !is.null(.carried_ids) && length(.carried_ids) > 0,
          drilldown_n       = nrow(opp_pop),
          mode              = "swapout_delinq"
        )
      })
    }
  })

  output$segment_opp <- renderDataTable({
    seg <- segments_data()
    mode <- seg$mode %||% "swapout_delinq"
    total_issued <- sum(seg$metrics$issued_full, na.rm = TRUE)

    if (mode == "swapout_irr") {
      # IRR mode: show only segments with IRR worse than portfolio average
      df <- seg$metrics %>%
        filter(mean_irr < seg$overall_irr) %>%
        arrange(mean_irr) %>%
        transmute(
          `IRR Class`         = as.character(risk_class),
          `#Issued`           = issued_full,
          `% Portfolio`       = round(issued_full / max(total_issued, 1) * 100, 1),
          `IRR`               = round(mean_irr, 4),
          `Avg APR`           = round(mean_apr, 4),
          `2+ Rate`          = round(bad_rate, 4),
          `#Apps`             = apps_full,
          `#Apps Last Month`  = apps_last_month,
          Rule                = vapply(condition, format_rule_display, character(1),
                                       USE.NAMES = FALSE)
        )
      # Footer TOTAL row -- reconciles the #Issued column to the full
      # population so the CRO doesn't have to mentally sum the rows.
      all_rows <- seg$metrics
      tot_issued <- sum(all_rows$issued_full,     na.rm = TRUE)
      tot_apps   <- sum(all_rows$apps_full,       na.rm = TRUE)
      tot_lm     <- sum(all_rows$apps_last_month, na.rm = TRUE)
      tot_bad    <- if (tot_issued > 0) {
        sum(all_rows$issued_full * all_rows$bad_rate, na.rm = TRUE) / tot_issued
      } else NA_real_
      tot_irr    <- if (tot_issued > 0 &&
                        all(c("mean_irr","issued_full") %in% names(all_rows))) {
        sum(all_rows$issued_full * all_rows$mean_irr, na.rm = TRUE) / tot_issued
      } else NA_real_
      tot_apr    <- if (tot_issued > 0 &&
                        all(c("mean_apr","issued_full") %in% names(all_rows))) {
        sum(all_rows$issued_full * all_rows$mean_apr, na.rm = TRUE) / tot_issued
      } else NA_real_
      df <- dplyr::bind_rows(df, data.frame(
        check.names = FALSE,
        `IRR Class`         = "TOTAL",
        `#Issued`           = as.integer(tot_issued),
        `% Portfolio`       = round(tot_issued / max(total_issued, 1) * 100, 1),
        `IRR`               = round(tot_irr, 4),
        `Avg APR`           = round(tot_apr, 4),
        `2+ Rate`          = round(tot_bad, 4),
        `#Apps`             = as.integer(tot_apps),
        `#Apps Last Month`  = as.integer(tot_lm),
        Rule                = "(all sub-segments combined)",
        stringsAsFactors    = FALSE
      ))
      dt <- datatable(df, rownames = FALSE, selection = "single",
        options = list(
          pageLength = 10, dom = "tp", scrollX = TRUE,
          initComplete = JS(
            "function(s,j){",
            "$(this.api().table().header()).css(",
            "  {'background-color':'#fb6e36','color':'white'});",
            "}"
          ),
          rowCallback = JS(
            "function(row, data) {",
            "  if (data[0] === 'TOTAL') {",
            "    $(row).css({'font-weight':'700','background-color':'#e5e7eb','border-top':'2px solid #374151'});",
            "  }",
            "}"
          )
        )
      ) %>%
        formatCurrency(c("#Apps", "#Apps Last Month", "#Issued"),
                       currency = "", digits = 0) %>%
        formatPercentage(c("IRR", "Avg APR", "2+ Rate"), digits = 1) %>%
        formatRound("% Portfolio", digits = 1)
    } else {
      # Delinquency mode. Filter to actionable segments only (Low Risk +
      # lift<1 excluded). In drill-down mode lift is computed against the
      # portfolio baseline (not the segment baseline), so sub-leaves that
      # are still risky vs the portfolio stay labeled High/Medium Risk and
      # remain visible here -- matching Scanner's framing.
      visible_rows <- seg$metrics %>%
        filter(risk_class != "Low Risk", lift >= 1)
      hidden_rows <- seg$metrics %>%
        filter(!(risk_class != "Low Risk" & lift >= 1))
      df <- visible_rows %>%
        transmute(
          `Risk Class`        = as.character(risk_class),
          `#Issued`           = issued_full,
          `% Portfolio`       = round(issued_full / max(total_issued, 1) * 100, 1),
          `2+ Rate`          = round(bad_rate, 4),
          `Lift (Observed)`   = lift,
          `#Apps`             = apps_full,
          `#Apps Last Month`  = apps_last_month,
          Rule                = vapply(condition, format_rule_display, character(1),
                                       USE.NAMES = FALSE)
        )
      # Drill-down reconciliation: append a remainder row summarising the
      # sub-leaves that were filtered out so the total #Issued ties to the
      # Scanner segment size (CRO demo requirement). Non-drill-down mode
      # keeps the focused view with no remainder.
      if (isTRUE(seg$drilldown) && nrow(hidden_rows) > 0) {
        rem_issued <- sum(hidden_rows$issued_full, na.rm = TRUE)
        rem_apps   <- sum(hidden_rows$apps_full,   na.rm = TRUE)
        rem_lm     <- sum(hidden_rows$apps_last_month, na.rm = TRUE)
        rem_bad    <- if (rem_issued > 0) {
          sum(hidden_rows$issued_full * hidden_rows$bad_rate, na.rm = TRUE) /
            rem_issued
        } else NA_real_
        rem_lift   <- if (!is.null(seg$lift_baseline) && seg$lift_baseline > 0 &&
                          !is.na(rem_bad)) {
          round(rem_bad / seg$lift_baseline, 2)
        } else NA_real_
        df <- dplyr::bind_rows(df, data.frame(
          check.names = FALSE,
          `Risk Class`        = sprintf("Remainder (%d sub-segments)", nrow(hidden_rows)),
          `#Issued`           = as.integer(rem_issued),
          `% Portfolio`       = round(rem_issued / max(total_issued, 1) * 100, 1),
          `2+ Rate`          = round(rem_bad, 4),
          `Lift (Observed)`   = rem_lift,
          `#Apps`             = as.integer(rem_apps),
          `#Apps Last Month`  = as.integer(rem_lm),
          Rule                = "(below action threshold)",
          stringsAsFactors    = FALSE
        ))
      }
      # Final TOTAL row -- gives the CRO an at-a-glance reconciliation to
      # the Scanner segment (drill-down) or full portfolio (normal). Weighted
      # 2+ rate uses issued_full-weighted bad_rate across ALL leaves; total
      # lift uses the same denominator as the per-row lift (portfolio in
      # drill-down, segment mean otherwise).
      all_rows <- seg$metrics
      tot_issued <- sum(all_rows$issued_full,     na.rm = TRUE)
      tot_apps   <- sum(all_rows$apps_full,       na.rm = TRUE)
      tot_lm     <- sum(all_rows$apps_last_month, na.rm = TRUE)
      tot_bad    <- if (tot_issued > 0) {
        sum(all_rows$issued_full * all_rows$bad_rate, na.rm = TRUE) / tot_issued
      } else NA_real_
      tot_lift   <- if (!is.null(seg$lift_baseline) && seg$lift_baseline > 0 &&
                        !is.na(tot_bad)) {
        round(tot_bad / seg$lift_baseline, 2)
      } else NA_real_
      df <- dplyr::bind_rows(df, data.frame(
        check.names = FALSE,
        `Risk Class`        = "TOTAL",
        `#Issued`           = as.integer(tot_issued),
        `% Portfolio`       = round(tot_issued / max(total_issued, 1) * 100, 1),
        `2+ Rate`          = round(tot_bad, 4),
        `Lift (Observed)`   = tot_lift,
        `#Apps`             = as.integer(tot_apps),
        `#Apps Last Month`  = as.integer(tot_lm),
        Rule                = "(all sub-segments combined)",
        stringsAsFactors    = FALSE
      ))
      dt <- datatable(df, rownames = FALSE, selection = "single",
        options = list(
          pageLength = 10, dom = "tp", scrollX = TRUE,
          initComplete = JS(
            "function(s,j){",
            "$(this.api().table().header()).css(",
            "  {'background-color':'#fb6e36','color':'white'});",
            "}"
          ),
          # Bold + highlight the TOTAL row so it reads as a footer, and
          # subtly dim the Remainder row so it feels secondary to the
          # action-eligible segments.
          rowCallback = JS(
            "function(row, data) {",
            "  if (data[0] === 'TOTAL') {",
            "    $(row).css({'font-weight':'700','background-color':'#e5e7eb','border-top':'2px solid #374151'});",
            "  } else if (String(data[0]).indexOf('Remainder') === 0) {",
            "    $(row).css({'color':'#6b7280','background-color':'#f9fafb','font-style':'italic'});",
            "  }",
            "}"
          )
        )
      ) %>%
        formatCurrency(c("#Apps", "#Apps Last Month", "#Issued"),
                       currency = "", digits = 0) %>%
        formatRound(c("Lift (Observed)", "% Portfolio"), digits = 2) %>%
        formatPercentage("2+ Rate", digits = 1)
    }
    dt
  })

  # ---- Overall portfolio stats bar (item 1) ----
  output$opp_overall_stats <- renderUI({
    seg <- segments_data()
    mode <- seg$mode %||% "swapout_delinq"
    total_issued <- sum(seg$metrics$issued_full, na.rm = TRUE)
    total_apps   <- sum(seg$metrics$apps_full, na.rm = TRUE)
    overall_bad  <- seg$overall_bad

    stats <- list(
      tags$div(style = "flex:1; min-width:0;",
        kpi_box(format(total_apps, big.mark = ","), "Total Applications")),
      tags$div(style = "flex:1; min-width:0;",
        kpi_box(format(total_issued, big.mark = ","), "Issued Loans")),
      tags$div(style = "flex:1; min-width:0;",
        kpi_box(paste0(round(overall_bad * 100, 1), "%"), "Overall 2+ Rate"))
    )
    if (mode == "swapout_irr" && !is.null(seg$overall_irr)) {
      stats <- c(stats, list(
        tags$div(style = "flex:1; min-width:0;",
          kpi_box(paste0(round(seg$overall_irr * 100, 1), "%"), "Overall IRR"))
      ))
    }
    tags$div(style = "display:flex; gap:10px; margin-bottom:10px;", stats)
  })

  # ---- Drill-down subtitle (shown only when Deeper cuts active) ----
  # Gives the CRO an at-a-glance reconciliation: "Drill-down on Scanner
  # segment of N loans. Lift normalised to portfolio baseline (X%), so
  # sub-leaf risk labels remain consistent with Scanner's framing."
  output$opp_drilldown_subtitle <- renderUI({
    seg <- segments_data()
    if (!isTRUE(seg$drilldown)) return(NULL)
    n <- seg$drilldown_n %||% 0L
    base_pct <- if (is.null(seg$lift_baseline)) NA_real_
                else round(seg$lift_baseline * 100, 1)
    base_txt <- if (is.na(base_pct)) "portfolio" else paste0(base_pct, "% portfolio baseline")
    tags$div(
      style = paste0(
        "background:#fff7ed; border-left:3px solid #fb6e36; padding:8px 12px;",
        " border-radius:4px; margin:6px 0 10px; font-size:12px; color:#6b7280;"),
      tags$b(style = "color:#b45309;",
        paste0("Drill-down: Scanner segment of ",
               format(n, big.mark = ","), " loans.")),
      tags$span(" Lift normalised to ", base_txt,
        " so risk labels match the Scanner card.")
    )
  })

  # ---- Lift explanation footnote (item 2) ----
  output$opp_lift_footnote <- renderUI({
    seg <- segments_data()
    mode <- seg$mode %||% "swapout_delinq"
    if (mode == "swapout_irr") {
      tags$div(style = "font-size:11px; color:#9ca3af; padding:6px 4px;",
        tags$em("Showing segments with IRR below portfolio average. Select a row to apply a policy action."))
    } else {
      tags$div(style = "font-size:11px; color:#9ca3af; padding:6px 4px;",
        tags$b("Lift"), " = Segment Bad Rate \u00F7 Overall Bad Rate. ",
        "A Lift of 2.0 means the segment's delinquency is 2\u00D7 the portfolio average. ",
        "Only segments with Lift \u2265 1 (worse than average) are shown. ",
        tags$em("Select a row to apply a policy action."))
    }
  })

  # Show loading overlay when Run Segmentation is clicked. The overlay hides
  # on success via the segments_data() observer below. If segments_data()
  # throws a validate error (e.g. drill-down segment too small), that
  # observer never fires -- the safety delay guarantees the overlay comes
  # down regardless so the user sees the validation message, not a stuck
  # spinner.
  observeEvent(input$submit_opp, {
    shinyjs::show("opp_action_overlay")
    shinyjs::delay(15000, shinyjs::hide("opp_action_overlay"))
  })

  observeEvent(segments_data(), {
    shinyjs::hide("opp_action_overlay")
    showElement("opp_action_row")
  })

  # Reactive to track which action button was clicked
  opp_action_choice <- reactiveVal("Decline")

  # Decline button: set action, hide pricing opts, trigger shared logic
  observeEvent(input$opp_decline_btn, {
    sel <- input$segment_opp_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      showModal(modalDialog(
        title = "No Segment Selected",
        tags$p(style = "font-size:14px;",
          "Please select a segment from the table above before taking an action."),
        footer = modalButton("OK"),
        easyClose = TRUE
      ))
      return()
    }
    opp_action_choice("Decline")
    shinyjs::hide("opp_pricing_opts")
    shinyjs::show("opp_action_overlay")
    opp_apply_trigger(opp_apply_trigger() + 1L)
  })

  # Reprice button: show pricing opts (does NOT trigger simulation)
  observeEvent(input$opp_reprice_btn, {
    sel <- input$segment_opp_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      showModal(modalDialog(
        title = "No Segment Selected",
        tags$p(style = "font-size:14px;",
          "Please select a segment from the table above before taking an action."),
        footer = modalButton("OK"),
        easyClose = TRUE
      ))
      return()
    }
    opp_action_choice("Pricing")
    shinyjs::show("opp_pricing_opts")
  })

  # Apply Reprice button: trigger shared simulation logic
  observeEvent(input$opp_apply_reprice_btn, {
    shinyjs::show("opp_action_overlay")
    opp_apply_trigger(opp_apply_trigger() + 1L)
  })

  opp_apply_trigger <- reactiveVal(0L)

  observeEvent(opp_apply_trigger(), ignoreInit = TRUE, {
    # Guard: must have a segment selected
    sel <- input$segment_opp_rows_selected
    if (is.null(sel) || length(sel) == 0) {
      showNotification("Please select a segment from the table first.",
                       type = "warning", duration = 4)
      return()
    }

    seg  <- segments_data()
    mode <- seg$mode %||% "swapout_delinq"

    # Identify selected node (table shows different subsets by mode). Must
    # mirror the filter applied in output$segment_opp so row index → node.
    if (mode == "swapout_irr") {
      visible_metrics <- seg$metrics %>%
        filter(mean_irr < seg$overall_irr) %>%
        arrange(mean_irr)
    } else {
      visible_metrics <- seg$metrics %>% filter(risk_class != "Low Risk", lift >= 1)
    }
    selected_node <- visible_metrics$node[sel]

    # ---- Build engine baseline (full L10-L90 cascade) ----
    pop_keep     <- opp_pop_keep()
    cd_filtered  <- credit_data[pop_keep, , drop = FALSE]
    pid_filtered <- policy_input_data[pop_keep, , drop = FALSE]
    default_params <- opp_build_baseline_params()
    baseline_df <- apply_policy_with_params(pid_filtered, default_params)

    # Carry historical/performance columns from credit_data into baseline
    carry_cols <- c("issued_flag", "issued_amount", "performance_delinquency_flag",
                    "booking_vintage", "fico_band", "borrower_repeat_type",
                    "mob_bins", "date", "months_on_books", "product_type")
    for (col in carry_cols) {
      if (col %in% names(cd_filtered) && !(col %in% names(baseline_df)))
        baseline_df[[col]] <- cd_filtered[[col]]
    }
    # Override issued_flag and set interest_rate_apr from engine
    baseline_df$issued_flag <- cd_filtered$issued_flag
    baseline_df$interest_rate_apr <- ifelse(
      baseline_df$offered_decision_flag == "APPR",
      baseline_df$offered_final_apr,
      cd_filtered$interest_rate_apr)

    df_full <- baseline_df

    # Assign CART nodes on cd_filtered (has all CART vars like fico_band, mob_bins)
    tree_vars  <- seg$tree_vars
    pred_nodes <- assign_leaf_nodes(seg$tree, cd_filtered, tree_vars)
    # Include ALL loans in the CART leaf (not just issued) so funnel
    # metrics (approved -> issued conversion) are well-defined.
    in_segment <- !is.na(pred_nodes) & pred_nodes == selected_node

    # Drill-down guard: when a Scanner segment is carried over, the CART tree
    # was trained on only that segment's loan_ids -- but assign_leaf_nodes
    # runs across the full filtered book, so a single-leaf (or coarse-split)
    # tree would assign ALL 5,880 loans to the same leaf. Intersect with the
    # carried loan_ids to keep the action scoped to the drill-down segment.
    .carried_ids <- deeper_loan_ids_rv()
    if (!is.null(.carried_ids) && length(.carried_ids) > 0 &&
        "loan_id" %in% names(cd_filtered)) {
      in_segment <- in_segment &
                    as.character(cd_filtered$loan_id) %in% .carried_ids
    }

    # For pricing actions, only apply to issued loans in the segment.
    # For decline actions, apply to ALL approved loans in the segment
    # (including approved-but-not-issued), since the policy decision
    # is to reject these applicants entirely.
    in_segment_issued <- in_segment & (df_full$issued_flag == 1)

    # Build proposed dataset
    df_proposed <- df_full
    action      <- opp_action_choice()

    if (action == "Decline") {
      # Decline ALL approved OR historically-issued loans in the segment
      # so the count matches #Issued in the segment table (which uses
      # credit_data$issued_flag, the historical truth from the CSV).
      to_decline <- in_segment & (
        (!is.na(df_full$offered_decision_flag) &
         df_full$offered_decision_flag == "APPR") |
        (df_full$issued_flag == 1)
      )
      df_proposed$offered_decision_flag[to_decline] <- "DECL"
      df_proposed$issued_flag[to_decline] <- 0L
    } else {
      bps <- if (input$opp_pricing_dec == "Custom")
               suppressWarnings(as.numeric(input$opp_pricing_custom))
             else
               suppressWarnings(as.numeric(input$opp_pricing_dec))
      if (!is.na(bps) && bps > 0) {
        # Raise the policy-derived APR (adjusted_apr_pre_term_cap) for all
        # approved loans in the segment. This is the L30/L40 output that
        # feeds into final_apr via L50's 72-month cap. Do NOT bump
        # interest_rate_apr (original CSV column) — it has a different base.
        to_price <- in_segment &
          !is.na(df_full$offered_decision_flag) &
          df_full$offered_decision_flag == "APPR"
        df_proposed$adjusted_apr_pre_term_cap[to_price] <-
          df_proposed$adjusted_apr_pre_term_cap[to_price] + bps / 10000

        # Replay downstream policy layers (50, 80, 90) with engine params
        df_proposed <- replay_layers_50_80_90(df_proposed,
          fico_min                       = default_params$fico_min,
          fico_max                       = default_params$fico_max,
          thin_file_mo                   = default_params$thin_file_months,
          delinq_thresh                  = default_params$delinquency_max,
          dti_thresh                     = default_params$dti_cutoff,
          n_hard_inq_6m_max              = default_params$n_hard_inq_6m_max %||% 99L,
          thin_file_reentry_delinq_gate  = default_params$thin_file_reentry_delinq_gate %||% 99L,
          thin_file_reentry_min          = default_params$thin_file_reentry_min,
          thin_file_reentry_max          = default_params$thin_file_reentry_max,
          revol_utl_max                  = default_params$revol_utl_max %||% 1.0)

        # Sync interest_rate_apr from offered_final_apr so KPIs, sample
        # preview, and IRR charts read the repriced APR (not stale CSV value)
        df_proposed$interest_rate_apr <- ifelse(
          df_proposed$offered_decision_flag == "APPR",
          df_proposed$offered_final_apr,
          df_proposed$interest_rate_apr)

        # Sync issued_flag: newly declined loans can no longer be issued
        newly_declined <- df_proposed$offered_decision_flag != "APPR" &
                          df_full$offered_decision_flag == "APPR"
        df_proposed$issued_flag[newly_declined] <- 0L

        # Price elasticity: higher rate -> fewer take-ups (user-configurable)
        # primary_var lets the waterfall's Layer 1 match this segment against
        # any V4 direct test/control evidence on the same attribute.
        seg_grades <- df_full$risk_grade[in_segment & df_full$issued_flag == 1]
        elast_pct  <- get_segment_assumption(seg_grades, "elasticity",
                                             primary_var = seg$primary_var) / 100
        drop_rate     <- min((bps / 100) * elast_pct, 0.50)
        in_seg_issued <- in_segment_issued & df_full$issued_flag == 1
        n_seg_issued  <- sum(in_seg_issued, na.rm = TRUE)
        n_drop        <- round(n_seg_issued * drop_rate)

        if (n_drop > 0 && n_seg_issued > 0) {
          seg_issued_idx <- which(in_seg_issued)
          fico_order <- order(df_full$fico_score[seg_issued_idx],
                              decreasing = TRUE)
          drop_idx   <- seg_issued_idx[fico_order[seq_len(n_drop)]]
          df_proposed$issued_flag[drop_idx] <- 0L
        }

        # Adverse selection: pricing increase raises bad rate in segment
        adv_pct <- get_segment_assumption(seg_grades, "adverse_selection") / 100
        adverse_flip_ids <- character(0)
        if (adv_pct > 0 && bps > 0) {
          adverse_adj <- (bps / 100) * adv_pct
          still_issued <- in_segment & df_proposed$issued_flag == 1
          good_in_seg  <- which(still_issued &
                                  df_proposed$performance_delinquency_flag == 0)
          n_flip <- round(length(good_in_seg) * adverse_adj)
          if (n_flip > 0 && length(good_in_seg) > 0) {
            set.seed(123)
            fico_ord <- order(df_proposed$fico_score[good_in_seg])
            flip_idx <- good_in_seg[fico_ord[seq_len(min(n_flip,
                                                          length(good_in_seg)))]]
            df_proposed$performance_delinquency_flag[flip_idx] <- 1L
            adverse_flip_ids <- as.character(df_proposed$loan_id[flip_idx])
          }
        }
      }
    }

    # APR KPIs — scoped to full segment (both throttled and unthrottled loans)
    bl_seg_apr <- mean(
      df_full$interest_rate_apr[in_segment & df_full$issued_flag == 1],
      na.rm = TRUE)
    pr_seg_apr <- mean(
      df_proposed$interest_rate_apr[in_segment & df_proposed$issued_flag == 1],
      na.rm = TRUE)

    # Funnel metrics (portfolio-level)
    bl <- compute_funnel(df_full)
    pr <- compute_funnel(df_proposed)

    seg_size      <- sum(in_segment, na.rm = TRUE)
    seg_appr      <- sum(
      in_segment & (
        (!is.na(df_full$offered_decision_flag) &
         df_full$offered_decision_flag == "APPR") |
        (df_full$issued_flag == 1)),
      na.rm = TRUE)
    bl_seg_issued <- sum(in_segment & df_full$issued_flag     == 1, na.rm = TRUE)
    pr_seg_issued <- sum(in_segment & df_proposed$issued_flag == 1, na.rm = TRUE)

    # Store everything in reactiveValues
    opp_rv$df_full       <- df_full
    opp_rv$df_proposed   <- df_proposed
    opp_rv$in_segment    <- in_segment
    opp_rv$action        <- action
    opp_rv$bl            <- bl
    opp_rv$pr            <- pr
    opp_rv$seg_size      <- seg_size
    opp_rv$seg_appr      <- seg_appr
    opp_rv$bl_seg_issued <- bl_seg_issued
    opp_rv$pr_seg_issued <- pr_seg_issued
    opp_rv$bl_seg_apr    <- bl_seg_apr
    opp_rv$pr_seg_apr    <- pr_seg_apr
    opp_rv$seg_metrics   <- seg$metrics
    opp_rv$overall_irr   <- seg$overall_irr
    opp_rv$segment_condition <- visible_metrics$condition[sel]
    opp_rv$strategy_mode     <- mode
    opp_rv$saved_policy_id   <- NULL   # reset on new action
    # Save CART tree and node for accurate replay in policy closure. When the
    # action was triggered from a drill-down (Scanner "Deeper cuts"), also
    # stash the carried loan_ids so the saved policy scopes the segment to
    # exactly those loans when replayed -- otherwise a coarse/single-leaf
    # tree would match the whole book.
    opp_rv$saved_tree      <- seg$tree
    opp_rv$saved_tree_vars <- seg$tree_vars
    opp_rv$saved_node      <- selected_node
    opp_rv$saved_loan_ids  <- if (isTRUE(seg$drilldown)) .carried_ids else NULL

    # Store bps for XIRR cashflow adjustment
    if (action == "Pricing" && exists("bps", inherits = FALSE) && !is.na(bps)) {
      opp_rv$bps <- bps
    } else {
      opp_rv$bps <- 0
    }

    # Store adversely-selected loan IDs for vintage curve adjustment
    opp_rv$adverse_flip_ids <- if (exists("adverse_flip_ids", inherits = FALSE))
      adverse_flip_ids else character(0)

    # Populate vintage cohort choices for analysis filter
    vintages <- sort(unique(na.omit(format(df_full$date, "%Y-%m"))))
    updatePickerInput(session, "opp_perf_vintage",
      choices  = c("All" = "all", setNames(vintages, vintages)),
      selected = c("all", vintages))

    # ---- Build sample preview data ----
    sample_preview <- NULL
    sample_full <- NULL
    sample_all  <- NULL
    n_affected <- 0L

    if (action == "Decline") {
      # Scope to historically-issued loans whose issued_flag was zeroed out
      # by the decline action. This matches #Issued in the segment table
      # (which uses credit_data$issued_flag, the CSV ground truth).
      changed <- which(
        in_segment &
        df_full$issued_flag == 1 &
        df_proposed$issued_flag == 0
      )
      if (length(changed) > 0) {
        sample_df <- data.frame(
          loan_id           = df_full$loan_id[changed],
          fico_score        = df_full$fico_score[changed],
          risk_grade        = df_full$risk_grade[changed],
          original_decision = df_full$offered_decision_flag[changed],
          new_decision      = "DECL",
          original_issued   = df_full$issued_amount[changed],
          new_issued        = 0,
          stringsAsFactors  = FALSE
        )
        sample_all  <- sample_df
        set.seed(42)
        sample_full <- head(sample_df[sample(nrow(sample_df)), ], 50)
        sample_preview <- head(sample_full, 5)
        n_affected <- length(changed)
      }
    } else {
      # Pricing action
      changed <- which(
        in_segment_issued &
        !is.na(df_full$interest_rate_apr) &
        !is.na(df_proposed$interest_rate_apr) &
        abs(df_proposed$interest_rate_apr - df_full$interest_rate_apr) > 1e-6
      )
      if (length(changed) > 0) {
        sample_df <- data.frame(
          loan_id      = df_full$loan_id[changed],
          fico_score   = df_full$fico_score[changed],
          risk_grade   = df_full$risk_grade[changed],
          original_apr = round(df_full$interest_rate_apr[changed] * 100, 2),
          new_apr      = round(df_proposed$interest_rate_apr[changed] * 100, 2),
          apr_change_bps = round(
            (df_proposed$interest_rate_apr[changed] - df_full$interest_rate_apr[changed]) * 10000),
          stringsAsFactors = FALSE
        )
        sample_all  <- sample_df
        set.seed(42)
        sample_full <- head(sample_df[sample(nrow(sample_df)), ], 50)
        sample_preview <- head(sample_full, 5)
        n_affected <- length(changed)
      }
    }

    # ---- Store in dual simulation slot ----
    slot <- input$opp_sim_slot %||% "A"
    sim_data <- list(
      df_full = df_full, df_proposed = df_proposed,
      in_segment = in_segment, action = action, bps = opp_rv$bps,
      bl = bl, pr = pr,
      bl_seg_apr = bl_seg_apr, pr_seg_apr = pr_seg_apr,
      seg_size = seg_size, seg_appr = seg_appr,
      bl_seg_issued = bl_seg_issued, pr_seg_issued = pr_seg_issued,
      label = scenario_slot_label(slot),
      # Replay metadata for Save to Repository
      segment_condition = opp_rv$segment_condition,
      saved_tree        = opp_rv$saved_tree,
      saved_tree_vars   = opp_rv$saved_tree_vars,
      saved_node        = opp_rv$saved_node,
      saved_loan_ids    = opp_rv$saved_loan_ids,
      elast             = get_segment_assumption(
        df_full$risk_grade[in_segment & df_full$issued_flag == 1], "elasticity",
        primary_var = seg$primary_var),
      adverse_flip_ids  = opp_rv$adverse_flip_ids,
      sample_preview    = sample_preview,
      sample_full       = sample_full,
      sample_all        = sample_all,
      n_affected        = n_affected,
      filters           = list(
        fico    = input$opp_fico,
        partner = input$opp_partner,
        repeat_ = input$opp_repeat,
        mob     = input$opp_mob
      )
    )
    opp_rv[[paste0("sim_", slot)]] <- sim_data
    opp_rv$sample_slot <- slot

    if (!is.null(sample_preview) || !is.null(opp_rv$sim_A$sample_preview) || !is.null(opp_rv$sim_B$sample_preview)) {
      showElement("opp_sample_preview_panel")
    } else {
      hideElement("opp_sample_preview_panel")
    }

    showElement("opp_perf_show")
    # NOTE: overlay hide + scroll moved to opp_funnel_table renderUI
    # so the overlay persists until funnel metrics are actually rendered.

    # ── Post-action recap modal (matches Scanner's Analysis Engine card) ──
    opp_show_analysis_modal(sim_data)
  })

  # ── Analysis Engine recap (shared visual language with Scanner) ──
  opp_show_analysis_modal <- function(sim) {
    if (is.null(sim)) return(invisible())
    action <- sim$action %||% "Pricing"
    bps    <- sim$bps %||% 0
    is_pricing <- action == "Pricing" && bps != 0

    fmt_pp <- function(x) sprintf("%+.2fpp", x * 100)
    bl_bad <- sim$bl$bad_rate %||% NA_real_
    pr_bad <- sim$pr$bad_rate %||% NA_real_

    # Portfolio IRR: reuse the Explorer's adjusted XIRR compute on the
    # sim's baseline and proposed frames. Tape is session-cached.
    tape <- tryCatch(perf_tape(), error = function(e) NULL)
    bl_irr <- NA_real_; pr_irr <- NA_real_
    if (!is.null(tape)) {
      bl_irr <- tryCatch(
        compute_adjusted_xirr_v2(sim$df_full, tape, sim$df_full),
        error = function(e) NA_real_)
      pr_irr <- tryCatch(
        compute_adjusted_xirr_v2(sim$df_proposed, tape, sim$df_full),
        error = function(e) NA_real_)
    }

    action_label <- if (action == "Decline") "Decline segment"
                    else sprintf("Reprice %+d bps", as.integer(bps))
    bad_rate_str <- if (is.finite(bl_bad) && is.finite(pr_bad))
      paste0(scales::percent(bl_bad, 0.1), " \u2192 ", scales::percent(pr_bad, 0.1),
             " (", fmt_pp(pr_bad - bl_bad), ")") else "n/a"
    irr_str <- if (is.finite(bl_irr) && is.finite(pr_irr))
      paste0(scales::percent(bl_irr, 0.1), " \u2192 ", scales::percent(pr_irr, 0.1),
             " (", fmt_pp(pr_irr - bl_irr), ")") else "n/a"

    summary <- list(
      list(label = "Action",             value = action_label),
      list(label = "Portfolio Bad Rate", value = bad_rate_str,
           muted = bad_rate_str == "n/a"),
      list(label = "Portfolio IRR",      value = irr_str,
           muted = irr_str == "n/a")
    )

    step_pricing_status <- if (is_pricing) "done" else "skipped"
    steps <- list(
      list(label  = "Segment action applied",
           result = action_label,
           status = "done"),
      list(label  = "Price-elasticity model applied",
           result = if (is_pricing) "CALIBRATED" else "NOT APPLICABLE",
           status = step_pricing_status),
      list(label  = "Adverse-selection model applied",
           result = if (is_pricing) "CALIBRATED" else "NOT APPLICABLE",
           status = step_pricing_status),
      list(label  = "Portfolio metrics recomputed",
           result = irr_str,
           status = "done")
    )

    showModal(modalDialog(
      title = tagList(icon("chart-line"),
                      " Action Analysis \u2014 How the numbers were computed"),
      size = "l", easyClose = TRUE,
      analysis_engine_static(steps, summary = summary),
      footer = modalButton("Close")
    ))
  }

  # ---- Clear individual simulation slots ----
  opp_hide_if_both_cleared <- function() {
    both_empty <- is.null(opp_rv$sim_A) && is.null(opp_rv$sim_B)
    if (both_empty) {
      hideElement("opp_perf_show")
      hideElement("opp_sample_preview_panel")
    } else {
      # Still have one slot — only hide sample if neither has preview
      if (is.null(opp_rv$sim_A$sample_preview) && is.null(opp_rv$sim_B$sample_preview))
        hideElement("opp_sample_preview_panel")
    }
  }
  observeEvent(input$opp_clear_A, {
    opp_rv$sim_A <- NULL
    opp_hide_if_both_cleared()
    showNotification("Primary action cleared.", type = "message", duration = 2)
  })
  observeEvent(input$opp_clear_B, {
    opp_rv$sim_B <- NULL
    opp_hide_if_both_cleared()
    showNotification("Alternative action cleared.", type = "message", duration = 2)
  })

  sample_slot_data <- function(slot) {
    opp_rv[[paste0("sim_", slot)]]
  }

  scenario_slot_label <- function(slot) {
    if (slot == "A") "Primary" else "Alternative"
  }

  scenario_action_label <- function(sim) {
    if (is.null(sim)) return(NULL)
    if (identical(sim$action, "Pricing")) {
      paste0("Price", if (!is.null(sim$bps) && sim$bps > 0) paste0(" +", sim$bps, " bps") else "")
    } else {
      "Decline"
    }
  }

  scenario_segment_label <- function(sim) {
    if (is.null(sim)) return(NULL)
    cond <- sim$segment_condition %||% ""
    if (!nzchar(cond)) return("Segment not available")
    paste(strwrap(cond, width = 70), collapse = " ")
  }

  build_scenario_summary_ui <- function() {
    rows <- list()
    for (slot in c("A", "B")) {
      sim <- sample_slot_data(slot)
      if (is.null(sim)) next
      rows[[length(rows) + 1]] <- div(
        style = "display:flex; gap:16px; align-items:flex-start; padding:10px 14px; border-top:1px solid #e5e7eb;",
        tags$div(style = "min-width:88px; font-weight:700; color:#0f172a;",
          scenario_slot_label(slot)),
        tags$div(style = "min-width:120px; color:#334155;",
          scenario_action_label(sim)),
        tags$div(style = "min-width:110px; color:#334155;",
          paste0(format(sim$n_affected %||% 0, big.mark = ","), " affected")),
        tags$div(style = "flex:1; color:#64748b; font-size:12px;",
          scenario_segment_label(sim))
      )
    }
    if (length(rows) == 0) return(NULL)
    div(class = "card",
      h4("Scenario Summary", style = "font-weight:bold; margin-bottom:8px;"),
      div(style = "background:#f8fafc; border-radius:8px; overflow:hidden;", rows)
    )
  }

  output$opp_scenario_summary <- renderUI({
    build_scenario_summary_ui()
  })

  # ── Root Cause Analysis panel (UC1) ──
  output$opp_root_cause <- renderUI({
    req(opp_rv$df_full, opp_rv$in_segment, opp_rv$seg_metrics)

    in_segment <- opp_rv$in_segment
    if (sum(in_segment, na.rm = TRUE) < 10) return(NULL)

    # Build segment info from opp_rv state
    seg_condition <- opp_rv$segment_condition %||% "(no rule)"
    df_full <- opp_rv$df_full
    seg_issued <- df_full[in_segment & df_full$issued_flag == 1 &
                          !is.na(df_full$performance_delinquency_flag), , drop = FALSE]
    if (nrow(seg_issued) < 10) return(NULL)

    seg_bad  <- mean(seg_issued$performance_delinquency_flag, na.rm = TRUE)
    port_bad <- mean(df_full$performance_delinquency_flag[
      df_full$issued_flag == 1 & !is.na(df_full$performance_delinquency_flag)],
      na.rm = TRUE)
    seg_irr  <- mean(seg_issued$loan_irr, na.rm = TRUE)
    seg_lift <- if (!is.na(port_bad) && port_bad > 0) round(seg_bad / port_bad, 2) else NA_real_

    seg_info <- list(
      display_rule = seg_condition,
      bad_rate     = seg_bad,
      mean_irr     = seg_irr,
      n            = nrow(seg_issued),
      lift         = seg_lift
    )

    # Compute variable distributions
    var_dists <- tryCatch({
      av <- if (exists("analysis_vars", envir = globalenv()))
              get("analysis_vars", envir = globalenv()) else character(0)
      all_issued <- df_full[df_full$issued_flag == 1 &
                            !is.na(df_full$performance_delinquency_flag), , drop = FALSE]
      avail_vars <- intersect(unname(av), names(all_issued))
      avail_vars <- avail_vars[vapply(avail_vars,
        function(v) is.numeric(all_issued[[v]]), logical(1))]
      if (length(avail_vars) == 0) return(NULL)

      y <- all_issued$performance_delinquency_flag
      cors <- vapply(avail_vars, function(v) {
        x <- all_issued[[v]]
        if (sum(!is.na(x)) < 50 || sd(x, na.rm = TRUE) == 0) return(NA_real_)
        abs(suppressWarnings(cor(x, y, use = "pairwise.complete.obs")))
      }, numeric(1))
      cors <- cors[!is.na(cors)]
      top_vars <- names(sort(cors, decreasing = TRUE))[seq_len(min(5, length(cors)))]

      in_seg_issued <- in_segment[df_full$issued_flag == 1 &
                                  !is.na(df_full$performance_delinquency_flag)]
      dists <- list()
      for (v in top_vars) {
        seg_vals  <- all_issued[[v]][in_seg_issued]
        port_vals <- all_issued[[v]]
        dists[[v]] <- list(
          segment_mean  = mean(seg_vals, na.rm = TRUE),
          portfolio_mean = mean(port_vals, na.rm = TRUE),
          correlation   = cors[[v]]
        )
      }
      dists
    }, error = function(e) {
      message("[OppExplorer] opp_root_cause variable distributions failed: ", e$message)
      NULL
    })

    rca_text <- tryCatch(
      root_cause_analysis(seg_info, var_dists,
        portfolio_context = list(portfolio_bad = port_bad,
                                 portfolio_irr = opp_rv$overall_irr)),
      error = function(e) {
        message("[OppExplorer] root_cause_analysis failed: ", e$message)
        NULL
      }
    )
    if (is.null(rca_text) || !nzchar(rca_text)) return(NULL)

    tags$details(
      style = "margin-top:10px; margin-bottom:10px; background:#1a1f2e; border-left:3px solid #fb6e36; border-radius:6px; padding:10px 14px;",
      open = "open",
      tags$summary(style = "color:#fb6e36; font-weight:600; cursor:pointer;",
        icon("sparkles"), " Root Cause Analysis"),
      tags$p(style = "color:#d1d5db; font-size:13px; margin-top:6px;", rca_text)
    )
  })

  # ── Action Reasoning panel (UC4) ──
  output$opp_action_reasoning <- renderUI({
    req(opp_rv$df_full, opp_rv$action, opp_rv$in_segment)

    in_segment <- opp_rv$in_segment
    df_full <- opp_rv$df_full
    action <- opp_rv$action
    seg_condition <- opp_rv$segment_condition %||% "(no rule)"

    seg_issued <- df_full[in_segment & df_full$issued_flag == 1 &
                          !is.na(df_full$performance_delinquency_flag), , drop = FALSE]
    if (nrow(seg_issued) < 5) return(NULL)

    seg_bad  <- mean(seg_issued$performance_delinquency_flag, na.rm = TRUE)
    port_bad <- mean(df_full$performance_delinquency_flag[
      df_full$issued_flag == 1 & !is.na(df_full$performance_delinquency_flag)],
      na.rm = TRUE)
    seg_irr  <- mean(seg_issued$loan_irr, na.rm = TRUE)
    seg_lift <- if (!is.na(port_bad) && port_bad > 0) round(seg_bad / port_bad, 2) else NA_real_

    action_type <- if (identical(action, "Pricing")) "reprice" else "decline"
    seg_info <- list(
      display_rule = seg_condition,
      bad_rate     = seg_bad,
      mean_irr     = seg_irr,
      n            = nrow(seg_issued),
      lift         = seg_lift,
      avg_apr      = opp_rv$bl_seg_apr
    )

    reasoning_text <- tryCatch(
      explain_action(action_type, seg_info, marginal_data = NULL,
        portfolio_context = list(portfolio_bad = port_bad,
                                 portfolio_irr = opp_rv$overall_irr)),
      error = function(e) {
        message("[OppExplorer] explain_action failed: ", e$message)
        NULL
      }
    )
    if (is.null(reasoning_text) || !nzchar(reasoning_text)) return(NULL)

    tags$div(
      style = "background:#1a1f2e; border-left:3px solid #fb6e36; border-radius:6px; padding:10px 14px; margin-bottom:12px;",
      tags$span(style = "color:#fb6e36; font-weight:600;",
        icon("sparkles"), " Why this action?"),
      tags$p(style = "color:#d1d5db; font-size:13px; margin-top:6px;",
        reasoning_text)
    )
  })

  # Reprice Method & Models panel removed — the post-action Analysis Engine
  # modal (opp_show_analysis_modal) now covers this in the same visual
  # language as the Scanner, and without exposing calibration coefficients.

  sample_slot_title <- function(slot) {
    sim <- sample_slot_data(slot)
    if (is.null(sim) || is.null(sim$sample_preview)) return(NULL)
    label <- scenario_slot_label(slot)
    verb <- ifelse(identical(sim$action, "Decline"), "declined", "repriced")
    paste0(label, " Sample Preview — ", sim$n_affected %||% 0, " loans ", verb)
  }

  output$opp_sample_preview_cards <- renderUI({
    cards <- list()
    for (slot in c("A", "B")) {
      sim <- sample_slot_data(slot)
      if (is.null(sim) || is.null(sim$sample_preview)) next
      cards[[length(cards) + 1]] <- column(
        if (!is.null(sample_slot_data("A")$sample_preview) && !is.null(sample_slot_data("B")$sample_preview)) 6 else 12,
        div(class = "card", style = "padding:10px 15px;",
          fluidRow(
            column(8,
              h5(icon("table"), sample_slot_title(slot),
                style = "font-weight:bold; margin:0; display:inline-block;")
            ),
            column(4, style = "text-align:right;",
              actionLink(
                paste0("opp_sample_expand_btn_", slot),
                label = tagList(icon("expand"), " View affected loans"),
                style = "color:#fb6e36; font-size:12px;"
              )
            )
          )
        )
      )
    }
    if (length(cards) == 0) return(NULL)
    do.call(fluidRow, cards)
  })

  show_sample_modal <- function(slot) {
    sim <- sample_slot_data(slot)
    req(sim$sample_full)
    verb <- ifelse(identical(sim$action, "Decline"), "declined", "repriced")
    # Show ALL affected loans in the modal (not just the 50-row sample)
    display_df <- if (!is.null(sim$sample_all)) sim$sample_all else sim$sample_full
    output[[paste0("opp_sample_modal_table_", slot)]] <- renderDataTable({
      datatable(display_df,
        selection = "none", rownames = FALSE,
        options = list(pageLength = 25, dom = "tip",
                       scrollX = TRUE,
                       columnDefs = list(list(className = "dt-center", targets = "_all"))))
    })
    # Store for download handler
    opp_rv[[paste0("modal_download_", slot)]] <- display_df
    showModal(modalDialog(
      title = h4(icon("table"), paste0(
        scenario_slot_label(slot),
        " — ", sim$n_affected %||% 0, " total (", verb, ")"
      )),
      size = "l", easyClose = TRUE,
      footer = tagList(
        downloadButton(paste0("opp_download_affected_", slot),
          "Download CSV",
          class = "btn-sm",
          style = "background-color:#10b981; color:white; border:none; margin-right:8px;"),
        modalButton("Close")
      ),
      DTOutput(paste0("opp_sample_modal_table_", slot))
    ))
  }

  observeEvent(input$opp_sample_expand_btn_A, { show_sample_modal("A") })
  observeEvent(input$opp_sample_expand_btn_B, { show_sample_modal("B") })

  # Download handlers for affected loans CSV
  output$opp_download_affected_A <- downloadHandler(
    filename = function() {
      sim <- sample_slot_data("A")
      verb <- if (identical(sim$action, "Decline")) "declined" else "repriced"
      paste0("affected_loans_primary_", verb, "_",
             format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      df <- opp_rv$modal_download_A
      if (!is.null(df)) write.csv(df, file, row.names = FALSE)
    }
  )
  output$opp_download_affected_B <- downloadHandler(
    filename = function() {
      sim <- sample_slot_data("B")
      verb <- if (identical(sim$action, "Decline")) "declined" else "repriced"
      paste0("affected_loans_alternative_", verb, "_",
             format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      df <- opp_rv$modal_download_B
      if (!is.null(df)) write.csv(df, file, row.names = FALSE)
    }
  )

  # ---- Save Opportunity Finder policy to Policy Repository ----
  # ---- Save policy helper (shared by Save A / Save B) ----
  save_scenario_to_repo <- function(slot) {
    sim  <- opp_rv[[paste0("sim_", slot)]]

    if (is.null(sim)) {
      showNotification(paste0("Scenario ", slot, " has no simulation. Run Apply first."),
        type = "warning", duration = 3)
      return()
    }

    name <- trimws(input$opp_save_name)
    if (nchar(name) == 0) {
      showNotification("Please provide a policy name.", type = "warning", duration = 3)
      return()
    }

    # Source action parameters from the selected simulation slot
    saved_condition <- sim$segment_condition %||% "TRUE"
    saved_action    <- sim$action
    saved_bps       <- sim$bps %||% 0
    saved_elast     <- sim$elast %||% 5

    # Capture the CART tree and selected node for accurate segment replay.
    # saved_loan_ids pins the segment to exactly the Scanner drill-down set
    # (NULL when not in drill-down mode).
    saved_tree      <- sim$saved_tree
    saved_tree_vars <- sim$saved_tree_vars
    saved_node      <- sim$saved_node
    saved_loan_ids  <- sim$saved_loan_ids

    # Capture Opportunity Finder filters from the simulation slot
    saved_fico_filter    <- sim$filters$fico
    saved_partner_filter <- sim$filters$partner
    saved_repeat_filter  <- sim$filters$repeat_
    saved_mob_filter     <- sim$filters$mob

    # Policy function: dynamically replays the Opportunity Finder action
    # including downstream Layers 50/80/90 cascade.
    # Built in a clean environment so saveRDS doesn't capture the full
    # server scope (which would bloat the .rds file by hundreds of MB).
    policy_env <- new.env(parent = globalenv())
    policy_env$saved_condition      <- saved_condition
    policy_env$saved_action         <- saved_action
    policy_env$saved_bps            <- saved_bps
    policy_env$saved_elast          <- saved_elast
    policy_env$saved_tree           <- saved_tree
    policy_env$saved_tree_vars      <- saved_tree_vars
    policy_env$saved_node           <- saved_node
    policy_env$saved_fico_filter    <- saved_fico_filter
    policy_env$saved_partner_filter <- saved_partner_filter
    policy_env$saved_repeat_filter  <- saved_repeat_filter
    policy_env$saved_mob_filter     <- saved_mob_filter

    policy_fn <- local(function(data) {
      result <- data

      # Apply the same population filters that were active in Opportunity Finder
      pop_mask <- rep(TRUE, nrow(data))
      if (!("all" %in% saved_fico_filter) && length(saved_fico_filter) > 0 &&
          "fico_band" %in% names(data))
        pop_mask <- pop_mask & (data$fico_band %in% saved_fico_filter)
      if (!("all" %in% saved_partner_filter) && length(saved_partner_filter) > 0 &&
          "partner_channel" %in% names(data))
        pop_mask <- pop_mask & (data$partner_channel %in% saved_partner_filter)
      if (!("all" %in% saved_repeat_filter) && length(saved_repeat_filter) > 0 &&
          "borrower_repeat_type" %in% names(data))
        pop_mask <- pop_mask & (data$borrower_repeat_type %in% saved_repeat_filter)
      if (!("all" %in% saved_mob_filter) && length(saved_mob_filter) > 0 &&
          "mob_bins" %in% names(data))
        pop_mask <- pop_mask & (data$mob_bins %in% saved_mob_filter)

      # Identify segment using saved CART tree for exact rpart-consistent
      # node assignment (fixes boundary mismatch vs text condition eval)
      seg_mask <- if (!is.null(saved_tree) && !is.null(saved_node)) {
        pred_nodes <- assign_leaf_nodes(saved_tree, data, saved_tree_vars)
        !is.na(pred_nodes) & pred_nodes == saved_node
      } else {
        # Fallback for legacy saved policies without tree
        m <- tryCatch(
          with(data, eval(parse(text = saved_condition))),
          error = function(e) rep(FALSE, nrow(data))
        )
        m[is.na(m)] <- FALSE
        m
      }
      # Drill-down guard: when the saved policy came from a Scanner drill-down,
      # the CART tree was trained on a small subset, so a coarse/single-leaf
      # tree can match the whole book. Pin the segment to the Scanner's
      # original loan_ids so replay stays faithful to the saved intent.
      if (!is.null(saved_loan_ids) && length(saved_loan_ids) > 0 &&
          "loan_id" %in% names(data)) {
        seg_mask <- seg_mask &
                    as.character(data$loan_id) %in% saved_loan_ids
      }
      seg_mask <- seg_mask & pop_mask
      in_segment_applied <- seg_mask & (result$issued_flag == 1L)

      if (saved_action == "Decline") {
        to_decline <- seg_mask & (
          (!is.na(result$offered_decision_flag) &
           result$offered_decision_flag == "APPR") |
          (result$issued_flag == 1))
        result$offered_decision_flag[to_decline] <- "DECL"
        result$issued_flag[to_decline] <- 0L
      } else if (saved_bps > 0) {
        to_price <- seg_mask &
          !is.na(result$offered_decision_flag) &
          result$offered_decision_flag == "APPR"
        result$adjusted_apr_pre_term_cap[to_price] <-
          result$adjusted_apr_pre_term_cap[to_price] + saved_bps / 10000

        # Replay downstream policy layers (50, 80, 90)
        result <- replay_layers_50_80_90(result)

        # Sync issued_flag: newly declined loans can no longer be issued
        newly_declined <- result$offered_decision_flag != "APPR" &
                          data$offered_decision_flag == "APPR"
        result$issued_flag[newly_declined] <- 0L

        # Price elasticity
        elast_pct <- saved_elast / 100
        drop_rate <- min((saved_bps / 100) * elast_pct, 0.50)
        in_seg_issued <- in_segment_applied & data$issued_flag == 1L
        n_seg_issued  <- sum(in_seg_issued, na.rm = TRUE)
        n_drop        <- round(n_seg_issued * drop_rate)
        if (n_drop > 0 && n_seg_issued > 0) {
          seg_issued_idx <- which(in_seg_issued)
          fico_order <- order(data$fico_score[seg_issued_idx], decreasing = TRUE)
          drop_idx   <- seg_issued_idx[fico_order[seq_len(n_drop)]]
          result$issued_flag[drop_idx] <- 0L
        }
      }
      result
    }, envir = policy_env)

    # Generate next policy ID
    new_id <- max(c(mockPolicies$id,
                    if (!is.null(added_policy())) max(added_policy()$id) else 0L),
                  na.rm = TRUE) + 1L
    pid <- paste0("PL-", sprintf("%03d", new_id))

    # Build descriptive logic summaries using sim slot data
    new_policy <- data.frame(
      id                  = new_id,
      policyId            = pid,
      policyName          = paste0(name, " (Scenario ", slot, ")"),
      status              = "simulated",
      product             = "PL",
      version             = "v1.0",
      updatedAt           = Sys.Date(),
      endDate             = as.Date(NA),
      grading_logic       = "Baseline (unchanged)",
      pricing_logic       = if (saved_action == "Pricing")
                              paste0("Baseline + ", saved_bps, " BPS on targeted segment")
                            else "Baseline (unchanged)",
      decision_logic      = if (saved_action == "Decline")
                              "Baseline + decline targeted segment"
                            else "Baseline (unchanged)",
      term_logic          = "Baseline (unchanged)",
      affordability_logic = "Baseline (unchanged)",
      max_amount_logic    = "Baseline (unchanged)",
      r_logic             = NA_character_,
      stringsAsFactors    = FALSE
    )

    # Append to added policies
    existing <- added_policy()
    if (!is.null(existing)) {
      added_policy(bind_rows(existing, new_policy))
    } else {
      added_policy(new_policy)
    }

    # Register policy function so it appears in Policy Overlap simulations
    fns <- policy_fns_rv()
    fns[[pid]] <- policy_fn
    policy_fns_rv(fns)

    # Persist to disk so policies survive app restart
    persist_policies()

    # Store for download
    opp_rv$saved_policy_id   <- pid
    opp_rv$saved_policy_name <- name

    showNotification(
      paste0("Scenario ", slot, " saved as '", name, "' (", pid, ") in Policy Repository."),
      type = "message", duration = 4)

    updateTextInput(session, "opp_save_name", value = "")
  }

  observeEvent(input$opp_save_btn_A, { save_scenario_to_repo("A") })
  observeEvent(input$opp_save_btn_B, { save_scenario_to_repo("B") })

  # ---- SQL Policy Code Preview (Swap-Out) ----
  show_sql_modal <- function(sql_code, title) {
    showModal(modalDialog(
      title = tagList(icon("database"), title),
      size  = "l",
      easyClose = TRUE,
      tags$div(
        style = "position:relative;",
        tags$button(
          id = "copy_sql_btn", class = "btn btn-sm",
          style = paste0(
            "position:absolute; top:0; right:0; z-index:10;",
            "background:#f97316; color:white; border:none;",
            "padding:4px 12px; border-radius:4px; font-size:12px; cursor:pointer;"),
          icon("copy"), "Copy to Clipboard",
          onclick = "var t=document.getElementById('sql_code_block');
                     navigator.clipboard.writeText(t.innerText);
                     this.innerHTML='<i class=\"fa fa-check\"></i> Copied!';
                     setTimeout(function(){
                       document.getElementById('copy_sql_btn').innerHTML=
                         '<i class=\"fa fa-copy\"></i> Copy to Clipboard';
                     }, 2000);"
        ),
        tags$pre(id = "sql_code_block",
          style = paste0(
            "background:#1e293b; color:#e2e8f0; padding:16px; padding-top:36px;",
            "border-radius:6px; font-size:12.5px; line-height:1.5;",
            "max-height:70vh; overflow:auto; white-space:pre-wrap;",
            "font-family:'Fira Code','Source Code Pro','Consolas',monospace;"),
          sql_code
        )
      ),
      footer = modalButton("Close")
    ))
  }

  observeEvent(input$opp_sql_baseline, {
    bp <- opp_build_baseline_params()
    sql <- generate_policy_sql(bp, label = "Baseline Policy")
    show_sql_modal(sql, "Baseline Policy (SQL)")
  })

  show_scenario_sql <- function(slot) {
    sim <- opp_rv[[paste0("sim_", slot)]]
    if (is.null(sim)) {
      showNotification(paste0("Scenario ", slot, " has not been run yet."),
                       type = "warning", duration = 3)
      return()
    }
    bp  <- opp_build_baseline_params()
    act <- sim$action %||% "Decline"
    sql <- generate_policy_sql(bp,
      label        = paste0("Scenario ", slot, " — ", act,
                      if (act == "Pricing" && (sim$bps %||% 0) > 0)
                        paste0(" +", sim$bps, "bps") else ""),
      segment_cond = sim$segment_condition,
      action       = act,
      bps          = sim$bps %||% 0)
    show_sql_modal(sql, paste0("Scenario ", slot, " (SQL)"))
  }

  observeEvent(input$opp_sql_scenario_a, { show_scenario_sql("A") })
  observeEvent(input$opp_sql_scenario_b, { show_scenario_sql("B") })

  # ---- SQL Policy Code Preview (Swap-In) ----
  observeEvent(input$opp_swapin_sql_baseline, {
    bp <- opp_build_baseline_params()
    sql <- generate_policy_sql(bp, label = "Baseline Policy")
    show_sql_modal(sql, "Baseline Policy (SQL)")
  })

  observeEvent(input$opp_swapin_sql_new, {
    relaxed <- opp_rv$swapin_relaxed_params
    if (is.null(relaxed)) {
      showNotification("Run Analyze + Simulate first.", type = "warning", duration = 3)
      return()
    }
    sql <- generate_policy_sql(relaxed,
      label  = "Swap-In Policy (Relaxed Thresholds)",
      action = "Swap-In")
    show_sql_modal(sql, "Swap-In Policy (SQL)")
  })

  # ---- Save Swap-In policy to Policy Repository ----
  observeEvent(input$opp_swapin_save_btn, {
    relaxed_params <- opp_rv$swapin_relaxed_params
    cohort         <- opp_rv$swapin_cohort
    if (is.null(relaxed_params) || is.null(cohort)) {
      showNotification("Run Analyze + Simulate first.", type = "warning", duration = 3)
      return()
    }

    name <- trimws(input$opp_swapin_save_name)
    if (nchar(name) == 0) {
      showNotification("Please provide a policy name.", type = "warning", duration = 3)
      return()
    }

    reason <- input$opp_swapin_reason
    relax  <- input$opp_swapin_relax %||% 20

    # Capture filter state
    saved_fico_filter    <- input$opp_fico
    saved_partner_filter <- input$opp_partner
    saved_repeat_filter  <- input$opp_repeat
    saved_mob_filter     <- input$opp_mob

    # Build policy closure that runs full engine with relaxed params
    policy_env <- new.env(parent = globalenv())
    policy_env$saved_relaxed_params  <- relaxed_params
    policy_env$saved_fico_filter     <- saved_fico_filter
    policy_env$saved_partner_filter  <- saved_partner_filter
    policy_env$saved_repeat_filter   <- saved_repeat_filter
    policy_env$saved_mob_filter      <- saved_mob_filter

    policy_fn <- local(function(data) {
      # Apply population filters
      pop_mask <- rep(TRUE, nrow(data))
      if (!("all" %in% saved_fico_filter) && length(saved_fico_filter) > 0 &&
          "fico_band" %in% names(data))
        pop_mask <- pop_mask & (data$fico_band %in% saved_fico_filter)
      if (!("all" %in% saved_partner_filter) && length(saved_partner_filter) > 0 &&
          "partner_channel" %in% names(data))
        pop_mask <- pop_mask & (data$partner_channel %in% saved_partner_filter)
      if (!("all" %in% saved_repeat_filter) && length(saved_repeat_filter) > 0 &&
          "borrower_repeat_type" %in% names(data))
        pop_mask <- pop_mask & (data$borrower_repeat_type %in% saved_repeat_filter)
      if (!("all" %in% saved_mob_filter) && length(saved_mob_filter) > 0 &&
          "mob_bins" %in% names(data))
        pop_mask <- pop_mask & (data$mob_bins %in% saved_mob_filter)

      # Extract policy_input_data columns for engine run
      pid_cols <- c("loan_id", "fico_score", "annual_income", "monthly_rent",
        "monthly_debt_payment", "debt_to_income_ratio", "credit_file_age_months",
        "delinquent_accounts_count", "months_since_last_delinquency",
        "internal_file_age_months", "term_months", "partner_channel",
        "pricing_test_variant", "risk_model_score_1", "risk_model_score_2",
        "risk_model_score_3")
      avail_cols <- intersect(pid_cols, names(data))
      pid_subset <- data[pop_mask, avail_cols, drop = FALSE]

      # Run engine with relaxed params
      engine_out <- apply_policy_with_params(pid_subset, saved_relaxed_params)

      # Merge engine outputs back into result
      result <- data
      pop_idx <- which(pop_mask)
      result$offered_decision_flag[pop_idx] <- engine_out$offered_decision_flag
      result$risk_grade[pop_idx]            <- engine_out$risk_grade
      result$interest_rate_apr[pop_idx] <- ifelse(
        engine_out$offered_decision_flag == "APPR",
        engine_out$offered_final_apr,
        result$interest_rate_apr[pop_idx])

      # Sync issued_flag for newly declined
      newly_declined <- pop_idx[engine_out$offered_decision_flag != "APPR" &
                                data$offered_decision_flag[pop_idx] == "APPR"]
      result$issued_flag[newly_declined] <- 0L

      result
    }, envir = policy_env)

    # Generate next policy ID
    new_id <- max(c(mockPolicies$id,
                    if (!is.null(added_policy())) max(added_policy()$id) else 0L),
                  na.rm = TRUE) + 1L
    pid <- paste0("PL-", sprintf("%03d", new_id))

    # Describe the relaxation
    reason_label <- sub("^\\d+\\.\\s*", "", reason)
    decision_desc <- sprintf("Relax %s by %s", reason_label,
      if (reason == "04. HighDTI") paste0(relax, "pp") else as.character(relax))

    new_policy <- data.frame(
      id                  = new_id,
      policyId            = pid,
      policyName          = name,
      status              = "simulated",
      product             = "PL",
      version             = "v1.0",
      updatedAt           = Sys.Date(),
      endDate             = as.Date(NA),
      grading_logic       = "Baseline (unchanged)",
      pricing_logic       = "Baseline (unchanged)",
      decision_logic      = decision_desc,
      term_logic          = "Baseline (unchanged)",
      affordability_logic = "Baseline (unchanged)",
      max_amount_logic    = "Baseline (unchanged)",
      r_logic             = NA_character_,
      stringsAsFactors    = FALSE
    )

    existing <- added_policy()
    if (!is.null(existing)) {
      added_policy(bind_rows(existing, new_policy))
    } else {
      added_policy(new_policy)
    }

    fns <- policy_fns_rv()
    fns[[pid]] <- policy_fn
    policy_fns_rv(fns)

    persist_policies()

    opp_rv$saved_policy_id   <- pid
    opp_rv$saved_policy_name <- name

    showNotification(
      paste0("Swap-In policy saved as '", name, "' (", pid, ") in Policy Repository."),
      type = "message", duration = 4)

    updateTextInput(session, "opp_swapin_save_name", value = "")
  })

  build_policy_download <- function(slot) {
    sim <- opp_rv[[paste0("sim_", slot)]]
    if (is.null(sim)) {
      stop(paste0("Scenario ", slot, " has not been run yet."))
    }
    name <- trimws(input$opp_save_name)
    if (!nzchar(name)) {
      name <- sim$label %||% if (slot == "A") "Primary Action" else "Alternative Action"
    }
    list(
      pid = sim$policy_id %||% paste0("SCENARIO_", slot),
      name = name,
      action = sim$action %||% "Decline",
      bps = sim$bps %||% 0,
      condition = sim$segment_condition %||% "TRUE",
      mode = opp_rv$strategy_mode %||% "swapout_delinq",
      elast = sim$elast %||% 5
    )
  }

  write_policy_download <- function(file, slot) {
      cfg <- build_policy_download(slot)
      pid       <- cfg$pid
      name      <- cfg$name
      action    <- cfg$action
      bps       <- cfg$bps
      condition <- cfg$condition
      mode      <- cfg$mode
      elast     <- cfg$elast

      # Build the R code
      header <- c(
        paste0("# ", paste(rep("=", 70), collapse = "")),
        paste0("# Credit Policy Strategy: ", name),
        paste0("# Policy ID: ", pid),
        paste0("# Generated: ", Sys.time()),
        paste0("# Strategy Mode: ", mode),
        paste0("# Action: ", action),
        if (action == "Pricing") paste0("# BPS Adjustment: +", bps, " BPS"),
        if (action == "Pricing") paste0("# Price Elasticity: ", elast, "% drop per 100 BPS"),
        paste0("# ", paste(rep("=", 70), collapse = "")),
        "",
        paste0("# Segment Rule (from CART segmentation):"),
        paste0("#   ", condition),
        ""
      )

      # Parse condition into R expression
      # CART rules use var < val, var >= val syntax which is valid R
      segment_fn <- c(
        "# ---- Segment identification ----",
        "identify_segment <- function(data) {",
        "  with(data, {",
        paste0("    ", condition),
        "  })",
        "}",
        ""
      )

      if (action == "Decline") {
        action_fn <- c(
          "# ---- Policy action: Decline targeted segment ----",
          "apply_policy <- function(data) {",
          "  result <- data",
          "  seg <- identify_segment(data)",
          "  # Decline all approved or historically-issued loans in the segment",
          "  to_decline <- seg & (",
          "    (!is.na(result$offered_decision_flag) &",
          "     result$offered_decision_flag == \"APPR\") |",
          "    (result$issued_flag == 1)",
          "  )",
          "  result$offered_decision_flag[to_decline] <- \"DECL\"",
          "  result$issued_flag[to_decline] <- 0L",
          "  result",
          "}",
          ""
        )
      } else {
        action_fn <- c(
          paste0("# ---- Policy action: Pricing +", bps, " BPS on targeted segment ----"),
          "apply_policy <- function(data) {",
          "  result <- data",
          "  seg <- identify_segment(data)",
          "  # Raise policy APR (adjusted_apr_pre_term_cap) for approved loans",
          "  to_price <- seg &",
          "    !is.na(result$offered_decision_flag) &",
          "    result$offered_decision_flag == \"APPR\"",
          paste0("  bps_adj <- ", bps, " / 10000"),
          "  result$adjusted_apr_pre_term_cap[to_price] <-",
          "    result$adjusted_apr_pre_term_cap[to_price] + bps_adj",
          "",
          "  # ---- Replay downstream policy layers (50, 80, 90) ----",
          "  sql_least_v <- function(a, b) { out <- pmin(a, b); out[is.na(a) | is.na(b)] <- NA_real_; out }",
          "  new_apr <- result$adjusted_apr_pre_term_cap",
          "",
          "  # Layer 10 intermediates",
          "  ann_inc  <- ifelse(is.na(result$annual_income), 0, result$annual_income)",
          "  mth_debt <- ifelse(is.na(result$monthly_debt_payment), 0, result$monthly_debt_payment)",
          "  rent_est <- ifelse(!is.na(result$monthly_rent), pmax(result$monthly_rent, 300), NA_real_)",
          "  soc_tax  <- ifelse(ann_inc < 140000, ann_inc * 0.06, 0)",
          "  med_tax  <- ann_inc * 0.04",
          "  post_tax <- ann_inc - soc_tax - med_tax",
          "",
          "  # Layer 50: 72-month APR cap, income-based limit, max approved amount",
          "  tm <- result$term_months",
          "  result$final_apr <- ifelse(!is.na(tm) & tm == 72, pmin(new_apr, 0.18), new_apr)",
          "  result$income_based_limit_cap <- (post_tax - 12 * mth_debt) / (1 - result$final_apr)",
          "  result$maximum_approved_amount <- sql_least_v(result$income_based_limit_cap, result$exposure_limit_cap)",
          "",
          "  # Layer 80: offered APR, affordability, max offered amount",
          "  ot <- result$offered_term_months",
          "  result$offered_final_apr <- ifelse(!is.na(ot) & ot == 72, pmin(new_apr, 0.18), new_apr)",
          "  mth_pti <- post_tax / 12.0",
          "  pti_cap <- ifelse(result$risk_grade == \"High\", 0.25,",
          "             ifelse(result$risk_grade == \"Med\",  0.20,",
          "             ifelse(result$risk_grade == \"Low\",  0.15, 0.20)))",
          "  exist_oblig <- mth_debt",
          "  max_install <- (pti_cap * mth_pti) - exist_oblig",
          "  r_new <- result$offered_final_apr / 12.0",
          "  n_new <- ot",
          "  result$affordability_limit_cap <- ifelse(",
          "    max_install <= 0, 0,",
          "    ifelse(r_new == 0, max_install * n_new,",
          "           max_install * ((1 + r_new)^n_new - 1) / (r_new * (1 + r_new)^n_new)))",
          "  result$maximum_offered_amount <- sql_least_v(result$maximum_approved_amount, result$affordability_limit_cap)",
          "",
          "  # Layer 90: full decline waterfall",
          "  flag <- rep(\"APPR\", nrow(result))",
          "  c1 <- is.na(result$fico_score) | result$fico_score > 850 | result$fico_score < 300",
          "  flag[c1] <- \"01. Fico\"",
          "  c2 <- !c1 & !is.na(result$credit_file_age_months) & result$credit_file_age_months <= 30",
          "  flag[c2] <- \"02. Thin file\"",
          "  c3 <- !(c1 | c2) & !is.na(result$delinquent_accounts_count) & result$delinquent_accounts_count > 1",
          "  flag[c3] <- \"03. Delinquency decline\"",
          "  c4 <- !(c1 | c2 | c3) & !is.na(result$debt_to_income_ratio) & result$debt_to_income_ratio > 0.50",
          "  flag[c4] <- \"04. HighDTI\"",
          "  c5 <- !(c1 | c2 | c3 | c4) & !is.na(result$maximum_offered_amount) & result$maximum_offered_amount < 1",
          "  flag[c5] <- \"05. max_amount_limit\"",
          "  result$offered_decision_flag <- flag",
          "",
          "  # Sync interest_rate_apr from offered_final_apr so KPIs reflect repricing",
          "  result$interest_rate_apr <- ifelse(",
          "    result$offered_decision_flag == \"APPR\",",
          "    result$offered_final_apr,",
          "    result$interest_rate_apr)",
          "",
          "  # Sync issued_flag: newly declined loans can no longer be issued",
          "  newly_declined <- result$offered_decision_flag != \"APPR\" &",
          "                    data$offered_decision_flag == \"APPR\"",
          "  result$issued_flag[newly_declined] <- 0L",
          "",
          "  # Price elasticity: higher rate reduces take-up",
          paste0("  elasticity <- ", elast, " / 100  # ", elast, "% drop per 100 BPS"),
          paste0("  drop_rate  <- min((", bps, " / 100) * elasticity, 0.50)"),
          "  seg_issued_idx <- which(to_price & result$issued_flag == 1L)",
          "  n_drop <- round(length(seg_issued_idx) * drop_rate)",
          "  if (n_drop > 0) {",
          "    fico_order <- order(result$fico_score[seg_issued_idx], decreasing = TRUE)",
          "    drop_idx   <- seg_issued_idx[fico_order[seq_len(n_drop)]]",
          "    result$issued_flag[drop_idx] <- 0L",
          "  }",
          "  result",
          "}",
          ""
        )
      }

      usage <- c(
        "# ---- Usage ----",
        "# source(\"this_file.R\")",
        "# modified_data <- apply_policy(credit_data)",
        ""
      )

      writeLines(c(header, segment_fn, action_fn, usage), file)
  }

  # SQL download helpers
  build_sql_download <- function(slot) {
    sim <- opp_rv[[paste0("sim_", slot)]]
    if (is.null(sim)) stop(paste0("Scenario ", slot, " has not been run yet."))
    bp  <- opp_build_baseline_params()
    act <- sim$action %||% "Decline"
    lbl <- sim$label %||% if (slot == "A") "Primary Action" else "Alternative Action"
    name <- trimws(input$opp_save_name)
    if (nzchar(name)) lbl <- name
    generate_policy_sql(bp,
      label        = paste0(lbl, " — ", act,
                      if (act == "Pricing" && (sim$bps %||% 0) > 0)
                        paste0(" +", sim$bps, "bps") else ""),
      segment_cond = sim$segment_condition,
      action       = act,
      bps          = sim$bps %||% 0)
  }

  output$opp_download_policy_A <- downloadHandler(
    filename = function() {
      cfg <- build_policy_download("A")
      nm <- gsub("[^A-Za-z0-9_-]", "_", cfg$name)
      paste0("credit_policy_", gsub("\\s+", "_", trimws(nm)), ".sql")
    },
    content = function(file) writeLines(build_sql_download("A"), file)
  )

  output$opp_download_policy_B <- downloadHandler(
    filename = function() {
      cfg <- build_policy_download("B")
      nm <- gsub("[^A-Za-z0-9_-]", "_", cfg$name)
      paste0("credit_policy_", gsub("\\s+", "_", trimws(nm)), ".sql")
    },
    content = function(file) writeLines(build_sql_download("B"), file)
  )

  # ============================================================
  # SWAP-IN (EXPAND) SERVER LOGIC
  # ============================================================

  # ---- Strategy mode visibility toggle ----
  observeEvent(input$opp_strategy_mode, {
    mode <- input$opp_strategy_mode
    if (mode == "swapin") {
      hideElement("opp_rules_display")
      hideElement("opp_action_row")
      hideElement("opp_perf_show")
      hideElement("opp_growth_panel")
      showElement("opp_swapin_panel")
    } else if (mode == "growth_scout") {
      hideElement("opp_rules_display")
      hideElement("opp_action_row")
      hideElement("opp_perf_show")
      hideElement("opp_swapin_panel")
      showElement("opp_growth_panel")
    } else {
      showElement("opp_rules_display")
      hideElement("opp_swapin_panel")
      hideElement("opp_growth_panel")
      hideElement("opp_action_row")
      hideElement("opp_perf_show")
    }
  })

  # ============================================================
  # GROWTH SCOUT (Find Pricing Headroom) SERVER LOGIC
  # ============================================================

  # Profile-first cross-tab: filter approved applicants to those with strong
  # credit profile (FICO >= threshold AND grade in selected set), then rank
  # cells by take rate ASCENDING. The premise: premium customers who declined
  # likely got better pricing elsewhere - candidates for a price cut. IRR
  # hurdle is a guard only (don't surface segments already under-priced).
  growth_segments_data <- eventReactive(input$opp_growth_run, {
    message("[GrowthScout] cross-tab compute triggered")
    df_full <- credit_data[opp_pop_keep(), , drop = FALSE]
    shiny::validate(need(nrow(df_full) > 0,
      "No applicants in the selected population. Widen filters."))

    irr_data <- loan_irr_cache()
    shiny::validate(need(!is.null(irr_data) && nrow(irr_data) > 0,
      "Performance tape required for segment IRR (the under-priced guard)."))

    needed <- c("offered_decision_flag", "issued_flag", "fico_band",
                "fico_score", "risk_grade", "partner_channel",
                "borrower_repeat_type", "interest_rate_apr", "loan_id")
    missing <- setdiff(needed, names(df_full))
    shiny::validate(need(length(missing) == 0,
      paste("Missing required columns:", paste(missing, collapse = ", "))))

    min_n     <- max(10L, as.integer(input$opp_growth_min_n %||% 50))
    min_fico  <- as.numeric(input$opp_growth_min_fico %||% 700)
    grades    <- input$opp_growth_grades %||% "High"
    hurdle    <- as.numeric(input$opp_growth_hurdle %||% 12) / 100

    # Restrict to approved applicants meeting profile criteria
    df_premium <- df_full %>%
      filter(!is.na(offered_decision_flag) & offered_decision_flag == "APPR",
             !is.na(fico_score) & fico_score >= min_fico,
             risk_grade %in% grades)

    shiny::validate(need(nrow(df_premium) > 0,
      "No approved applicants meet the profile criteria. Lower Min FICO or add more quality grades."))

    # Cell-level metrics within the premium pool
    cells <- df_premium %>%
      group_by(fico_band, risk_grade, partner_channel, borrower_repeat_type) %>%
      summarise(
        n_approved = dplyr::n(),
        n_issued   = sum(issued_flag == 1, na.rm = TRUE),
        take_rate  = n_issued / pmax(n_approved, 1L),
        avg_apr    = mean(interest_rate_apr[issued_flag == 1], na.rm = TRUE),
        .groups    = "drop") %>%
      filter(n_issued >= min_n)

    shiny::validate(need(nrow(cells) > 0,
      sprintf("No cells in the premium pool meet the %d-loan size guard. Lower the threshold or widen profile.", min_n)))

    # Premium-pool average take rate (the bar each cell is measured against)
    premium_take <- sum(cells$n_issued, na.rm = TRUE) /
                    pmax(sum(cells$n_approved, na.rm = TRUE), 1L)
    cells <- cells %>%
      mutate(take_gap_vs_premium = take_rate - premium_take)

    # Segment IRR per cell (issued loans only) - used as guard, not score
    df_iss <- df_premium %>% filter(issued_flag == 1)
    df_iss$loan_id   <- as.character(df_iss$loan_id)
    irr_join         <- irr_data
    irr_join$loan_id <- as.character(irr_join$loan_id)

    cell_irr <- df_iss %>%
      inner_join(irr_join, by = "loan_id") %>%
      filter(is.finite(loan_irr)) %>%
      group_by(fico_band, risk_grade, partner_channel, borrower_repeat_type) %>%
      summarise(segment_irr = mean(loan_irr, na.rm = TRUE),
                .groups     = "drop")

    cells %>%
      left_join(cell_irr,
                by = c("fico_band", "risk_grade", "partner_channel",
                       "borrower_repeat_type")) %>%
      mutate(irr_headroom = segment_irr - hurdle) %>%
      # Guard: keep only cells still profitable above the hurdle. Cells with
      # missing IRR (no IRR-cache match) are kept so the user still sees them.
      filter(is.na(segment_irr) | irr_headroom >= 0) %>%
      arrange(take_rate)
  })

  # Renderer for the cross-tab table
  output$opp_growth_table <- DT::renderDataTable({
    d <- growth_segments_data()
    message(sprintf("[GrowthScout] renderDataTable: d has %d rows", nrow(d)))
    shiny::validate(need(nrow(d) > 0,
      "No segments meet the criteria. Try lowering the minimum issued threshold or hurdle."))

    # Keep numeric columns as numeric so styleInterval and column ordering
    # work; let DT format them for display.
    d_disp <- d %>%
      transmute(
        Segment = paste(fico_band, risk_grade, partner_channel,
                        borrower_repeat_type, sep = " / "),
        `Issued`              = n_issued,
        `Take Rate`           = take_rate,
        `Take Gap vs Premium` = take_gap_vs_premium,
        `Avg APR`             = avg_apr,
        `Segment IRR`         = segment_irr
      )
    message(sprintf("[GrowthScout] d_disp has %d rows, %d cols",
      nrow(d_disp), ncol(d_disp)))

    DT::datatable(d_disp,
      rownames = FALSE,
      options  = list(pageLength = 15, scrollX = TRUE, dom = "tip",
                      order = list()),
      selection = "none") %>%
      DT::formatPercentage(c("Take Rate", "Take Gap vs Premium",
                             "Avg APR", "Segment IRR"), digits = 1) %>%
      DT::formatStyle("Take Rate",
        fontWeight = "bold") %>%
      DT::formatStyle("Take Gap vs Premium",
        color = DT::styleInterval(c(-1e-9), c("#dc2626", "#10b981")))
  })

  # CART discovery on the PROFILE-FILTERED premium pool. Target is
  # issued_flag (probability of taking the offer); leaves are ranked by
  # take rate ASCENDING - the lowest-take leaves are the strongest
  # candidates for a price cut.
  growth_cart_data <- eventReactive(input$opp_growth_discover, {
    df_full <- credit_data[opp_pop_keep(), , drop = FALSE]
    shiny::validate(need(nrow(df_full) > 0,
      "No applicants in the selected population."))

    irr_data <- loan_irr_cache()
    shiny::validate(need(!is.null(irr_data) && nrow(irr_data) > 0,
      "Performance tape required for segment IRR."))

    min_fico <- as.numeric(input$opp_growth_min_fico %||% 700)
    grades   <- input$opp_growth_grades %||% "High"
    hurdle   <- as.numeric(input$opp_growth_hurdle %||% 12) / 100

    df_premium <- df_full %>%
      filter(!is.na(offered_decision_flag) & offered_decision_flag == "APPR",
             !is.na(fico_score) & fico_score >= min_fico,
             risk_grade %in% grades)
    shiny::validate(need(nrow(df_premium) >= 200,
      "Not enough premium-pool applicants for CART (need >= 200). Lower Min FICO or add more grades."))

    cart_vars <- intersect(c(
      "fico_band", "partner_channel", "borrower_repeat_type",
      "fico_score", "annual_income", "debt_to_income_ratio",
      "credit_file_age_months", "interest_rate_apr"
    ), names(df_premium))

    for (v in cart_vars) {
      if (is.character(df_premium[[v]])) df_premium[[v]] <- as.factor(df_premium[[v]])
    }

    df_cart <- df_premium %>%
      select(all_of(c(cart_vars, "issued_flag", "loan_id",
                      "interest_rate_apr"))) %>%
      filter(complete.cases(.))
    shiny::validate(need(nrow(df_cart) >= 200,
      "Too few complete-case rows for CART after filtering."))

    formula <- as.formula(paste("issued_flag ~",
                                paste(cart_vars, collapse = "+")))
    tree <- tryCatch(
      rpart(formula, data = df_cart, method = "anova",
            control = rpart.control(maxdepth = 4, minbucket = 100, cp = 0.002)),
      error = function(e) {
        message("[GrowthScout] CART training: ", e$message)
        NULL
      })
    shiny::validate(need(!is.null(tree), "CART training failed."))

    leaf_ids <- as.integer(rownames(tree$frame)[tree$frame$var == "<leaf>"])
    shiny::validate(need(length(leaf_ids) >= 2,
      "Tree found no meaningful splits inside the premium pool. Try widening the profile filter."))

    paths      <- path.rpart(tree, nodes = leaf_ids, print.it = FALSE)
    conditions <- vapply(paths, function(p) paste(p[-1], collapse = " & "),
                         character(1))
    conditions[!nzchar(conditions)] <- "(root)"
    if (exists("simplify_cart_condition", mode = "function")) {
      conditions <- vapply(conditions, simplify_cart_condition,
                           character(1), USE.NAMES = TRUE)
    }

    df_cart$.leaf <- as.integer(rownames(tree$frame))[tree$where]

    leaf_take <- df_cart %>%
      group_by(.leaf) %>%
      summarise(
        n_approved = dplyr::n(),
        n_issued   = sum(issued_flag == 1, na.rm = TRUE),
        take_rate  = n_issued / pmax(n_approved, 1L),
        avg_apr    = mean(interest_rate_apr[issued_flag == 1], na.rm = TRUE),
        .groups    = "drop")

    irr_join         <- irr_data
    irr_join$loan_id <- as.character(irr_join$loan_id)
    df_iss <- df_cart %>% filter(issued_flag == 1)
    df_iss$loan_id <- as.character(df_iss$loan_id)
    leaf_irr <- df_iss %>%
      inner_join(irr_join, by = "loan_id") %>%
      filter(is.finite(loan_irr)) %>%
      group_by(.leaf) %>%
      summarise(segment_irr = mean(loan_irr, na.rm = TRUE),
                .groups     = "drop")

    leaf_take %>%
      left_join(leaf_irr, by = ".leaf") %>%
      mutate(
        rule         = conditions[match(as.character(.leaf), names(conditions))],
        irr_headroom = segment_irr - hurdle
      ) %>%
      filter(n_issued >= 30,
             is.na(segment_irr) | irr_headroom >= 0) %>%
      arrange(take_rate)
  })

  output$opp_growth_cart_table <- DT::renderDataTable({
    d <- growth_cart_data()
    shiny::validate(need(nrow(d) > 0,
      "No CART-discovered segments with positive IRR headroom and adequate sample size."))

    d_disp <- d %>%
      transmute(
        Rule          = rule,
        `Issued`      = n_issued,
        `Take Rate`   = take_rate,
        `Avg APR`     = avg_apr,
        `Segment IRR` = segment_irr
      )

    DT::datatable(d_disp,
      rownames = FALSE,
      options  = list(pageLength = 10, scrollX = TRUE, dom = "tip",
                      order = list()),
      selection = "none") %>%
      DT::formatPercentage(c("Take Rate", "Avg APR", "Segment IRR"),
                           digits = 1) %>%
      DT::formatStyle("Take Rate", fontWeight = "bold")
  })

  # ---- Dynamic threshold relaxation input ----
  output$opp_swapin_relax_ui <- renderUI({
    reason <- input$opp_swapin_reason
    if (reason == "01. Fico") {
      numericInput("opp_swapin_relax", "Relax FICO Minimum By (points)",
        value = 20, min = 5, max = 200, step = 10)
    } else if (reason == "02. Thin file") {
      numericInput("opp_swapin_relax", "Reduce File Age Threshold By (months)",
        value = 6, min = 3, max = 30, step = 3)
    } else if (reason == "03. Delinquency decline") {
      numericInput("opp_swapin_relax", "Increase Max Delinquent Accounts By",
        value = 1, min = 1, max = 5, step = 1)
    } else if (reason == "04. HighDTI") {
      numericInput("opp_swapin_relax", "Increase DTI Cutoff By (pp)",
        value = 5, min = 1, max = 20, step = 1)
    } else if (reason == "05. HardInquiry") {
      numericInput("opp_swapin_relax", "Increase Max Hard Inquiries By",
        value = 1, min = 1, max = 5, step = 1)
    }
  })

  # ---- Show count of declined applicants (overall + selected reason) ----
  output$opp_swapin_n_total_declined <- renderUI({
    n <- sum(credit_data$offered_decision_flag != "APPR", na.rm = TRUE)
    kpi_box(fmt_k(n), "Overall Declines")
  })

  output$opp_swapin_n_declined <- renderUI({
    reason <- input$opp_swapin_reason
    n <- sum(credit_data$offered_decision_flag == reason, na.rm = TRUE)
    lbl <- sub("^\\d+\\.\\s*", "", reason)
    kpi_box(fmt_k(n), paste0("Declined: ", lbl))
  })

  # ---- Analyze Neighbor Band ----
  observeEvent(input$opp_swapin_analyze, {
    hideElement("opp_sample_preview_panel")

    tape <- perf_tape()
    if (is.null(tape)) {
      showNotification("Performance tape required for Swap-In analysis.",
        type = "warning", duration = 4)
      return()
    }

    reason <- input$opp_swapin_reason
    relax  <- input$opp_swapin_relax %||% 20

    # Current policy thresholds (from Layer 60 defaults)
    thresholds <- get_active_decline_thresholds()
    threshold <- thresholds[[reason]]

    # ---- Show analysis pipeline ----
    pipeline_show(session, "opp_swapin_pipeline", list(
      list(label = "Searching calibration cache for pricing experiments"),
      list(label = "Projecting bad rate from empirical data"),
      list(label = "Estimating take-rate elasticity"),
      list(label = "Running policy cascade (Layers 10\u201390)"),
      list(label = "Detecting newly eligible applicants")
    ))

    # Step 0: Neighbor band lookup
    neighbor <- get_neighbor_metrics(tape, credit_data, reason, threshold)

    if (neighbor$n_loans == 0) {
      pipeline_hide(session, "opp_swapin_pipeline")
      showNotification("No issued loans found in the neighbor band. Try a different decline reason.",
        type = "warning", duration = 4)
      return()
    }

    opp_rv$swapin_neighbor <- neighbor

    # ---- Step 0 done: calibration lookup ----
    cc_global <- if (exists("calibration_cache_global", envir = .GlobalEnv))
      get("calibration_cache_global", envir = .GlobalEnv) else NULL
    primary_var <- .decline_reason_to_attribute(reason)
    grade_mix <- if (!is.null(neighbor$neighbor_loan_ids) && length(neighbor$neighbor_loan_ids) > 0) {
      nb_grades <- credit_data$risk_grade[
        as.character(credit_data$loan_id) %in% as.character(neighbor$neighbor_loan_ids)]
      nb_grades <- nb_grades[!is.na(nb_grades)]
      if (length(nb_grades) > 0) prop.table(table(nb_grades)) else c(High=0, Med=1, Low=0)
    } else c(High=0, Med=1, Low=0)

    # Build calibration result text
    cal_result <- if (!is.null(cc_global)) {
      n_elast <- if (!is.null(cc_global$elasticity)) nrow(cc_global$elasticity) else 0
      paste0(format(neighbor$n_loans, big.mark = ","), " neighbor loans, ",
             n_elast, " calibration cells")
    } else {
      paste0(format(neighbor$n_loans, big.mark = ","), " neighbor loans")
    }
    pipeline_step_done(session, "opp_swapin_pipeline", 0L, cal_result)

    # ---- Step 1: Bad rate projection ----
    seg_def <- list(
      primary_var = primary_var,
      cutoff_val  = threshold,
      direction   = "above",
      grade_mix   = grade_mix,
      loan_ids    = neighbor$neighbor_loan_ids
    )
    br_proj <- tryCatch(
      get_segment_bad_rate_projection(seg_def, credit_data,
        perf_tape = tape, calibration_cache = cc_global),
      error = function(e) NULL)
    opp_rv$swapin_bad_rate_projection <- br_proj

    br_result <- if (!is.null(br_proj)) {
      src_label <- switch(as.character(br_proj$layer),
        "1" = "pricing experiment",
        "2" = "neighbor band",
        "3" = "grade proxy",
        "fallback")
      paste0(round(br_proj$value * 100, 1), "% \u2014 ", src_label)
    } else "fallback estimate"
    pipeline_step_done(session, "opp_swapin_pipeline", 1L, br_result)

    # ---- Step 2: Elasticity estimation ----
    elast_proj <- tryCatch(
      get_segment_elasticity(primary_var, grade_mix,
        calibration_cache = cc_global$elasticity,
        obs_elasticity_cache = cc_global$obs_elasticity),
      error = function(e) NULL)
    opp_rv$swapin_elasticity_projection <- elast_proj

    el_result <- if (!is.null(elast_proj) && !is.na(elast_proj$value)) {
      src_label <- switch(as.character(elast_proj$layer),
        "1" = "direct test",
        "2" = "obs model",
        "default")
      paste0(round(elast_proj$value, 2), " pp/100bps \u2014 ", src_label)
    } else "fallback"
    pipeline_step_done(session, "opp_swapin_pipeline", 2L, el_result)

    # ---- Step 3 & 4: Policy cascade + newly eligible ----
    pop_keep     <- opp_pop_keep()
    pid_filtered <- policy_input_data[pop_keep, , drop = FALSE]
    cd_filtered  <- credit_data[pop_keep, , drop = FALSE]
    default_params <- opp_build_baseline_params()

    # Build relaxed params based on decline reason
    relaxed_params <- default_params
    if (reason == "01. Fico")              relaxed_params$fico_min        <- default_params$fico_min - relax
    if (reason == "02. Thin file")         relaxed_params$thin_file_months <- default_params$thin_file_months - relax
    if (reason == "03. Delinquency decline") relaxed_params$delinquency_max <- default_params$delinquency_max + relax
    if (reason == "04. HighDTI")           relaxed_params$dti_cutoff      <- default_params$dti_cutoff + relax / 100
    if (reason == "05. HardInquiry")       relaxed_params$n_hard_inq_6m_max <- default_params$n_hard_inq_6m_max + relax

    # Run both engines
    baseline_df <- apply_policy_with_params(pid_filtered, default_params)
    scenario_df <- apply_policy_with_params(pid_filtered, relaxed_params)

    pipeline_step_done(session, "opp_swapin_pipeline", 3L,
      paste0(format(nrow(pid_filtered), big.mark = ","), " applications scored"))

    # Carry historical columns into both
    carry_cols <- c("issued_flag", "issued_amount", "performance_delinquency_flag",
                    "booking_vintage", "fico_band", "borrower_repeat_type",
                    "mob_bins", "date", "months_on_books", "product_type")
    for (col in carry_cols) {
      if (col %in% names(cd_filtered)) {
        if (!(col %in% names(baseline_df))) baseline_df[[col]] <- cd_filtered[[col]]
        if (!(col %in% names(scenario_df))) scenario_df[[col]] <- cd_filtered[[col]]
      }
    }
    baseline_df$issued_flag <- cd_filtered$issued_flag
    scenario_df$issued_flag <- cd_filtered$issued_flag
    baseline_df$interest_rate_apr <- ifelse(
      baseline_df$offered_decision_flag == "APPR",
      baseline_df$offered_final_apr, cd_filtered$interest_rate_apr)
    scenario_df$interest_rate_apr <- ifelse(
      scenario_df$offered_decision_flag == "APPR",
      scenario_df$offered_final_apr, baseline_df$interest_rate_apr)

    # Detect newly eligible (baseline DECL -> scenario APPR)
    newly_eligible <- scenario_df$offered_decision_flag == "APPR" &
                      baseline_df$offered_decision_flag != "APPR"
    n_swapin <- sum(newly_eligible, na.rm = TRUE)
    opp_rv$swapin_declined_n <- n_swapin

    # Store engine state for simulate step
    opp_rv$swapin_baseline_df    <- baseline_df
    opp_rv$swapin_scenario_df    <- scenario_df
    opp_rv$swapin_newly_eligible <- newly_eligible
    opp_rv$swapin_relaxed_params <- relaxed_params

    pipeline_step_done(session, "opp_swapin_pipeline", 4L,
      paste0(format(n_swapin, big.mark = ","), " newly eligible"))

    # Mark pipeline complete
    pipeline_complete(session, "opp_swapin_pipeline")

    # Build neighbor definition description — includes band range, threshold,
    # and newly-eligible count to connect neighbor metrics to swap-in analysis
    n_swapin_lbl <- format(n_swapin, big.mark = ",")
    band_desc <- if (reason == "01. Fico") {
      sprintf("Neighbor band: issued loans with FICO %d\u2013%d (decline cutoff = %d)",
              threshold, threshold + 40, threshold)
    } else if (reason == "02. Thin file") {
      sprintf("Neighbor band: issued loans with file age %d\u2013%d months (decline cutoff \u2264 %d mo)",
              threshold + 1, threshold + 18, threshold)
    } else if (reason == "03. Delinquency decline") {
      sprintf("Neighbor band: issued loans with exactly %d delinquent account(s) (decline cutoff > %d)",
              threshold, threshold)
    } else if (reason == "04. HighDTI") {
      sprintf("Neighbor band: issued loans with DTI %.0f%%\u2013%.0f%% (decline cutoff > %.0f%%)",
              (threshold - 0.10) * 100, threshold * 100, threshold * 100)
    } else if (reason == "05. HardInquiry") {
      eff_thr <- if (is.null(threshold) || is.na(threshold) || threshold >= 99) 4L else as.integer(threshold)
      sprintf("Nearest approved group: issued loans with exactly %d hard inquir%s (decline cutoff \u2265 %d)",
              eff_thr - 1L, if (eff_thr - 1L == 1L) "y" else "ies", eff_thr)
    } else { "" }
    opp_rv$swapin_band_desc <- band_desc
    opp_rv$swapin_neighbor_def <- paste0(
      band_desc,
      " | Relaxing by ", relax, " \u2192 ",
      n_swapin_lbl, " newly eligible applicants"
    )

    # Set default assumed delinquency rate.
    # Prefer waterfall Layer 1 (direct test/control) or Layer 2 (neighbor band)
    # when confidence is high/medium. Fall back to neighbor × 1.15 stress only
    # when no empirical layer is available.
    default_delinq <- if (!is.null(br_proj) &&
                         !is.null(br_proj$layer) &&
                         br_proj$layer %in% c(1L, 2L)) {
      round(br_proj$value * 100, 1)
    } else {
      round(neighbor$delinq_rate * 1.15 * 100, 1)
    }
    default_delinq <- max(default_delinq, 0.5)
    updateNumericInput(session, "opp_swapin_delinq_assumed",
      value = default_delinq)

    # Hide previous results, show neighbor panel
    hideElement("opp_swapin_results_panel")
    showElement("opp_swapin_neighbor_panel")
  })

  # ---- Swap-In Neighbor Band KPIs ----
  output$opp_swapin_kpi_n <- renderUI({
    nb <- opp_rv$swapin_neighbor
    req(nb)
    kpi_box(
      fmt_k(nb$n_loans),
      "Neighbor Loans",
      paste0(fmt_k(opp_rv$swapin_declined_n), " newly eligible")
    )
  })

  output$opp_swapin_kpi_irr <- renderUI({
    nb <- opp_rv$swapin_neighbor
    req(nb)
    irr_str <- if (!is.na(nb$observed_irr)) {
      paste0(round(nb$observed_irr * 100, 1), "%")
    } else "N/A"
    kpi_box(irr_str, "Neighbor IRR (Observed)")
  })

  output$opp_swapin_kpi_delinq <- renderUI({
    nb <- opp_rv$swapin_neighbor
    req(nb)
    br <- opp_rv$swapin_bad_rate_projection
    bps <- input$opp_swapin_pricing_bps %||% 0
    bps_suffix <- if (bps != 0) paste0(" at ", sprintf("%+d", bps), "bps") else ""
    if (!is.null(br) && !is.null(br$layer) && br$layer %in% c(1L, 2L)) {
      kpi_box(
        paste0(round(br$value * 100, 1), "%"),
        paste0("Projected Bad Rate", bps_suffix)
      )
    } else {
      kpi_box(
        paste0(round(nb$delinq_rate * 100, 1), "%"),
        "Neighbor Delinq Rate"
      )
    }
  })

  output$opp_swapin_kpi_elasticity <- renderUI({
    el <- opp_rv$swapin_elasticity_projection
    if (is.null(el) || is.null(el$value) || is.na(el$value)) {
      return(kpi_box("\u2014", "Take-Rate Elasticity"))
    }
    kpi_box(
      paste0(round(el$value, 2), " pp/100bps"),
      "Take-Rate Elasticity"
    )
  })

  # ------------------------------------------------------------------
  # Internal helpers for the waterfall-aware KPI boxes above.
  # ------------------------------------------------------------------
  .kpi_provenance_badge <- function(layer, n = NA, kind = "bad_rate",
                                      source = NULL, reason = NULL,
                                      band_desc = NULL) {
    n_fmt <- if (!is.null(n) && !is.na(n) && n > 0)
      format(n, big.mark = ",") else ""
    if (isTRUE(layer == 1L) ||
        identical(source, "direct_test_control") ||
        identical(source, "test_control")) {
      detail <- if (kind == "elasticity") {
        paste0("Measured take-rate sensitivity from PL-V4 Test A and Test B: ",
               n_fmt, " borrowers in this segment were randomly assigned ",
               "different rate levels")
      } else {
        paste0("Actual default rate of ", n_fmt,
               " loans issued under PL-V4 Test B, a controlled pricing ",
               "experiment that priced this borrower segment at a premium")
      }
      list(text = "OBSERVED \u2014 PRICING EXPERIMENT",
           bg   = "#15803d",  # green-700
           detail = detail)
    } else if (isTRUE(layer == 2L)) {
      band_part <- if (!is.null(band_desc) && nzchar(band_desc)) {
        desc <- gsub("^(Neighbor band|Nearest approved group): ", "", band_desc)
        gsub("^issued loans", "loans already on your book", desc)
      } else {
        "loans near the decline threshold"
      }
      list(text = "OBSERVED \u2014 SIMILAR BORROWERS",
           bg   = "#0369a1",  # sky-700
           detail = paste0("Actual default rate of ", n_fmt, " ", band_part,
                           " \u2014 the nearest approved group to the ",
                           "re-entry cohort"))
    } else if (isTRUE(layer == 3L)) {
      detail <- if (kind == "elasticity") {
        paste0("Price sensitivity estimated for borrowers in the same credit ",
               "grade from the PL-V4 pricing experiment \u2014 no direct test ",
               "exists for this specific variable")
      } else {
        paste0("Estimated from the average default rate for borrowers in the ",
               "same credit grade across the portfolio \u2014 no direct ",
               "observation available for this variable")
      }
      list(text = "ESTIMATED \u2014 CREDIT GRADE",
           bg   = "#ca8a04",  # yellow-600
           detail = detail)
    } else {
      list(text = "ASSUMPTION",
           bg   = "#6b7280",  # grey-500
           detail = paste0("Industry-standard assumption \u2014 no empirical ",
                           "data or experiment available for this segment"))
    }
  }

  .kpi_box_with_badge <- function(value, label, badge) {
    tags$div(
      style = paste0(
        "background:#1f2937; border-radius:8px; padding:10px 10px 12px;",
        " color:white; text-align:center; margin-bottom:10px; min-height:110px;",
        " position:relative;"),
      tags$div(
        style = paste0(
          "display:inline-block; background:", badge$bg, ";",
          " color:white; font-size:9.5px; font-weight:700;",
          " padding:3px 8px; border-radius:10px; margin-bottom:6px;",
          " letter-spacing:0.4px;"),
        badge$text),
      tags$div(style = "font-size:20px; font-weight:bold; color:#fb6e36;",
               value),
      tags$div(style = "font-size:10px; color:#9ca3af; margin-top:2px;",
               label),
      tags$div(style = "font-size:9.5px; color:#c7cbd1; margin-top:4px; line-height:1.2;",
               badge$detail)
    )
  }

  output$opp_swapin_kpi_apr <- renderUI({
    nb <- opp_rv$swapin_neighbor
    req(nb)
    kpi_box(
      paste0(round(nb$avg_apr * 100, 1), "%"),
      "Neighbor Avg APR"
    )
  })

  output$opp_swapin_kpi_mtd <- renderUI({
    nb <- opp_rv$swapin_neighbor
    req(nb)
    kpi_box(
      paste0(round(nb$avg_months_to_default, 1), " mo"),
      "Avg Months to Default"
    )
  })

  output$opp_swapin_neighbor_def <- renderUI({
    def <- opp_rv$swapin_neighbor_def
    req(def, nchar(def) > 0)
    tags$div(
      style = "background:#1e293b; border-left:3px solid #fb6e36; padding:8px 12px; border-radius:4px; color:#94a3b8; font-size:13px;",
      icon("info-circle", style = "color:#fb6e36; margin-right:6px;"),
      def
    )
  })

  # ---- Simulate Swap-In IRR ----
  observeEvent(input$opp_swapin_simulate, {
    nb <- opp_rv$swapin_neighbor
    req(nb)
    tape <- perf_tape()
    req(tape)

    # Require engine state from Analyze step
    baseline_df    <- opp_rv$swapin_baseline_df
    scenario_df    <- opp_rv$swapin_scenario_df
    newly_eligible <- opp_rv$swapin_newly_eligible
    req(baseline_df, scenario_df, newly_eligible)

    delinq_pct  <- input$opp_swapin_delinq_assumed %||% 5
    pricing_bps <- input$opp_swapin_pricing_bps %||% 0
    obs_months  <- input$opp_swapin_obs_months %||% 36
    delinq_rate <- delinq_pct / 100

    n_eligible <- sum(newly_eligible, na.rm = TRUE)
    if (n_eligible == 0) {
      showNotification("No newly eligible applicants from engine. Adjust the relaxation.",
        type = "warning", duration = 4)
      return()
    }

    withProgress(message = "Simulating swap-in IRR...", value = 0.2, {

      # ---- Grade-stratified take-up simulation (What-If pattern) ----
      # Compute grade-level historical take rates from baseline approved population
      bl_appr_mask <- !is.na(baseline_df$offered_decision_flag) &
        baseline_df$offered_decision_flag == "APPR"
      grade_take <- tapply(
        baseline_df$issued_flag[bl_appr_mask] == 1L,
        baseline_df$risk_grade[bl_appr_mask],
        mean, na.rm = TRUE
      )
      overall_take <- mean(baseline_df$issued_flag[bl_appr_mask] == 1L, na.rm = TRUE)

      # Grade-level delinquency rates among baseline issued
      bl_issued_mask <- bl_appr_mask & baseline_df$issued_flag == 1L
      grade_del <- tapply(
        baseline_df$performance_delinquency_flag[bl_issued_mask],
        baseline_df$risk_grade[bl_issued_mask],
        mean, na.rm = TRUE
      )

      # Get newly eligible cohort from engine results
      # Use which() — logical indexing with NAs produces spurious NA rows
      new_idx    <- which(newly_eligible)
      cohort_df  <- scenario_df[new_idx, , drop = FALSE]

      # Allocate take-up by grade (best FICO first per grade)
      new_grades <- scenario_df$risk_grade[new_idx]
      taken_mask <- rep(FALSE, length(new_idx))

      for (g in unique(new_grades[!is.na(new_grades)])) {
        g_pos  <- which(new_grades == g & !is.na(new_grades))
        g_rate <- if (g %in% names(grade_take)) grade_take[[g]] else overall_take
        n_take <- round(length(g_pos) * g_rate)
        if (n_take > 0) {
          fico_ord <- order(scenario_df$fico_score[new_idx[g_pos]], decreasing = TRUE)
          take_pos <- g_pos[fico_ord[seq_len(min(n_take, length(g_pos)))]]
          taken_mask[take_pos] <- TRUE
        }
      }

      taken_df <- cohort_df[taken_mask[seq_len(nrow(cohort_df))], , drop = FALSE]
      n_loans  <- nrow(taken_df)

      incProgress(0.3, detail = "Building synthetic cashflows...")

      if (n_loans == 0) {
        showNotification("No loans expected to convert based on historical take rates.",
          type = "warning", duration = 4)
        return()
      }

      # Use engine-assigned values (not neighbor averages)
      avg_amount <- mean(taken_df$maximum_offered_amount, na.rm = TRUE)
      avg_apr    <- mean(taken_df$offered_final_apr, na.rm = TRUE) + pricing_bps / 10000
      avg_term   <- round(mean(taken_df$offered_term_months, na.rm = TRUE))

      # Build synthetic cashflows
      cfs <- build_synthetic_cashflows(
        n_loans               = n_loans,
        avg_amount            = avg_amount,
        apr                   = avg_apr,
        term                  = avg_term,
        delinq_rate           = delinq_rate,
        avg_months_to_default = nb$avg_months_to_default,
        observation_months    = obs_months,
        lgd                   = nb$avg_lgd
      )

      incProgress(0.3, detail = "Computing IRR...")

      projected_irr <- tryCatch(
        xirr(cfs$cashflow, cfs$date),
        error = function(e) NA_real_
      )

      # Compute blended portfolio IRR (volume-weighted approximation)
      existing_irr <- compute_portfolio_xirr_v2(tape)
      existing_volume <- sum(
        credit_data$issued_amount[credit_data$issued_flag == 1],
        na.rm = TRUE)
      new_volume <- n_loans * avg_amount

      blended_irr <- if (!is.na(existing_irr) && !is.na(projected_irr)) {
        w <- existing_volume / (existing_volume + new_volume)
        existing_irr * w + projected_irr * (1 - w)
      } else {
        NA_real_
      }

      # Grade distribution of taken loans
      grade_dist <- if (n_loans > 0) table(taken_df$risk_grade) else table(character(0))

      opp_rv$swapin_cohort <- list(
        n_loans      = n_loans,
        n_eligible   = n_eligible,
        avg_amount   = avg_amount,
        avg_apr      = avg_apr,
        avg_term     = avg_term,
        total_volume = new_volume,
        grade_dist   = grade_dist
      )
      opp_rv$swapin_projected_cf  <- cfs
      opp_rv$swapin_projected_irr <- projected_irr

      incProgress(0.2, detail = "Done.")
    })

    showElement("opp_swapin_results_panel")
  })

  # ---- Swap-In Simulation Result KPIs ----
  output$opp_swapin_res_volume <- renderUI({
    cohort <- opp_rv$swapin_cohort
    req(cohort)
    n_eligible <- cohort$n_eligible %||% cohort$n_loans
    vol_sub <- paste0(fmt_k(n_eligible), " eligible, ",
      fmt_k(cohort$n_loans), " expected to convert, ",
      fmt_dollar_k(cohort$total_volume), " volume")
    kpi_box(
      fmt_k(cohort$n_loans),
      "Expected Take-Up",
      vol_sub
    )
  })

  output$opp_swapin_res_delinq <- renderUI({
    delinq <- input$opp_swapin_delinq_assumed %||% 5
    kpi_box(
      paste0(delinq, "%"),
      "Assumed Delinq Rate"
    )
  })

  output$opp_swapin_res_apr <- renderUI({
    cohort <- opp_rv$swapin_cohort
    req(cohort)
    kpi_box(
      paste0(round(cohort$avg_apr * 100, 1), "%"),
      "Cohort Avg APR",
      if ((input$opp_swapin_pricing_bps %||% 0) != 0)
        paste0("(incl. ", input$opp_swapin_pricing_bps, " BPS adjustment)")
    )
  })

  output$opp_swapin_res_irr <- renderUI({
    irr <- opp_rv$swapin_projected_irr
    req(!is.null(irr))
    irr_str <- if (!is.na(irr)) paste0(round(irr * 100, 1), "%") else "N/A"
    cohort <- opp_rv$swapin_cohort
    sub_str <- if (!is.na(irr) && !is.null(cohort)) {
      paste0("Based on ", cohort$n_loans, " loans @ ",
             round(cohort$avg_apr * 100, 1), "% APR")
    } else NULL
    kpi_box(irr_str, "Projected Segment IRR", sub_str)
  })

  output$opp_swapin_res_blended <- renderUI({
    tryCatch({
      irr <- opp_rv$swapin_projected_irr
      req(!is.null(irr))
      tape <- tryCatch(perf_tape(), error = function(e) {
        message("[OppExplorer] opp_swapin_res_blended perf_tape: ", e$message)
        NULL
      })
      req(tape)

      existing_irr <- tryCatch(compute_portfolio_xirr_v2(tape), error = function(e) {
        message("[OppExplorer] opp_swapin_res_blended compute_portfolio_xirr_v2: ", e$message)
        NA_real_
      })
      existing_volume <- sum(
        credit_data$issued_amount[credit_data$issued_flag == 1], na.rm = TRUE)
      new_volume <- opp_rv$swapin_cohort$total_volume

      if (!is.na(existing_irr) && !is.na(irr)) {
        w <- existing_volume / (existing_volume + new_volume)
        blended <- existing_irr * w + irr * (1 - w)
        blended_str <- paste0(round(blended * 100, 1), "%")
        delta <- blended - existing_irr
        delta_str <- paste0(ifelse(delta >= 0, "+", ""),
          round(delta * 10000), " bps vs current ",
          round(existing_irr * 100, 1), "%")
      } else {
        blended_str <- "N/A"
        delta_str <- NULL
      }
      kpi_box(blended_str, "Approx. Blended Portfolio IRR", delta_str)
    }, error = function(e) {
      message("[OppExplorer] opp_swapin_res_blended: ", e$message)
      kpi_box("N/A", "Approx. Blended Portfolio IRR",
              paste("Error:", e$message))
    })
  })

  # ---- Swap-In Cashflow Plot ----
  output$opp_swapin_cf_plot <- renderPlotly({
    cfs <- opp_rv$swapin_projected_cf
    req(cfs, nrow(cfs) > 1)

    # Skip month 0 (disbursement) for readability
    plot_data <- cfs[-1, ]
    plot_data$month <- seq_len(nrow(plot_data))

    plot_ly(plot_data, x = ~month) %>%
      add_trace(y = ~performing_cf, name = "Performing CF",
        type = "scatter", mode = "lines",
        line = list(color = "#16a34a", width = 2)) %>%
      add_trace(y = ~loss_cf, name = "Loss CF",
        type = "scatter", mode = "lines",
        line = list(color = "#dc2626", width = 2)) %>%
      add_trace(y = ~cashflow, name = "Net CF",
        type = "scatter", mode = "lines",
        line = list(color = "#fb6e36", width = 3)) %>%
      layout(
        xaxis = list(title = "Month", gridcolor = "#374151"),
        yaxis = list(title = "Cashflow ($)", gridcolor = "#374151",
          tickformat = "$,.0f"),
        paper_bgcolor = "#1f2937",
        plot_bgcolor  = "#1f2937",
        font = list(color = "#e5e7eb"),
        legend = list(orientation = "h", y = -0.15),
        margin = list(t = 10)
      )
  })

  # ---- Swap-In Summary Table ----
  output$opp_swapin_summary_table <- renderDT({
    cohort <- opp_rv$swapin_cohort
    nb     <- opp_rv$swapin_neighbor
    irr    <- opp_rv$swapin_projected_irr
    req(cohort, nb)

    df <- data.frame(
      Metric = c("Swap-In Loans", "Avg Loan Amount", "Avg APR",
                  "Avg Term (months)", "Assumed Delinq Rate",
                  "Projected IRR", "Neighbor Observed IRR",
                  "Neighbor Delinq Rate"),
      `Swap-In Cohort` = c(
        format(cohort$n_loans, big.mark = ","),
        paste0("$", format(round(cohort$avg_amount), big.mark = ",")),
        paste0(round(cohort$avg_apr * 100, 1), "%"),
        as.character(cohort$avg_term),
        paste0(round(input$opp_swapin_delinq_assumed, 1), "%"),
        if (!is.na(irr)) paste0(round(irr * 100, 1), "%") else "N/A",
        if (!is.na(nb$observed_irr))
          paste0(round(nb$observed_irr * 100, 1), "%") else "N/A",
        paste0(round(nb$delinq_rate * 100, 1), "%")
      ),
      check.names = FALSE, stringsAsFactors = FALSE
    )

    datatable(df, rownames = FALSE, selection = "none",
      options = list(dom = "t", pageLength = 10,
        initComplete = JS(
          "function(s,j){",
          "$(this.api().table().header()).css(",
          "  {'background-color':'#fb6e36','color':'white'});",
          "}"
        )
      )
    )
  })

  # ---- Analysis-section filter reactive (reads opp_rv, applies 5 filters) ----
  opp_perf_data <- reactive({
    req(opp_rv$df_full)
    df_f <- opp_rv$df_full
    df_p <- opp_rv$df_proposed
    seg  <- opp_rv$in_segment

    if (!("all" %in% input$opp_perf_fico) && length(input$opp_perf_fico) > 0) {
      keep <- df_f$fico_band %in% input$opp_perf_fico
      df_f <- df_f[keep, ]; df_p <- df_p[keep, ]; seg <- seg[keep]
    }
    if (!("all" %in% input$opp_perf_partner) && length(input$opp_perf_partner) > 0) {
      keep <- df_f$partner_channel %in% input$opp_perf_partner
      df_f <- df_f[keep, ]; df_p <- df_p[keep, ]; seg <- seg[keep]
    }
    if (!("all" %in% input$opp_perf_repeat) && length(input$opp_perf_repeat) > 0) {
      keep <- df_f$borrower_repeat_type %in% input$opp_perf_repeat
      df_f <- df_f[keep, ]; df_p <- df_p[keep, ]; seg <- seg[keep]
    }
    if (!("all" %in% input$opp_perf_mob) && length(input$opp_perf_mob) > 0) {
      keep <- df_f$mob_bins %in% input$opp_perf_mob
      df_f <- df_f[keep, ]; df_p <- df_p[keep, ]; seg <- seg[keep]
    }
    if (!("all" %in% input$opp_perf_vintage) && length(input$opp_perf_vintage) > 0) {
      v    <- format(df_f$date, "%Y-%m")
      keep <- !is.na(v) & v %in% input$opp_perf_vintage
      df_f <- df_f[keep, ]; df_p <- df_p[keep, ]; seg <- seg[keep]
    }
    list(df_full = df_f, df_proposed = df_p, in_segment = seg)
  })

  # ---- KPI panel: dual rows when both scenarios exist ----
  build_kpi_row <- function(sim, label_prefix = "") {
    df_f <- sim$df_full
    df_p <- sim$df_proposed
    seg  <- sim$in_segment
    action <- sim$action
    bl <- compute_funnel(df_f)
    pr <- compute_funnel(df_p)
    seg_issued <- sum(seg & df_f$issued_flag == 1, na.rm = TRUE)
    # Count approved OR historically-issued (consistent with decline logic
    # which targets both, since historical issued_flag is ground truth)
    seg_appr   <- sum(seg & (df_f$offered_decision_flag == "APPR" |
                             df_f$issued_flag == 1), na.rm = TRUE)
    sa  <- seg_appr
    bsi <- seg_issued
    psi <- sum(seg & df_p$issued_flag == 1, na.rm = TRUE)
    bl_conv <- if (sa > 0) bsi / sa else NA_real_
    pr_conv <- if (sa > 0) psi / sa else NA_real_
    ba  <- mean(df_f$interest_rate_apr[seg & df_f$issued_flag == 1], na.rm = TRUE)
    pa  <- mean(df_p$interest_rate_apr[seg & df_p$issued_flag == 1], na.rm = TRUE)
    # Segment-level bad rates (only loans IN the CART segment)
    seg_bl_br_raw <- mean(df_f$performance_delinquency_flag[seg & df_f$issued_flag == 1], na.rm = TRUE)
    seg_pr_br_raw <- mean(df_p$performance_delinquency_flag[seg & df_p$issued_flag == 1], na.rm = TRUE)
    seg_bl_br <- ifelse(is.na(seg_bl_br_raw), 0, seg_bl_br_raw * 100)
    seg_pr_br <- ifelse(is.na(seg_pr_br_raw), 0, seg_pr_br_raw * 100)
    # Portfolio-level bad rates (all issued loans)
    bl_br <- ifelse(is.na(bl$bad_rate), 0, bl$bad_rate * 100)
    pr_br <- ifelse(is.na(pr$bad_rate), 0, pr$bad_rate * 100)

    kpi1 <- kpi_box(fmt_k(seg_issued), "Issued (Segment)",
      sub = paste0(fmt_k(seg_appr), " approved"))

    if (action == "Decline") {
      kpi2 <- kpi_box(sprintf("%.1f%% \u2192 %.1f%%", bl$appr_rate * 100, pr$appr_rate * 100),
        "Approval Rate")
      delta_appr <- (pr$appr_rate - bl$appr_rate) * 100
      kpi3 <- kpi_box(sprintf("%+.1f pp", delta_appr), "Approval Rate Delta",
        sub = if (delta_appr < 0) "Tighter risk filter" else "Expanded approvals")
    } else {
      kpi2 <- kpi_box(sprintf("%.1f%% \u2192 %.1f%%",
        ifelse(is.na(bl_conv), 0, bl_conv * 100), ifelse(is.na(pr_conv), 0, pr_conv * 100)),
        "Take Rate")
      delta_conv <- (pr_conv - bl_conv) * 100
      kpi3 <- kpi_box(sprintf("%+.1f pp", ifelse(is.na(delta_conv), 0, delta_conv)),
        "Take Rate Delta", sub = sprintf("%s \u2192 %s issued", fmt_k(bsi), fmt_k(psi)))
    }
    kpi4 <- kpi_box(sprintf("%.1f%% \u2192 %.1f%%", seg_bl_br, seg_pr_br),
      "Segment 2+ Rate")
    kpi5 <- kpi_box(sprintf("%.1f%% \u2192 %.1f%%",
      ifelse(is.na(ba), 0, ba * 100), ifelse(is.na(pa), 0, pa * 100)),
      "Avg APR (Segment)")
    # Compute projected portfolio bad rate for supplementary display
    tape <- tryCatch(perf_tape(), error = function(e) {
      message("[OppExplorer] build_kpi_row perf_tape: ", e$message)
      NULL
    })
    proj_sub_label <- tryCatch({
      if (!is.null(tape)) {
        bl_ids <- as.character(df_f$loan_id[df_f$issued_flag == 1])
        bl_proj_res <- compute_projected_bad_rate(bl_ids, tape, df_f)
        pr_ids <- as.character(df_p$loan_id[df_p$issued_flag == 1])
        pr_proj_res <- compute_projected_bad_rate(pr_ids, tape, df_p)
        bl_p <- if (nrow(bl_proj_res) > 0) mean(bl_proj_res$projected_lifetime_prob, na.rm = TRUE) else NA_real_
        pr_p <- if (nrow(pr_proj_res) > 0) mean(pr_proj_res$projected_lifetime_prob, na.rm = TRUE) else NA_real_
        if (!is.na(bl_p) && abs(bl_p - bl$bad_rate) > 0.001)
          paste0(if (pr_br < bl_br) "Improved" else if (pr_br > bl_br) "Worsened" else "Unchanged",
                 " | projected: ", sprintf("%.1f%%", bl_p * 100),
                 " \u2192 ", sprintf("%.1f%%", pr_p * 100))
        else if (pr_br < bl_br) "Improved" else if (pr_br > bl_br) "Worsened" else "Unchanged"
      } else {
        if (pr_br < bl_br) "Improved" else if (pr_br > bl_br) "Worsened" else "Unchanged"
      }
    }, error = function(e) {
      message("[OppExplorer] build_kpi_row compute_projected_bad_rate: ", e$message)
      if (pr_br < bl_br) "Improved" else if (pr_br > bl_br) "Worsened" else "Unchanged"
    })
    kpi6 <- kpi_box(sprintf("%.1f%% \u2192 %.1f%%", bl_br, pr_br),
      "Portfolio 2+ Rate", sub = proj_sub_label)

    tags$div(
      if (nchar(label_prefix) > 0) {
        tags$div(style = "padding:0 15px; margin-bottom:4px;",
          tags$strong(label_prefix, style = "font-size:14px; color:#f59e0b;"))
      },
      tags$div(style = "display:flex; gap:10px; padding:0 15px; margin-bottom:10px;",
        tags$div(style = "flex:1; min-width:0;", kpi1),
        tags$div(style = "flex:1; min-width:0;", kpi2),
        tags$div(style = "flex:1; min-width:0;", kpi3),
        tags$div(style = "flex:1; min-width:0;", kpi4),
        tags$div(style = "flex:1; min-width:0;", kpi5),
        tags$div(style = "flex:1; min-width:0;", kpi6)
      )
    )
  }

  output$opp_kpi_panel <- renderUI({
    simA <- opp_rv$sim_A
    simB <- opp_rv$sim_B
    if (!is.null(simA) && !is.null(simB)) {
      tagList(
        build_kpi_row(simA, simA$label %||% "Primary Action"),
        build_kpi_row(simB, simB$label %||% "Alternative Action")
      )
    } else if (!is.null(simA)) {
      build_kpi_row(simA)
    } else if (!is.null(simB)) {
      build_kpi_row(simB)
    } else {
      # fallback: show KPIs from current opp_rv state
      req(opp_rv$df_full)
      d <- opp_perf_data()
      build_kpi_row(list(df_full = d$df_full, df_proposed = d$df_proposed,
        in_segment = d$in_segment, action = opp_rv$action))
    }
  })

  # ---- Funnel filter subtitle ----
  output$opp_funnel_subtitle <- renderUI({
    req(opp_rv$df_full)
    parts <- c()
    if (!("all" %in% input$opp_perf_fico) && length(input$opp_perf_fico) > 0)
      parts <- c(parts, paste0("FICO: ", paste(input$opp_perf_fico, collapse = ", ")))
    if (!("all" %in% input$opp_perf_partner) && length(input$opp_perf_partner) > 0)
      parts <- c(parts, paste0("Partner: ", paste(input$opp_perf_partner, collapse = ", ")))
    if (!("all" %in% input$opp_perf_repeat) && length(input$opp_perf_repeat) > 0)
      parts <- c(parts, paste0("Repeat: ", paste(input$opp_perf_repeat, collapse = ", ")))
    if (!("all" %in% input$opp_perf_mob) && length(input$opp_perf_mob) > 0)
      parts <- c(parts, paste0("MOB: ", paste(input$opp_perf_mob, collapse = ", ")))
    if (!("all" %in% input$opp_perf_vintage) && length(input$opp_perf_vintage) > 0)
      parts <- c(parts, paste0("Vintage: ", paste(input$opp_perf_vintage, collapse = ", ")))
    if (length(parts) == 0) return(NULL)
    tags$p(style = "color:#9ca3af; font-size:11px; margin:0 0 8px 0;",
      icon("filter"), " Filtered: ", paste(parts, collapse = " | "))
  })

  # ---- Funnel metrics table (supports dual simulation comparison) ----
  output$opp_funnel_table <- renderUI({
    req(opp_rv$df_full)
    d  <- opp_perf_data()
    bl <- compute_funnel(d$df_full)
    pr <- compute_funnel(d$df_proposed)

    fp  <- function(x) if (is.na(x)) "\u2014" else sprintf("%.1f%%", x * 100)
    fc  <- function(x) if (is.na(x)) "\u2014" else fmt_k(x)
    fdpct <- function(b, p) {
      if (is.na(b) || is.na(p) || b == 0) return("\u2014")
      sprintf("%+.1f%%", (p - b) / b * 100)
    }
    fdp <- function(b, p) {
      if (is.na(b) || is.na(p)) return("\u2014")
      sprintf("%+.1f pp", (p - b) * 100)
    }

    # Compute portfolio XIRR if tape is available
    tape <- tryCatch(perf_tape(), error = function(e) {
      message("[OppExplorer] opp_funnel_table perf_tape: ", e$message)
      NULL
    })
    compute_irr_pair <- function(bl_fun, pr_fun, bps_val, seg_mask) {
      if (!is.null(tape)) {
        # Use compute_adjusted_xirr_v2 for per-loan APR-delta accuracy
        # (accounts for 72-month cap on repriced loans)
        tryCatch({
          bl_xirr <- compute_adjusted_xirr_v2(bl_fun, tape, bl_fun)
          pr_xirr <- compute_adjusted_xirr_v2(pr_fun, tape, bl_fun)
          list(bl = bl_xirr, pr = pr_xirr, label = "Portfolio IRR")
        }, error = function(e) {
          message("[OppExplorer] opp_funnel_table compute_adjusted_xirr_v2: ", e$message)
          bl_f <- compute_funnel(bl_fun)
          pr_f <- compute_funnel(pr_fun)
          list(bl = bl_f$avg_apr, pr = pr_f$avg_apr, label = "Avg IRR (APR)")
        })
      } else {
        bl_f <- compute_funnel(bl_fun)
        pr_f <- compute_funnel(pr_fun)
        list(bl = bl_f$avg_apr, pr = pr_f$avg_apr, label = "Avg IRR (APR)")
      }
    }

    irr <- compute_irr_pair(d$df_full, d$df_proposed, opp_rv$bps, d$in_segment)

    # Compute projected bad rates for funnel table (if tape available)
    compute_funnel_proj <- function(df_src) {
      if (is.null(tape)) return(NA_real_)
      tryCatch({
        ids <- as.character(df_src$loan_id[df_src$issued_flag == 1])
        if (length(ids) == 0) return(NA_real_)
        proj <- compute_projected_bad_rate(ids, tape, df_src)
        if (!is.null(proj) && nrow(proj) > 0)
          mean(proj$projected_lifetime_prob, na.rm = TRUE)
        else NA_real_
      }, error = function(e) {
        message("[OppExplorer] opp_funnel_table compute_projected_bad_rate: ", e$message)
        NA_real_
      })
    }

    # Check if both simulation slots are populated
    sim_A <- opp_rv$sim_A
    sim_B <- opp_rv$sim_B
    dual <- !is.null(sim_A) && !is.null(sim_B)

    if (dual) {
      # Dual mode: compute funnel for both scenarios
      pr_A <- compute_funnel(sim_A$df_proposed)
      pr_B <- compute_funnel(sim_B$df_proposed)
      bl_A <- compute_funnel(sim_A$df_full)
      irr_A <- compute_irr_pair(sim_A$df_full, sim_A$df_proposed,
                                 sim_A$bps, sim_A$in_segment)
      irr_B <- compute_irr_pair(sim_B$df_full, sim_B$df_proposed,
                                 sim_B$bps, sim_B$in_segment)

      rows <- list(
        c("Applications",  fc(bl_A$apps),      fc(pr_A$apps),      fc(pr_B$apps)),
        c("Approved",      fc(bl_A$approved),  fc(pr_A$approved),  fc(pr_B$approved)),
        c("Issued",        fc(bl_A$issued),    fc(pr_A$issued),    fc(pr_B$issued)),
        c("Approval Rate", fp(bl_A$appr_rate), fp(pr_A$appr_rate), fp(pr_B$appr_rate)),
        c("Take Rate",     fp(bl_A$conv_rate), fp(pr_A$conv_rate), fp(pr_B$conv_rate)),
        c("2+ Rate",      fp(bl_A$bad_rate),  fp(pr_A$bad_rate),  fp(pr_B$bad_rate))
      )
      bl_proj <- compute_funnel_proj(sim_A$df_full)
      prA_proj <- compute_funnel_proj(sim_A$df_proposed)
      prB_proj <- compute_funnel_proj(sim_B$df_proposed)
      if (!is.na(bl_proj))
        rows <- c(rows, list(c("2+ Rate (Projected)", fp(bl_proj), fp(prA_proj), fp(prB_proj))))
      rows <- c(rows, list(
        c("Avg APR",       fp(bl_A$avg_apr),   fp(pr_A$avg_apr),   fp(pr_B$avg_apr)),
        c(irr_A$label,     fp(irr_A$bl),       fp(irr_A$pr),       fp(irr_B$pr))
      ))
      tbl <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
      names(tbl) <- c("Metric", "Baseline", "Primary", "Alternative")
    } else {
      # Single scenario mode (original)
      rows <- list(
        c("Applications",    fc(bl$apps),      fc(pr$apps)),
        c("Approved",        fc(bl$approved),  fc(pr$approved)),
        c("Issued",          fc(bl$issued),    fc(pr$issued)),
        c("Approval Rate",   fp(bl$appr_rate), fp(pr$appr_rate)),
        c("Take Rate",       fp(bl$conv_rate), fp(pr$conv_rate)),
        c("2+ Rate",        fp(bl$bad_rate),  fp(pr$bad_rate))
      )
      bl_proj <- compute_funnel_proj(d$df_full)
      pr_proj <- compute_funnel_proj(d$df_proposed)
      if (!is.na(bl_proj))
        rows <- c(rows, list(c("2+ Rate (Projected)", fp(bl_proj), fp(pr_proj))))
      rows <- c(rows, list(
        c("Avg APR",         fp(bl$avg_apr),   fp(pr$avg_apr)),
        c(irr$label,         fp(irr$bl),       fp(irr$pr))
      ))
      tbl <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
      slot_label <- if (!is.null(opp_rv$sim_A)) "Primary" else
                    if (!is.null(opp_rv$sim_B)) "Alternative" else "Scenario"
      names(tbl) <- c("Metric", "Baseline", slot_label)
    }

    header_cells <- lapply(names(tbl), function(col) {
      tags$th(
        style = "background:#fb6e36; color:white; padding:10px 12px; font-size:12px; line-height:1.2; text-align:left; white-space:normal;",
        col
      )
    })
    body_rows <- lapply(seq_len(nrow(tbl)), function(i) {
      tags$tr(
        lapply(seq_along(tbl), function(j) {
          tags$td(
            style = paste0(
              "padding:10px 12px; font-size:12px; border-top:1px solid #e5e7eb;",
              if (j == 1) " font-weight:600; color:#0f172a;" else " color:#334155;"
            ),
            tbl[i, j]
          )
        })
      )
    })

    # Hide overlay and scroll to funnel now that metrics are ready
    shinyjs::hide("opp_action_overlay")
    shinyjs::runjs("setTimeout(function(){
      var el = document.getElementById('opp_perf_show');
      if(el) el.scrollIntoView({behavior:'smooth', block:'start'});
    }, 300);")

    tags$div(style = "overflow-x:auto;",
      tags$table(
        style = "width:100%; border-collapse:collapse; table-layout:fixed;",
        tags$thead(tags$tr(header_cells)),
        tags$tbody(body_rows)
      )
    )
  })

  # ---- Funnel chart ----
  output$opp_funnel_plot <- renderPlotly({
    req(opp_rv$df_full)
    d  <- opp_perf_data()
    bl <- compute_funnel(d$df_full)
    pr <- compute_funnel(d$df_proposed)

    funnel_df <- data.frame(
      Stage    = rep(c("Applications", "Approved", "Issued"), 2),
      Scenario = rep(c("Baseline", "Simulated"), each = 3),
      Value    = c(bl$apps, bl$approved, bl$issued,
                   pr$apps, pr$approved, pr$issued),
      stringsAsFactors = FALSE
    )
    funnel_df$Stage <- factor(funnel_df$Stage,
                              levels = c("Applications", "Approved", "Issued"))

    plot_ly(funnel_df,
            x = ~Stage, y = ~Value, color = ~Scenario,
            colors = c("Baseline" = "#6c757d", "Simulated" = "#fb6e36"),
            type = "bar") %>%
      layout(
        barmode = "group",
        title  = list(text = "Application Funnel: Baseline vs Simulated",
                      font = list(size = 14)),
        xaxis  = list(title = ""),
        yaxis  = list(title = "Count"),
        paper_bgcolor = "white", plot_bgcolor = "white",
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2)
      )
  })

  # ---- Segment distribution selector UI ----
  output$opp_seg_selector_ui <- renderUI({
    req(opp_rv$seg_metrics)
    mode <- opp_rv$strategy_mode %||% "swapout_delinq"
    if (mode == "swapout_irr") {
      m <- opp_rv$seg_metrics %>% filter(mean_irr < opp_rv$overall_irr)
    } else {
      m <- opp_rv$seg_metrics %>% filter(risk_class != "Low Risk", lift >= 1)
    }
    choices <- setNames(
      as.character(m$node),
      paste0(as.character(m$risk_class), ": ",
             substr(m$condition, 1, 40), "\u2026")
    )
    selectInput("opp_seg_focus", "Highlight Segment:",
                choices = choices, selected = choices[1])
  })

  # ---- Segment distribution chart ----
  output$opp_seg_dist_plot <- renderPlotly({
    req(opp_rv$seg_metrics)
    mode <- opp_rv$strategy_mode %||% "swapout_delinq"
    m <- opp_rv$seg_metrics
    if (mode == "swapout_irr") {
      m <- m %>% filter(mean_irr < opp_rv$overall_irr)
    } else {
      m <- m %>% filter(risk_class != "Low Risk", lift >= 1)
    }
    validate(need(nrow(m) > 0, "No segment distribution data available."))

    baseline_count <- if ("apps_full" %in% names(m)) m$apps_full else
      if ("applications" %in% names(m)) m$applications else rep(NA_real_, nrow(m))
    issued_count <- if ("issued_full" %in% names(m)) m$issued_full else rep(NA_real_, nrow(m))

    seg_df <- data.frame(
      Segment = paste0("Segment ", seq_len(nrow(m))),
      Applications = baseline_count,
      Issued = issued_count,
      stringsAsFactors = FALSE
    )

    seg_long <- tidyr::pivot_longer(seg_df,
      cols = intersect(c("Applications", "Issued"), names(seg_df)),
      names_to = "Metric", values_to = "Value")

    plot_ly(seg_long,
            x = ~Segment, y = ~Value, color = ~Metric,
            colors = c("Applications" = "#cbd5e1", "Issued" = "#fb6e36"),
            type = "bar",
            hovertemplate = paste0("<b>%{x}</b><br>%{y:,}<extra>%{fullData.name}</extra>")) %>%
      layout(
        barmode = "group",
        title  = list(text = "Segment Distribution", font = list(size = 13)),
        xaxis  = list(title = "", tickangle = -20),
        yaxis  = list(title = "Count"),
        paper_bgcolor = "white", plot_bgcolor = "white",
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.3)
      )
  })

  # ---- Vintage layout: full-width primary when no secondary action ----
  output$opp_vintage_layout <- renderUI({
    has_secondary <- !is.null(opp_rv$sim_B)
    if (has_secondary) {
      fluidRow(
        column(6, plotlyOutput("opp_vintage_plot_a", height = "420px")),
        column(6, plotlyOutput("opp_vintage_plot_b", height = "420px"))
      )
    } else {
      fluidRow(
        column(12, plotlyOutput("opp_vintage_plot_a", height = "420px"))
      )
    }
  })

  # ---- Vintage performance curves (split into two charts) ----

  # Shared helper: build vintage curves for a set of loan IDs from tape
  # adverse_ids: loan IDs flipped by adverse selection (synthetic delinquency)
  .opp_build_vintage <- function(tape, ids, sel_vqs, scenario_label,
                                 adverse_ids = character(0)) {
    rows_sub <- tape$rows[tape$rows$loan_id %in% ids &
                          tape$rows$vintage_quarter %in% sel_vqs, ]
    if (nrow(rows_sub) == 0) return(data.frame())

    min_dq_mob <- 4L
    cohort_size <- rows_sub %>%
      filter(month_on_books == 0) %>%
      group_by(vintage_quarter) %>%
      summarise(n_loans = n_distinct(loan_id), .groups = "drop")
    first_dq <- rows_sub %>%
      filter(co_amount > 0) %>%
      group_by(loan_id, vintage_quarter) %>%
      summarise(first_dq_mob = min(month_on_books), .groups = "drop")
    flagged_no_co <- rows_sub %>%
      filter(performance_delinquency_flag == 1) %>%
      distinct(loan_id, vintage_quarter) %>%
      anti_join(first_dq, by = "loan_id") %>%
      mutate(first_dq_mob = min_dq_mob)
    first_dq <- bind_rows(first_dq, flagged_no_co) %>%
      mutate(first_dq_mob = pmax(first_dq_mob, min_dq_mob))

    # Inject synthetic delinquency for adversely-selected loans
    if (length(adverse_ids) > 0) {
      # Median first-dq MOB from actual delinquencies (realistic timing)
      syn_mob <- if (nrow(first_dq) > 0) median(first_dq$first_dq_mob) else min_dq_mob
      # Only add loans not already delinquent
      adv_in_tape <- rows_sub %>%
        filter(loan_id %in% adverse_ids) %>%
        distinct(loan_id, vintage_quarter) %>%
        anti_join(first_dq, by = "loan_id")
      if (nrow(adv_in_tape) > 0) {
        adv_dq <- adv_in_tape %>%
          mutate(first_dq_mob = as.integer(round(syn_mob)))
        first_dq <- bind_rows(first_dq, adv_dq)
      }
    }
    max_mob_v <- rows_sub %>%
      group_by(vintage_quarter) %>%
      summarise(max_mob = max(month_on_books), .groups = "drop")
    grid <- max_mob_v %>%
      rowwise() %>%
      do({ data.frame(vintage_quarter = .$vintage_quarter,
                      month_on_books = 0:.$max_mob, stringsAsFactors = FALSE) }) %>%
      ungroup() %>%
      left_join(cohort_size, by = "vintage_quarter")
    dq_counts <- first_dq %>%
      group_by(vintage_quarter) %>%
      arrange(first_dq_mob) %>%
      mutate(cum_bad = row_number()) %>%
      group_by(vintage_quarter, first_dq_mob) %>%
      summarise(cum_bad = max(cum_bad), .groups = "drop")
    grid %>%
      left_join(dq_counts %>% rename(month_on_books = first_dq_mob),
                by = c("vintage_quarter", "month_on_books")) %>%
      arrange(vintage_quarter, month_on_books) %>%
      group_by(vintage_quarter) %>%
      tidyr::fill(cum_bad, .direction = "down") %>%
      mutate(cum_bad = ifelse(is.na(cum_bad), 0, cum_bad),
             bad_pct = cum_bad / n_loans,
             Scenario = scenario_label) %>%
      ungroup()
  }

  # Shared helper: render a vintage chart from baseline + scenario data
  .opp_vintage_rank <- function(vq) {
    year_num <- suppressWarnings(as.integer(sub("^(\\d{4}).*$", "\\1", vq)))
    qtr_num <- suppressWarnings(as.integer(sub("^\\d{4}Q([1-4])$", "\\1", vq)))
    ifelse(is.na(year_num) | is.na(qtr_num), NA_integer_, year_num * 4L + qtr_num)
  }

  .opp_pretty_vintage <- function(vq, recent_vintages = character(0)) {
    year_num <- suppressWarnings(as.integer(substr(vq, 1, 4)))
    is_recent <- vq %in% recent_vintages
    ifelse(!is_recent & !is.na(year_num) & grepl("Q[12]$", vq), sub("Q[12]$", "-H1", vq),
           ifelse(!is_recent & !is.na(year_num) & grepl("Q[34]$", vq), sub("Q[34]$", "-H2", vq), vq))
  }

  .opp_aggregate_vintage_display <- function(df) {
    if (nrow(df) == 0) return(df)
    vintage_order <- data.frame(
      vintage_quarter = sort(unique(df$vintage_quarter)),
      quarter_rank = .opp_vintage_rank(sort(unique(df$vintage_quarter))),
      stringsAsFactors = FALSE
    ) %>%
      filter(!is.na(quarter_rank)) %>%
      arrange(quarter_rank)

    recent_vintages <- tail(vintage_order$vintage_quarter, 4)

    df %>%
      mutate(display_vintage = .opp_pretty_vintage(vintage_quarter, recent_vintages)) %>%
      group_by(display_vintage, Scenario, month_on_books) %>%
      summarise(
        cum_bad = sum(cum_bad, na.rm = TRUE),
        n_loans = sum(n_loans, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        bad_pct = ifelse(n_loans > 0, cum_bad / n_loans, NA_real_),
        vintage_quarter = display_vintage
      )
  }

  .opp_plot_vintage <- function(bl_data, sc_data, chart_title) {
    both <- bind_rows(bl_data, sc_data) %>%
      .opp_aggregate_vintage_display()
    if (nrow(both) == 0) {
      return(plot_ly() %>%
               layout(title = list(text = "No vintage data available",
                                   font = list(size = 13)),
                      paper_bgcolor = "white", plot_bgcolor = "white"))
    }
    both$legend_label <- both$vintage_quarter
    vintages_in_data <- sort(unique(both$vintage_quarter))
    legend_labels <- setNames(vintages_in_data, vintages_in_data)
    palette_keys <- unique(unname(legend_labels))
    pal <- if (length(palette_keys) <= 10) {
      setNames(RColorBrewer::brewer.pal(max(3, length(palette_keys)), "Set1")[seq_along(palette_keys)],
               palette_keys)
    } else {
      setNames(colorRampPalette(RColorBrewer::brewer.pal(8, "Set1"))(length(palette_keys)),
               palette_keys)
    }

    p <- plot_ly()
    shown_legend <- character(0)
    for (vq in vintages_in_data) {
      for (sc in unique(both$Scenario)) {
        sub <- both %>% filter(vintage_quarter == vq, Scenario == sc)
        if (nrow(sub) == 0) next
        legend_label <- legend_labels[[vq]]
        line_color <- pal[[legend_label]]
        show_legend <- !(legend_label %in% shown_legend)
        if (show_legend) shown_legend <- c(shown_legend, legend_label)
        p <- p %>% add_trace(
          data = sub, x = ~month_on_books, y = ~bad_pct,
          type = "scatter", mode = "lines+markers",
          name = legend_label,
          showlegend = show_legend,
          line = list(color = line_color,
                      dash = if (sc == "Baseline") "solid" else "dash",
                      width = 2, shape = "linear"),
          marker = list(color = line_color, size = 3),
          legendgroup = legend_label,
          hovertemplate = paste0(
            "<b>", legend_label, " (", sc, ")</b><br>",
            "MOB: %{x}<br>Delinquency: %{y:.1%}<br>",
            "Loans: ", fmt_k(sub$n_loans[1]), "<extra></extra>")
        )
      }
    }
    p %>% layout(
      title  = list(text = chart_title, font = list(size = 13)),
      xaxis  = list(title = "Month on Books"),
      yaxis  = list(title = "Cumulative Delinquency Rate", tickformat = ".1%"),
      paper_bgcolor = "white", plot_bgcolor = "white",
      annotations = list(list(
        x = 1, y = 1.08, xref = "paper", yref = "paper",
        text = "Solid = Baseline, Dotted = Scenario",
        showarrow = FALSE, xanchor = "right",
        font = list(size = 10, color = "#6b7280")
      )),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.3,
                    font = list(size = 9))
    )
  }

  # Vintage chart A: Baseline vs Scenario A (or Simulated in single mode)
  output$opp_vintage_plot_a <- renderPlotly({
    tryCatch({
    tape <- tryCatch(perf_tape(), error = function(e) {
      message("[OppExplorer] opp_vintage_plot_a perf_tape: ", e$message)
      NULL
    })
    if (is.null(tape)) {
      return(plot_ly() %>%
               layout(title = list(text = "No performance tape loaded",
                                   font = list(size = 13)),
                      paper_bgcolor = "white", plot_bgcolor = "white"))
    }
    req(opp_rv$df_full)
    sim_A <- opp_rv$sim_A
    dual  <- !is.null(sim_A) && !is.null(opp_rv$sim_B)

    if (dual) {
      bl_ids <- as.character(sim_A$df_full$loan_id[sim_A$df_full$issued_flag == 1])
      pr_ids <- as.character(sim_A$df_proposed$loan_id[sim_A$df_proposed$issued_flag == 1])
      sc_label <- sim_A$label %||% "Primary Action"
      adv_ids <- sim_A$adverse_flip_ids %||% character(0)
    } else {
      d <- opp_perf_data()
      bl_ids <- as.character(d$df_full$loan_id[d$df_full$issued_flag == 1])
      pr_ids <- as.character(d$df_proposed$loan_id[d$df_proposed$issued_flag == 1])
      sc_label <- "Simulated"
      adv_ids <- opp_rv$adverse_flip_ids %||% character(0)
    }

    sel_vqs <- input$opp_vintage_quarters
    if (is.null(sel_vqs) || length(sel_vqs) == 0) {
      return(plot_ly() %>%
               layout(title = list(text = "Select at least one vintage quarter",
                                   font = list(size = 13)),
                      paper_bgcolor = "white", plot_bgcolor = "white"))
    }

    bl_v <- .opp_build_vintage(tape, bl_ids, sel_vqs, "Baseline")
    pr_v <- .opp_build_vintage(tape, pr_ids, sel_vqs, sc_label,
                               adverse_ids = adv_ids)
    title <- sc_label
    .opp_plot_vintage(bl_v, pr_v, title)
    }, error = function(e) {
      message("[OppExplorer] opp_vintage_plot_a: ", e$message)
      plotly::plot_ly() %>%
        plotly::layout(title = list(text = "Vintage chart unavailable"),
                       annotations = list(text = paste("Error:", e$message),
                                          x = 0.5, y = 0.5, showarrow = FALSE,
                                          xref = "paper", yref = "paper"),
                       paper_bgcolor = "white", plot_bgcolor = "white",
                       xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
    })
  })

  # Vintage chart B: Baseline vs Scenario B (placeholder when single mode)
  output$opp_vintage_plot_b <- renderPlotly({
    tryCatch({
    tape <- tryCatch(perf_tape(), error = function(e) {
      message("[OppExplorer] opp_vintage_plot_b perf_tape: ", e$message)
      NULL
    })
    sim_A <- opp_rv$sim_A
    sim_B <- opp_rv$sim_B

    if (is.null(tape) || is.null(sim_A) || is.null(sim_B)) {
      msg <- if (is.null(tape)) "No performance tape loaded"
             else "Run a second scenario to compare"
      return(plot_ly() %>%
               layout(title = list(text = msg, font = list(size = 13, color = "#9ca3af")),
                      paper_bgcolor = "white", plot_bgcolor = "white",
                      xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)))
    }

    bl_ids <- as.character(sim_A$df_full$loan_id[sim_A$df_full$issued_flag == 1])
    pr_ids <- as.character(sim_B$df_proposed$loan_id[sim_B$df_proposed$issued_flag == 1])

    sel_vqs <- input$opp_vintage_quarters
    if (is.null(sel_vqs) || length(sel_vqs) == 0) {
      return(plot_ly() %>%
               layout(title = list(text = "Select at least one vintage quarter",
                                   font = list(size = 13)),
                      paper_bgcolor = "white", plot_bgcolor = "white"))
    }

    adv_ids_B <- sim_B$adverse_flip_ids %||% character(0)
    label_b <- sim_B$label %||% "Alternative Action"
    bl_v <- .opp_build_vintage(tape, bl_ids, sel_vqs, "Baseline")
    pr_v <- .opp_build_vintage(tape, pr_ids, sel_vqs, label_b,
                               adverse_ids = adv_ids_B)
    .opp_plot_vintage(bl_v, pr_v, label_b)
    }, error = function(e) {
      message("[OppExplorer] opp_vintage_plot_b: ", e$message)
      plotly::plot_ly() %>%
        plotly::layout(title = list(text = "Vintage chart unavailable"),
                       annotations = list(text = paste("Error:", e$message),
                                          x = 0.5, y = 0.5, showarrow = FALSE,
                                          xref = "paper", yref = "paper"),
                       paper_bgcolor = "white", plot_bgcolor = "white",
                       xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
    })
  })

  # ---- Delinquency by FICO band ----
  output$opp_bad_rate_plot <- renderPlotly({
    req(opp_rv$df_full)
    d <- opp_perf_data()

    bad_bl <- d$df_full %>%
      filter(issued_flag == 1, !is.na(fico_band)) %>%
      group_by(fico_band) %>%
      summarise(bad_rate = mean(performance_delinquency_flag, na.rm = TRUE),
                .groups = "drop") %>%
      mutate(Scenario = "Baseline")

    bad_pr <- d$df_proposed %>%
      filter(issued_flag == 1, !is.na(fico_band)) %>%
      group_by(fico_band) %>%
      summarise(bad_rate = mean(performance_delinquency_flag, na.rm = TRUE),
                .groups = "drop") %>%
      mutate(Scenario = "Simulated")

    bad_fico <- bind_rows(bad_bl, bad_pr) %>% arrange(fico_band)

    plot_ly(bad_fico,
            x = ~fico_band, y = ~bad_rate, color = ~Scenario,
            colors = c("Baseline" = "#6c757d", "Simulated" = "#fb6e36"),
            type = "bar") %>%
      layout(
        barmode = "group",
        title  = list(text = "Delinquency Rate by FICO Band: Baseline vs Simulated",
                      font = list(size = 14)),
        xaxis  = list(title = "FICO Band"),
        yaxis  = list(title = "Delinquency Rate", tickformat = ".1%"),
        paper_bgcolor = "white", plot_bgcolor = "white",
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2)
      )
  })

  # ---- IRR title (dynamic based on tape) ----
  output$opp_irr_title <- renderUI({
    tape <- tryCatch(perf_tape(), error = function(e) {
      message("[OppExplorer] opp_irr_title perf_tape: ", e$message)
      NULL
    })
    if (!is.null(tape)) {
      h4("Portfolio IRR (Cashflow-based)", style = "font-weight:bold;")
    } else {
      h4("Average IRR (APR proxy)", style = "font-weight:bold;")
    }
  })

  # ---- IRR bar chart (stacked, with level dropdown) ----
  output$opp_irr_plot <- renderPlotly({
    tryCatch({
    req(opp_rv$df_full)
    d     <- opp_perf_data()
    level <- input$opp_irr_level %||% "partner_channel"
    tape  <- tryCatch(perf_tape(), error = function(e) {
      message("[OppExplorer] opp_irr_plot perf_tape: ", e$message)
      NULL
    })

    wrap_axis_label <- function(x, width = 14) {
      x <- as.character(x)
      vapply(x, function(val) {
        if (is.na(val) || !nzchar(val)) return(val)
        paste(strwrap(val, width = width), collapse = "<br>")
      }, character(1))
    }

    # Pretty label for the level
    level_label <- switch(level,
      "Overall"              = "Overall",
      "fico_band"            = "FICO Band",
      "partner_channel"      = "Partner Channel",
      "risk_grade"           = "Grade",
      "borrower_repeat_type" = "Repeat Type",
      level
    )

    # Detect dual scenario mode
    sim_A <- opp_rv$sim_A
    sim_B <- opp_rv$sim_B
    dual  <- !is.null(sim_A) && !is.null(sim_B)

    if (!is.null(tape)) {
      # ---- Real XIRR from tape cashflows ----

      # Get loan IDs per scenario
      if (dual) {
        bl_ids   <- as.character(sim_A$df_full$loan_id[sim_A$df_full$issued_flag == 1])
        pr_A_ids <- as.character(sim_A$df_proposed$loan_id[sim_A$df_proposed$issued_flag == 1])
        pr_B_ids <- as.character(sim_B$df_proposed$loan_id[sim_B$df_proposed$issued_flag == 1])
      } else {
        bl_ids   <- as.character(d$df_full$loan_id[d$df_full$issued_flag == 1])
        pr_A_ids <- as.character(d$df_proposed$loan_id[d$df_proposed$issued_flag == 1])
      }

      compute_real_irr <- function(ids, scenario_label, sim_data = NULL) {
        rows_sub <- tape$rows[tape$rows$loan_id %in% ids, ]
        if (nrow(rows_sub) == 0)
          return(data.frame(Group = character(), irr_val = numeric(),
                            n_loans = integer(), Scenario = character(),
                            stringsAsFactors = FALSE))

        # Compute per-loan APR deltas from baseline vs scenario interest_rate_apr
        # (accounts for 72-month cap on repriced loans, mirrors compute_adjusted_xirr_v2)
        seg_src <- if (!is.null(sim_data)) sim_data else d
        bl_apr_map  <- setNames(seg_src$df_full$interest_rate_apr,
                                as.character(seg_src$df_full$loan_id))
        pr_apr_map  <- setNames(seg_src$df_proposed$interest_rate_apr,
                                as.character(seg_src$df_proposed$loan_id))
        apr_delta_map <- pr_apr_map[names(bl_apr_map)] - bl_apr_map
        loan_apr <- setNames(tape$loan_view$interest_rate_apr,
                             as.character(tape$loan_view$loan_id))
        has_delta <- scenario_label != "Baseline" &&
          any(!is.na(apr_delta_map) & apr_delta_map != 0)

        if (has_delta) {
          rows_sub <- rows_sub %>%
            arrange(loan_id, payment_date) %>%
            group_by(loan_id) %>%
            mutate(.period_int = interest_payment - lag(interest_payment, default = 0)) %>%
            ungroup()
          row_apr    <- loan_apr[rows_sub$loan_id]
          loan_delta <- apr_delta_map[rows_sub$loan_id]
          incr <- ifelse(!is.na(loan_delta) & loan_delta != 0 &
                         !is.na(row_apr) & row_apr > 0,
                         pmax(rows_sub$.period_int, 0) * loan_delta / row_apr, 0)
          rows_sub$adj_cf <- rows_sub$adj_cf + incr
          rows_sub$.period_int <- NULL
        }

        .combine_obs_proj <- function(obs_rows, loan_ids) {
          obs_agg <- obs_rows %>%
            group_by(payment_date) %>%
            summarise(cf = sum(adj_cf, na.rm = TRUE), .groups = "drop")
          proj_sub <- if (!is.null(tape$projected)) {
            tape$projected[tape$projected$loan_id %in% loan_ids, ]
          } else data.frame(payment_date = as.Date(character()), proj_cf = numeric(),
                            proj_int = numeric(), loan_id = character())
          # Adjust projected cashflows using per-loan APR deltas
          if (has_delta && nrow(proj_sub) > 0 && "proj_int" %in% names(proj_sub)) {
            p_apr   <- loan_apr[proj_sub$loan_id]
            p_delta <- apr_delta_map[proj_sub$loan_id]
            p_incr  <- ifelse(!is.na(p_delta) & p_delta != 0 &
                              !is.na(p_apr) & p_apr > 0,
                              pmax(proj_sub$proj_int, 0) * p_delta / p_apr, 0)
            proj_sub$proj_cf <- proj_sub$proj_cf + p_incr
          }
          proj_agg <- proj_sub %>%
            group_by(payment_date) %>%
            summarise(cf = sum(proj_cf, na.rm = TRUE), .groups = "drop")
          bind_rows(obs_agg, proj_agg) %>%
            group_by(payment_date) %>%
            summarise(cf = sum(cf), .groups = "drop") %>%
            arrange(payment_date)
        }

        if (level == "Overall") {
          port <- .combine_obs_proj(rows_sub, unique(rows_sub$loan_id))
          irr_val <- tryCatch(xirr(port$cf, port$payment_date), error = function(e) {
            message("[OppExplorer] opp_irr_plot xirr (Overall, ", scenario_label, "): ", e$message)
            NA_real_
          })
          n_loans <- length(unique(rows_sub$loan_id))
          data.frame(Group = "Overall", irr_val = irr_val, n_loans = n_loans,
                     Scenario = scenario_label, stringsAsFactors = FALSE)
        } else {
          if (!level %in% names(rows_sub))
            return(data.frame(Group = character(), irr_val = numeric(),
                              n_loans = integer(), Scenario = character(),
                              stringsAsFactors = FALSE))
          rows_sub %>%
            filter(!is.na(.data[[level]])) %>%
            group_by(grp = .data[[level]]) %>%
            group_modify(~ {
              port <- .combine_obs_proj(.x, unique(.x$loan_id))
              data.frame(irr_val = tryCatch(xirr(port$cf, port$payment_date),
                                            error = function(e) {
                                              message("[OppExplorer] opp_irr_plot xirr (", level, ", ", scenario_label, "): ", e$message)
                                              NA_real_
                                            }),
                         n_loans = length(unique(.x$loan_id)))
            }) %>%
            ungroup() %>%
            transmute(Group = grp, irr_val, n_loans, Scenario = scenario_label)
        }
      }

      irr_bl <- compute_real_irr(bl_ids, "Baseline")
      if (dual) {
        label_a <- sim_A$label %||% "Primary Action"
        label_b <- sim_B$label %||% "Alternative Action"
        irr_A <- compute_real_irr(pr_A_ids, label_a, sim_data = sim_A)
        irr_B <- compute_real_irr(pr_B_ids, label_b, sim_data = sim_B)
        both  <- bind_rows(irr_bl, irr_A, irr_B) %>% filter(!is.na(irr_val))
        bar_colors <- setNames(c("#6c757d", "#fb6e36", "#17a2b8"),
                               c("Baseline", label_a, label_b))
      } else {
        irr_pr <- compute_real_irr(pr_A_ids, "Simulated")
        both   <- bind_rows(irr_bl, irr_pr) %>% filter(!is.na(irr_val))
        bar_colors <- c("Baseline" = "#6c757d", "Simulated" = "#fb6e36")
      }

      if (nrow(both) == 0) {
        return(plot_ly() %>%
                 layout(title = list(text = "IRR could not be computed"),
                        paper_bgcolor = "white", plot_bgcolor = "white"))
      }

      both$GroupDisplay <- if (identical(level, "partner_channel")) {
        wrap_axis_label(both$Group, width = 12)
      } else {
        both$Group
      }

      plot_ly(both, x = ~GroupDisplay, y = ~irr_val, color = ~Scenario,
              colors = bar_colors,
              type = "bar",
              hovertext = ~paste0("<b>", Group, "</b><br>IRR: ",
                                  scales::percent(irr_val, accuracy = 0.1),
                                  "<br>N = ", fmt_k(n_loans)),
              hoverinfo = "text+name") %>%
        layout(
          barmode = "group",
          title   = list(text = paste("Portfolio IRR by", level_label),
                         font = list(size = 13)),
          xaxis   = list(title = level_label, tickangle = -25, automargin = TRUE,
                         tickfont = list(size = if (identical(level, "partner_channel")) 10 else 11)),
          yaxis   = list(title = "IRR (annualised)", tickformat = ".1%"),
          paper_bgcolor = "white", plot_bgcolor = "white",
          legend  = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.32),
          margin  = list(b = if (identical(level, "partner_channel")) 120 else 90)
        )

    } else {
      # ---- Fallback: average APR when no tape ----
      compute_irr_fallback <- function(df_src, scenario_label) {
        issued <- df_src %>% filter(issued_flag == 1, !is.na(interest_rate_apr))
        if (nrow(issued) == 0)
          return(data.frame(Group = character(), irr_val = numeric(),
                            n_loans = integer(), Scenario = character(),
                            stringsAsFactors = FALSE))
        if (level == "Overall") {
          data.frame(Group = "Overall",
                     irr_val = mean(issued$interest_rate_apr, na.rm = TRUE),
                     n_loans = nrow(issued),
                     Scenario = scenario_label, stringsAsFactors = FALSE)
        } else {
          if (!level %in% names(issued))
            return(data.frame(Group = character(), irr_val = numeric(),
                              n_loans = integer(), Scenario = character(),
                              stringsAsFactors = FALSE))
          issued %>%
            filter(!is.na(.data[[level]])) %>%
            group_by(Group = .data[[level]]) %>%
            summarise(irr_val = mean(interest_rate_apr, na.rm = TRUE),
                      n_loans = n(),
                      .groups = "drop") %>%
            mutate(Scenario = scenario_label)
        }
      }

      irr_bl <- compute_irr_fallback(d$df_full, "Baseline")
      if (dual) {
        label_a <- sim_A$label %||% "Primary Action"
        label_b <- sim_B$label %||% "Alternative Action"
        irr_A <- compute_irr_fallback(sim_A$df_proposed, label_a)
        irr_B <- compute_irr_fallback(sim_B$df_proposed, label_b)
        both  <- bind_rows(irr_bl, irr_A, irr_B)
        bar_colors <- setNames(c("#6c757d", "#fb6e36", "#17a2b8"),
                               c("Baseline", label_a, label_b))
      } else {
        irr_pr <- compute_irr_fallback(d$df_proposed, "Simulated")
        both   <- bind_rows(irr_bl, irr_pr)
        bar_colors <- c("Baseline" = "#6c757d", "Simulated" = "#fb6e36")
      }

      if (nrow(both) == 0) {
        return(plot_ly() %>%
                 layout(title = list(text = "No IRR data available"),
                        paper_bgcolor = "white", plot_bgcolor = "white"))
      }

      both$GroupDisplay <- if (identical(level, "partner_channel")) {
        wrap_axis_label(both$Group, width = 12)
      } else {
        both$Group
      }

      plot_ly(both, x = ~GroupDisplay, y = ~irr_val, color = ~Scenario,
              colors = bar_colors,
              type = "bar",
              hovertext = ~paste0("<b>", Group, "</b><br>Avg APR: ",
                                  scales::percent(irr_val, accuracy = 0.1),
                                  "<br>N = ", fmt_k(n_loans)),
              hoverinfo = "text+name") %>%
        layout(
          barmode = "group",
          title   = list(text = paste("Avg APR by", level_label, "(no tape)"),
                         font = list(size = 13)),
          xaxis   = list(title = level_label, tickangle = -25, automargin = TRUE,
                         tickfont = list(size = if (identical(level, "partner_channel")) 10 else 11)),
          yaxis   = list(title = "Avg APR", tickformat = ".1%"),
          paper_bgcolor = "white", plot_bgcolor = "white",
          legend  = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.32),
          margin  = list(b = if (identical(level, "partner_channel")) 120 else 90)
        )
    }
    }, error = function(e) {
      message("[OppExplorer] opp_irr_plot: ", e$message)
      plotly::plot_ly() %>%
        plotly::layout(title = list(text = "IRR chart unavailable"),
                       annotations = list(text = paste("Error:", e$message),
                                          x = 0.5, y = 0.5, showarrow = FALSE,
                                          xref = "paper", yref = "paper"),
                       paper_bgcolor = "white", plot_bgcolor = "white",
                       xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
    })
  })

  # ---- NL Q&A current-view registration ----
  # Returns the loans the Opp Explorer is currently operating on: the simulated
  # df_proposed when a simulation is active, else df_full, else loan_data.
  # Then narrows by whatever population filters the user has set (FICO band,
  # risk grade, partner channel).  The filters here are multi-select checkboxes
  # that default to "all"; skip each one when "all" is included.
  opp_nl_current_view <- reactive({
    base <- opp_rv$df_proposed %||% opp_rv$df_full
    if (is.null(base) || !is.data.frame(base) || nrow(base) == 0) base <- loan_data

    # FICO band filter (input$opp_fico — multi-select, "all" means no filter)
    fico_sel <- input$opp_fico
    if (!is.null(fico_sel) && !("all" %in% fico_sel) && length(fico_sel) > 0 &&
        "fico_band" %in% names(base)) {
      base <- base[base$fico_band %in% fico_sel, , drop = FALSE]
    }

    # Risk grade filter (input$opp_grade — multi-select, "all" means no filter)
    grade_sel <- input$opp_grade
    if (!is.null(grade_sel) && !("all" %in% grade_sel) && length(grade_sel) > 0 &&
        "risk_grade" %in% names(base)) {
      base <- base[base$risk_grade %in% grade_sel, , drop = FALSE]
    }

    # Partner channel filter (input$opp_partner — multi-select, "all" means no filter)
    partner_sel <- input$opp_partner
    if (!is.null(partner_sel) && !("all" %in% partner_sel) && length(partner_sel) > 0 &&
        "partner_channel" %in% names(base)) {
      base <- base[base$partner_channel %in% partner_sel, , drop = FALSE]
    }

    base
  })

  nl_register_current_view(session, "opp_find", opp_nl_current_view)

  # ---- Approval & Take Rates ----
  output$opp_rates_plot <- renderPlotly({
    req(opp_rv$df_full)
    d  <- opp_perf_data()
    bl <- compute_funnel(d$df_full)
    pr <- compute_funnel(d$df_proposed)

    rates_df <- data.frame(
      Metric   = rep(c("Approval Rate", "Take Rate"), 2),
      Scenario = rep(c("Baseline", "Simulated"), each = 2),
      Value    = c(
        ifelse(is.na(bl$appr_rate), 0, bl$appr_rate),
        ifelse(is.na(bl$conv_rate), 0, bl$conv_rate),
        ifelse(is.na(pr$appr_rate), 0, pr$appr_rate),
        ifelse(is.na(pr$conv_rate), 0, pr$conv_rate)
      )
    )

    plot_ly(rates_df, x = ~Metric, y = ~Value, color = ~Scenario,
            colors = c("Baseline" = "#6c757d", "Simulated" = "#fb6e36"),
            type = "bar") %>%
      layout(
        barmode = "group",
        title   = list(text = "Approval & Take Rates: Baseline vs Simulated",
                       font = list(size = 14)),
        xaxis   = list(title = ""),
        yaxis   = list(title = "Rate", tickformat = ".1%"),
        paper_bgcolor = "white", plot_bgcolor = "white",
        legend  = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2)
      )
  })

}
