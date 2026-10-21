# ============================================================
# Tab: What-If Simulator
# Extracted from app.R — UI and server logic
# ============================================================

# Feature flag: disables projected bad rate computation in render path.
# The simulate observer stores baseline/scenario funnels; projected bad rate
# is slow and synchronous at render time, which blanks the What-If outputs
# (Shiny renderer errors are hidden by CSS). Enable only when caches are warm.
.WI_USE_PROJECTED_BAD_RATE <- FALSE

# ---- What-If rule variable metadata ----
# `wi_rule_vars` is now defined in R/03_globals_policies.R so engine helpers
# (e.g. translate_rule_var_labels in R/engine/segments.R) can resolve friendly
# labels without depending on tab files being sourced first.
wi_rule_var_choices <- setNames(
  sapply(wi_rule_vars, `[[`, "id"),
  sapply(wi_rule_vars, `[[`, "label")
)

# Combine full-pool counts/rates with scope-filtered risk metrics. The funnel
# (Apps -> Approved -> Issued, Approval Rate, Take Rate) is a property of the
# engine on the full inflow and must not change when the user toggles scope.
# Risk/yield metrics (Avg APR, Avg Loan Size, Bad Rate) require performance
# data and therefore stay tied to the scope-filtered cohort.
.compute_funnel_merged <- function(full_df, scoped_df) {
  full_fn   <- compute_funnel(full_df)
  scoped_fn <- if (!is.null(scoped_df) && nrow(scoped_df) > 0) {
    compute_funnel(scoped_df)
  } else {
    full_fn
  }
  list(
    apps      = full_fn$apps,
    approved  = full_fn$approved,
    issued    = full_fn$issued,
    appr_rate = full_fn$appr_rate,
    conv_rate = full_fn$conv_rate,
    avg_apr   = scoped_fn$avg_apr,
    avg_amt   = scoped_fn$avg_amt,
    bad_rate  = scoped_fn$bad_rate
  )
}

whatif_ui <- function() {
      tabItem(tabName = "whatif_tab",

        header_panel(
          "Simulation Agent",
          paste("Adjust any policy parameter and instantly simulate",
                "the impact across all layers. Add segment-targeted rules",
                "for precision interventions."),
          icon_name = "sliders",
          data_scope = "apps_perf"
        ),

        # Product selector
        fluidRow(column(12,
          product_selector_ui("wi")
        )),

        # Active policy banner (reflects current active per product)
        active_policy_card_ui("wi"),

        # Data scope toggle (Applications vs Issued)
        fluidRow(column(12,
          div(style = "background:#f3f4f6; padding:10px 15px; border-radius:6px; margin-bottom:12px;",
              tags$b("Data scope:"),
              radioButtons("wi_data_scope", label = NULL,
                           choices = c("Applications" = "applications", "Issued" = "issued"),
                           selected = "issued", inline = TRUE))
        )),

        # ---- Smart Suggestions panel (hidden for demo) ----
        # fluidRow(column(12, div(class = "card", style = "padding: 15px;",
        #   tags$div(
        #     style = "display:flex; justify-content:space-between; align-items:center; cursor:pointer;",
        #     onclick = "Shiny.setInputValue('wi_toggle_suggestions', Math.random())",
        #     h4(tags$span(icon("lightbulb"), " Smart Suggestions"),
        #       style = "font-weight: bold; color: #fb6e36; margin: 0;"),
        #     tags$span(id = "wi_sug_chevron",
        #       icon("chevron-down"), style = "color: #9ca3af;")
        #   ),
        #   hidden(div(id = "wi_suggestions_panel",
        #     style = "margin-top: 15px;",
        #     uiOutput("wi_suggestions_cards")
        #   ))
        # ))),

        # ---- Population Filters ----
        fluidRow(column(12, div(class = "card",
          h4("Filters", style = "font-weight:bold; margin-bottom:10px;"),
          fluidRow(
            column(2, dateRangeInput("wi_date_range", "Vintage Date Range",
              start = vs_date_min, end = vs_date_max,
              min = vs_date_min, max = vs_date_max,
              format = "dd/mm/yyyy")),
            column(2, pickerInput("wi_fico", "FICO Band",
              choices = c("All" = "all"), selected = "all",
              multiple = TRUE, options = list(`actions-box` = TRUE))),
            column(2, pickerInput("wi_grade", "Grade",
              choices  = c("All" = "all", "High", "Med", "Low"),
              selected = "all",
              multiple = TRUE, options = list(`actions-box` = TRUE))),
            column(2, pickerInput("wi_partner", "Partner",
              choices = c("All" = "all"), selected = "all",
              multiple = TRUE, options = list(`actions-box` = TRUE))),
            column(2, pickerInput("wi_repeat", "Repeat Type",
              choices = c("All" = "all"), selected = "all",
              multiple = TRUE, options = list(`actions-box` = TRUE))),
            column(2, pickerInput("wi_mob", "Months on Books",
              choices = c("All" = "all"), selected = "all",
              multiple = TRUE, options = list(`actions-box` = TRUE)))
          )
        ))),

        # ---- Group Selector ----
        fluidRow(column(12, div(class = "card",
          h4("Parameter Group", style = "font-weight:bold; margin-bottom:10px;"),
          radioGroupButtons("wi_group", NULL,
            choices  = c("Grading", "Pricing", "Loan Limits",
                         "Decline Rules", "Tenure", "Affordability"),
            selected = "Grading",
            status   = "default",
            justified = TRUE,
            individual = TRUE)
        ))),

        # ---- Dynamic Sliders ----
        fluidRow(column(12, div(class = "card",
          uiOutput("wi_subtab_pills"),
          uiOutput("wi_sliders_dynamic"),
          # Cascade impact badges
          uiOutput("wi_cascade_badges"),
          # Validation messages
          uiOutput("wi_validation_msgs")
        ))),

        # ---- Apply a Rule Directly + Cutoff Explorer (side-by-side) ----
        fluidRow(
          # LEFT — Apply a Rule Directly (collapsible)
          column(6, div(class = "card",
            tags$div(
              style = "display:flex; justify-content:space-between; align-items:center; cursor:pointer;",
              onclick = "Shiny.setInputValue('wi_toggle_rules', Math.random())",
              h4("Apply a Rule Directly", style = "font-weight:bold; margin:0;"),
              tags$span(id = "wi_rules_chevron", style = "font-size:18px; color:#9ca3af;",
                        icon("chevron-down"))
            ),
            hidden(div(id = "wi_rules_panel",
              p("Define a segment condition and apply a Decline or Pricing action.",
                style = "color:#9ca3af; font-size:12px; margin:4px 0 12px 0;"),
              # Rule builder (full width within the half-card)
              h5("Condition 1", style = "font-weight:600; font-size:13px; margin-bottom:6px;"),
              fluidRow(
                column(5, selectInput("wi_seg_dim1", "Variable",
                  choices = wi_rule_var_choices)),
                column(3, selectInput("wi_seg_op1", "Operator",
                  choices = c("<", "<=", ">", ">=", "=="))),
                column(4, uiOutput("wi_seg_val1_ui"))
              ),
              fluidRow(
                column(12,
                  actionLink("wi_seg_add_cond", "+ Add Condition",
                    style = "font-size:12px; color:#fb6e36;"),
                  hidden(div(id = "wi_seg_cond2_row",
                    hr(style = "margin:8px 0;"),
                    fluidRow(
                      column(3, radioGroupButtons("wi_seg_logic", NULL,
                        choices = c("AND", "OR"), selected = "AND",
                        size = "xs", status = "warning")),
                      column(9, h5("Condition 2",
                        style = "font-weight:600; font-size:13px; margin-bottom:6px;"))
                    ),
                    fluidRow(
                      column(5, selectInput("wi_seg_dim2", "Variable",
                        choices = wi_rule_var_choices)),
                      column(3, selectInput("wi_seg_op2", "Operator",
                        choices = c("<", "<=", ">", ">=", "=="))),
                      column(4, uiOutput("wi_seg_val2_ui"))
                    ),
                    actionLink("wi_seg_rm_cond", "Remove",
                      style = "font-size:11px; color:#9ca3af;")
                  ))
                )
              ),
              hr(style = "margin:10px 0;"),
              h5("Action", style = "font-weight:600; font-size:13px; margin-bottom:6px;"),
              fluidRow(
                column(4, selectInput("wi_seg_action", "Action",
                  choices = c("Decline", "Pricing"), selected = "Decline")),
                column(4, conditionalPanel(
                  condition = "input.wi_seg_action == 'Pricing'",
                  selectInput("wi_seg_pricing_bps", "Pricing Change (BPS)",
                    choices = c("50", "100", "150", "Custom"),
                    selected = "100"))),
                column(4, conditionalPanel(
                  condition = "input.wi_seg_action == 'Pricing' && input.wi_seg_pricing_bps == 'Custom'",
                  numericInput("wi_seg_custom_bps", "Custom BPS",
                    value = 75, min = 1, max = 5000, step = 25)))
              ),
              actionButton("wi_seg_add_rule", "Add Rule",
                icon = icon("plus"), class = "custom-button",
                style = "margin-top:8px;"),
              # Active rules now sit below the rule builder so the card holds
              # everything in a single column.
              hr(style = "margin:14px 0 10px 0;"),
              h5("Active Rules", style = "font-weight:600; font-size:13px; margin-bottom:6px;"),
              DTOutput("wi_seg_rules_table", height = "180px"),
              br(),
              actionButton("wi_seg_rm_rule", "Remove Selected",
                icon = icon("trash"), class = "btn btn-sm",
                style = "background:#dc2626; color:white; border:none;")
            ))
          )),
          # RIGHT — Cutoff Explorer (collapsible)
          column(6, div(class = "card", style = "padding:12px 20px;",
            tags$div(
              style = "display:flex; justify-content:space-between; align-items:center; cursor:pointer;",
              onclick = "Shiny.setInputValue('wi_toggle_cutoff', Math.random())",
              h4("Cutoff Explorer", style = "font-weight:bold; margin:0;"),
              tags$span(id = "wi_cutoff_chevron", style = "font-size:18px; color:#9ca3af;",
                        icon("chevron-down"))
            ),
            hidden(div(id = "wi_cutoff_panel",
              p("Plot 2+ delinquency rate against any policy variable to find where to set a cutoff.",
                style = "color:#9ca3af; font-size:12px; margin:4px 0 12px 0;"),
              fluidRow(
                column(6, selectInput("wi_cutoff_var", "Variable",
                  choices = wi_rule_var_choices,
                  selected = unname(wi_rule_var_choices[1]))),
                column(6, selectInput("wi_cutoff_mob", "2+ Rate Definition",
                  choices = c("2+ Rate @3 MOB"   = "bad_3mob",
                              "2+ Rate @6 MOB"   = "bad_6mob",
                              "2+ Rate @9 MOB"   = "bad_9mob",
                              "2+ Rate @12 MOB"  = "bad_12mob",
                              "2+ Rate (Lifetime)" = "lifetime"),
                  selected = "bad_6mob"))
              ),
              plotly::plotlyOutput("wi_cutoff_plot", height = "320px")
            ))
          ))
        ),

        # ---- Quick Pricing Adjustment ----
        fluidRow(column(12, div(class = "card",
          style = "padding:12px 20px;",
          fluidRow(
            column(6,
              h4("Quick Pricing Adjustment",
                style = "font-weight:bold; margin-bottom:8px; font-size:14px;"),
              div(style = "display:flex; align-items:center; gap:12px; flex-wrap:wrap;",
                actionButton("wi_bps_minus", "-25 bps",
                  icon = icon("minus"),
                  style = "background:#374151; color:white; border:none; padding:6px 16px; font-size:13px;"),
                tags$span(id = "wi_bps_display",
                  style = "font-size:16px; font-weight:bold; color:#fb6e36; min-width:80px; text-align:center;",
                  "+0 bps"),
                actionButton("wi_bps_plus", "+25 bps",
                  icon = icon("plus"),
                  style = "background:#374151; color:white; border:none; padding:6px 16px; font-size:13px;"),
                tags$span("|", style = "color:#4b5563;"),
                tags$span("Shifts all grade APRs uniformly",
                  style = "color:#9ca3af; font-size:12px;"),
                actionButton("wi_bps_reset", "Reset",
                  style = "background:transparent; color:#9ca3af; border:1px solid #4b5563; padding:4px 12px; font-size:11px;")
              )
            ),
          )
        ))),

        # ---- Simulation Controls ----
        fluidRow(column(12, div(class = "card", style = "padding:12px 20px;",
          fluidRow(
            column(4,
              radioGroupButtons("wi_sim_slot", "Save Result As",
                choices = c("Primary Action" = "A", "Alternative Action" = "B"),
                selected = "A", size = "sm", status = "warning", justified = FALSE),
              div(style = "margin-top:4px;",
                actionLink("wi_clear_A", label = tagList(icon("trash"), " Clear Primary"),
                  style = "color:#dc2626; font-size:11px; margin-right:12px;"),
                actionLink("wi_clear_B", label = tagList(icon("trash"), " Clear Alt"),
                  style = "color:#dc2626; font-size:11px;"))
            ),
            column(8, style = "text-align:right; padding-top:16px;",
              actionButton("wi_reset", "Reset to Baseline",
                icon = icon("undo"), class = "btn",
                style = "margin-right:16px; background:#374151; color:white; border:none; padding:10px 40px; font-size:16px; border-radius:6px;"),
              actionButton("wi_simulate", "Simulate",
                icon = icon("play"), class = "custom-button",
                style = "padding:10px 40px; font-size:16px;")
            )
          )
        ))),

        # Analysis pipeline (visible during simulation)
        pipeline_container("wi_sim_pipeline"),

        # ---- Results (hidden until Simulate clicked) ----
        hidden(div(id = "wi_results_panel",
          # Scenario summary (reminds user what each slot contains)
          uiOutput("wi_scenario_summary"),
          # Dynamic KPI panel (single or dual row)
          uiOutput("wi_kpi_panel"),
          # AI Insights panel (UC3)
          uiOutput("wi_ai_insights"),
          # Talking Points button (UC7)
          div(style = "text-align:right; margin:8px 0;",
            actionButton("wi_talking_pts", "Talking Points",
                         icon = icon("clipboard"),
                         class = "btn-sm",
                         style = "background:#374151; color:white; border:none;")
          ),
          # Sample preview cards (dynamic — shows both slots when available)
          hidden(div(id = "wi_sample_preview_panel",
            uiOutput("wi_sample_preview_cards")
          )),
          # Performance filters
          fluidRow(column(12, div(class = "card",
            h4("Analysis Filters", style = "font-weight:bold;"),
            fluidRow(
              column(3, pickerInput("wi_perf_fico", "FICO Band",
                choices = c("All" = "all"), selected = "all",
                multiple = TRUE, options = list(`actions-box` = TRUE))),
              column(2, pickerInput("wi_perf_grade", "Grade",
                choices = c("All" = "all"), selected = "all",
                multiple = TRUE, options = list(`actions-box` = TRUE))),
              column(3, pickerInput("wi_perf_partner", "Partner Channel",
                choices = c("All" = "all"), selected = "all",
                multiple = TRUE, options = list(`actions-box` = TRUE))),
              column(2, pickerInput("wi_perf_repeat", "Repeat Type",
                choices = c("All" = "all"), selected = "all",
                multiple = TRUE, options = list(`actions-box` = TRUE))),
              column(2, pickerInput("wi_perf_mob", "Months on Books",
                choices = c("All" = "all"), selected = "all",
                multiple = TRUE, options = list(`actions-box` = TRUE)))
            )
          ))),
          # Funnel table + IRR chart side by side
          fluidRow(
            column(6, div(class = "card",
              uiOutput("wi_funnel_title"),
              uiOutput("wi_funnel_subtitle"),
              uiOutput("wi_funnel_table")
            )),
            column(6, div(class = "card",
              h4("Portfolio IRR Comparison", style = "font-weight:bold;"),
              selectInput("wi_irr_level", NULL,
                choices = c("Overall", "FICO Band" = "fico_band",
                            "Partner Channel" = "partner_channel",
                            "Grade" = "risk_grade",
                            "Repeat Type" = "borrower_repeat_type"),
                selected = "partner_channel", width = "200px"),
              plotlyOutput("wi_irr_plot", height = "380px")))
          ),
          br(),
          # Vintage curve
          # Vintage curves (dual side-by-side)
          fluidRow(column(12, div(class = "card",
            h4("Vintage Performance Curves", style = "font-weight:bold;"),
            pickerInput("wi_vintage_quarters", "Select Vintages:",
              choices = NULL, selected = NULL, multiple = TRUE,
              options = list(`actions-box` = TRUE, `live-search` = TRUE,
                             `selected-text-format` = "count > 3",
                             `count-selected-text` = "{0} vintages selected"),
              width = "100%"),
            fluidRow(
              column(12, id = "wi_vintage_col_a",
                plotlyOutput("wi_vintage_plot_a", height = "420px")),
              hidden(column(6, id = "wi_vintage_col_b",
                plotlyOutput("wi_vintage_plot_b", height = "420px")))
            )
          ))),
          br(),
          # Save to Repository
          fluidRow(column(12, div(class = "card", style = "padding:15px;",
            h4("Save Policy to Repository", style = "font-weight:bold;"),
            p("Save a simulated policy action to the repository for comparison.",
              style = "color:#9ca3af; margin-bottom:12px;"),
            fluidRow(
              column(12,
                textInput("wi_save_name", "Policy Name",
                  placeholder = "e.g., Tighten DTI + Raise Low-Grade APR")),
              column(12,
                div(style = "display:flex; gap:10px; padding-top:10px;",
                  div(style = "flex:1;",
                    actionButton("wi_sql_baseline_save", "Baseline SQL",
                      icon = icon("database"), class = "btn-sm", width = "100%",
                      style = "background-color:#334155; color:white; border:none;")),
                  div(style = "flex:1;",
                    actionButton("wi_save_btn_A", "Save Primary",
                      icon = icon("save"), class = "btn-warning btn-sm", width = "100%")),
                  div(style = "flex:1;",
                    downloadButton("wi_download_policy_A", "SQL Primary",
                      class = "btn-sm",
                      style = "background-color:#10b981; color:white; width:100%;")),
                  div(style = "flex:1;",
                    actionButton("wi_save_btn_B", "Save Alternative",
                      icon = icon("save"), class = "btn-info btn-sm", width = "100%")),
                  div(style = "flex:1;",
                    downloadButton("wi_download_policy_B", "SQL Alternative",
                      class = "btn-sm",
                      style = "background-color:#0ea5e9; color:white; width:100%;"))
                ))
            )
          )))
        ))
      )
}

# ---- Flowchart modal helper ----
build_flowchart_html <- function(params, default_params, segment_rules,
                                  n_apps, baseline_funnel, scenario_funnel) {
  impact <- compute_cascade_impact(params, default_params)

  # Format helper
  fmt <- function(v, unit) {
    if (unit == "%") paste0(round(v * 100, 1), "%")
    else if (unit == "$") paste0("$", format(v, big.mark = ","))
    else if (unit == "mo") paste0(v, " mo")
    else as.character(v)
  }

  # Map changed param IDs to their labels and old/new values
  param_details <- list()
  for (p in policy_manifest) {
    if (p$id %in% impact$changed_ids) {
      param_details[[p$id]] <- list(
        label = p$label,
        old = fmt(default_params[[p$id]], p$unit),
        new = fmt(params[[p$id]], p$unit),
        layers = p$layers
      )
    }
  }

  # Build rule-detail rows: list of tags$div per param in a layer
  # Shows ALL params for the layer — changed ones highlighted, others dimmed
  build_detail_panel <- function(lyr, layer_changes) {
    # Collect all manifest params for this layer
    layer_params <- Filter(function(p) {
      !identical(p$type, "display") && lyr %in% p$layers
    }, policy_manifest)

    if (length(layer_params) == 0) return(NULL)

    rows <- lapply(layer_params, function(p) {
      is_changed <- p$id %in% names(param_details)
      cur_val <- fmt(params[[p$id]], p$unit)
      if (is_changed) {
        pd <- param_details[[p$id]]
        tags$div(class = "rule-row",
          tags$span(class = "rule-param", p$label),
          tags$span(class = "rule-val old", pd$old),
          tags$span(class = "rule-arrow", "\u2192"),
          tags$span(class = "rule-val new", pd$new)
        )
      } else {
        tags$div(class = "rule-row",
          tags$span(class = "rule-param", p$label),
          tags$span(class = "rule-val", cur_val)
        )
      }
    })
    rows
  }

  # Static rule descriptions for layers without adjustable params
  static_rules <- list(
    "10" = list(
      tags$div(class = "rule-row",
        tags$span(class = "rule-param", "Rent Estimate"),
        tags$span(class = "rule-val", "max(monthly_rent, $300)")),
      tags$div(class = "rule-row",
        tags$span(class = "rule-param", "DTI Ratio"),
        tags$span(class = "rule-val", "(debt + rent) / annual income")),
      tags$div(class = "rule-row",
        tags$span(class = "rule-param", "Social Tax"),
        tags$span(class = "rule-val", "6% if income < $140K")),
      tags$div(class = "rule-row",
        tags$span(class = "rule-param", "Post-Tax Income"),
        tags$span(class = "rule-val", "income - social tax - med tax (4%)"))
    )
  )

  # Layer card definitions
  all_layers <- c(10, 20, 30, 40, 50, 60, 70, 80, 90)
  layer_info <- list(
    "10" = list(name = "Calculate Borrower Profile",  icon = "\U0001F464", hint = "income, rent, DTI"),
    "20" = list(name = "Assign Risk Grade",           icon = "\U0001F3AF", hint = "3-model score waterfall \u2192 High / Med / Low"),
    "30" = list(name = "Set Base Interest Rate",      icon = "\U0001F4B2", hint = "APR by risk grade"),
    "40" = list(name = "Apply Channel Pricing",       icon = "\U0001F4E1", hint = "direct-channel add-ons"),
    "50" = list(name = "Set Maximum Loan Amount",     icon = "\U0001F4CA", hint = "income & exposure caps"),
    "60" = list(name = "Apply Decline Rules",         icon = "\U0001F6AB", hint = "FICO, DTI, delinquency checks"),
    "70" = list(name = "Assign Loan Term",            icon = "\U0001F4C5", hint = "tenure by grade + extensions"),
    "80" = list(name = "Check Affordability",         icon = "\U0001F3E6", hint = "payment-to-income caps"),
    "90" = list(name = "Final Offer Decision",        icon = "\u2705",     hint = "offered amount & approval")
  )

  steps <- list()
  delay_idx <- 0

  # Input card (not clickable)
  delay_idx <- delay_idx + 1
  steps[[length(steps) + 1]] <- tags$div(
    class = "wi-flow-step",
    style = sprintf("animation-delay: %dms; cursor: default;", delay_idx * 150),
    tags$span(class = "step-icon", "\U0001F4C4"),
    tags$div(class = "step-body",
      tags$div(class = "step-label", "Load Application Data"),
      tags$div(class = "step-detail",
        paste0(format(n_apps, big.mark = ","), " applications loaded"))
    )
  )
  steps[[length(steps) + 1]] <- tags$div(class = "wi-flow-connector")

  # Layer cards
  for (lyr in all_layers) {
    lyr_str <- as.character(lyr)
    info <- layer_info[[lyr_str]]
    is_changed <- lyr %in% impact$direct_layers
    is_affected <- !is_changed && lyr %in% impact$all_layers

    css_class <- paste("wi-flow-step",
      if (is_changed) "changed" else if (is_affected) "affected" else "")

    step_icon <- if (is_changed) "\U0001F538" else if (is_affected) "\U0001F539" else info$icon

    # Collect param changes for this layer
    layer_changes <- Filter(function(pd) lyr %in% pd$layers, param_details)

    detail_text <- if (length(layer_changes) > 0) {
      paste(sapply(layer_changes, function(pd)
        paste0(pd$label, ": ", pd$old, " \u2192 ", pd$new)
      ), collapse = " | ")
    } else if (is_affected) {
      paste0("re-evaluated \u2014 ", info$hint)
    } else {
      info$hint
    }

    badge <- if (is_changed) {
      tags$span(style = "color:#fb6e36; font-size:10px; font-weight:bold;", " CHANGED")
    } else if (is_affected) {
      tags$span(style = "color:#f59e0b; font-size:10px;", " CASCADE")
    }

    # Build expandable detail panel
    detail_rows <- if (lyr_str %in% names(static_rules)) {
      static_rules[[lyr_str]]
    } else {
      build_detail_panel(lyr, layer_changes)
    }

    panel_id <- paste0("wi_flow_detail_", lyr)

    delay_idx <- delay_idx + 1
    steps[[length(steps) + 1]] <- tags$div(
      class = css_class, id = paste0("wi_flow_step_", lyr),
      style = sprintf("animation-delay: %dms;", delay_idx * 150),
      onclick = sprintf(
        "var p=document.getElementById('%s'); var s=this; if(p.classList.contains('open')){p.classList.remove('open');s.classList.remove('expanded');}else{p.classList.add('open');s.classList.add('expanded');}",
        panel_id),
      tags$span(class = "step-icon", step_icon),
      tags$div(class = "step-body",
        tags$div(class = "step-label", info$name, badge),
        tags$div(class = "step-detail", detail_text)
      ),
      tags$span(class = "step-chevron", "\u25B8")
    )

    # Expandable detail panel (hidden by default)
    if (!is.null(detail_rows) && length(detail_rows) > 0) {
      steps[[length(steps) + 1]] <- tags$div(
        class = "wi-flow-expand", id = panel_id,
        detail_rows
      )
    } else {
      # Empty hidden panel so JS toggle doesn't error
      steps[[length(steps) + 1]] <- tags$div(
        class = "wi-flow-expand", id = panel_id,
        tags$div(class = "rule-row",
          tags$span(class = "rule-param", "No adjustable parameters"),
          tags$span(class = "rule-val", "fixed logic"))
      )
    }

    if (lyr < 90) steps[[length(steps) + 1]] <- tags$div(class = "wi-flow-connector")
  }

  # Manual rules section (Pass 2)
  if (length(segment_rules) > 0) {
    steps[[length(steps) + 1]] <- tags$hr(style = "border-color:#374151; margin:12px 0;")
    delay_idx <- delay_idx + 1
    steps[[length(steps) + 1]] <- tags$div(
      style = sprintf("color:#9ca3af; font-size:11px; font-weight:600; padding-left:16px; opacity:0; animation: wi-flow-in 0.3s ease forwards; animation-delay:%dms;", delay_idx * 150),
      paste0("PASS 2: ", length(segment_rules), " SEGMENT RULE",
             if (length(segment_rules) > 1) "S")
    )
    for (i in seq_along(segment_rules)) {
      rule <- segment_rules[[i]]
      rule_panel_id <- paste0("wi_flow_rule_", i)
      delay_idx <- delay_idx + 1

      # Rule detail rows
      rule_detail <- list(
        tags$div(class = "rule-row",
          tags$span(class = "rule-param", "Condition"),
          tags$span(class = "rule-val", rule$cond_str)),
        tags$div(class = "rule-row",
          tags$span(class = "rule-param", "Action"),
          tags$span(class = "rule-val new", rule$action_str))
      )
      if (rule$action == "Pricing") {
        rule_detail <- c(rule_detail, list(
          tags$div(class = "rule-row",
            tags$span(class = "rule-param", "Downstream"),
            tags$span(class = "rule-val",
              "Replays Loan Limits \u2192 Affordability \u2192 Final Decision"))
        ))
      } else {
        rule_detail <- c(rule_detail, list(
          tags$div(class = "rule-row",
            tags$span(class = "rule-param", "Effect"),
            tags$span(class = "rule-val",
              "All approved loans matching condition \u2192 declined"))
        ))
      }

      steps[[length(steps) + 1]] <- tags$div(
        class = "wi-flow-step changed",
        style = sprintf("animation-delay: %dms;", delay_idx * 150),
        onclick = sprintf(
          "var p=document.getElementById('%s'); var s=this; if(p.classList.contains('open')){p.classList.remove('open');s.classList.remove('expanded');}else{p.classList.add('open');s.classList.add('expanded');}",
          rule_panel_id),
        tags$span(class = "step-icon", "\U0001F6D1"),
        tags$div(class = "step-body",
          tags$div(class = "step-label", rule$cond_str),
          tags$div(class = "step-detail",
            paste0("Action: ", rule$action_str))
        ),
        tags$span(class = "step-chevron", "\u25B8")
      )
      steps[[length(steps) + 1]] <- tags$div(
        class = "wi-flow-expand", id = rule_panel_id,
        rule_detail
      )
    }
  }

  # Result card (not clickable)
  steps[[length(steps) + 1]] <- tags$div(class = "wi-flow-connector")
  delay_idx <- delay_idx + 1

  bl <- baseline_funnel
  sc <- scenario_funnel
  appr_delta <- if (!is.null(bl) && !is.null(sc))
    round((sc$appr_rate - bl$appr_rate) * 100, 1) else 0
  apr_delta <- if (!is.null(bl) && !is.null(sc))
    round((sc$avg_apr - bl$avg_apr) * 10000) else 0

  sign_appr <- if (appr_delta >= 0) "+" else ""
  sign_apr  <- if (apr_delta >= 0) "+" else ""

  steps[[length(steps) + 1]] <- tags$div(
    class = "wi-flow-step result",
    style = sprintf("animation-delay: %dms; cursor: default;", delay_idx * 150),
    tags$span(class = "step-icon", "\u2714"),
    tags$div(class = "step-body",
      tags$div(class = "step-label", "Simulation Complete"),
      tags$div(class = "step-detail", style = "color:#22c55e;",
        paste0("Approval: ", sign_appr, appr_delta, "pp",
               " | Avg APR: ", sign_apr, apr_delta, " bps"))
    )
  )

  tags$div(style = "padding: 8px 0;", steps)
}

whatif_server <- function(input, output, session,
                          credit_data, policy_input_data, perf_tape,
                          added_policy, policy_fns_rv) {

  # ---- Active policy banner under product selector ----
  wire_active_policy_card(input, output, session, "wi")

  # Normalize a rule operator: accept raw, HTML-encoded, or Unicode variants.
  # Returns one of the five canonical strings, or NA_character_ if unknown.
  .wi_normalize_op <- function(op) {
    if (is.null(op) || length(op) == 0 || is.na(op)) return(NA_character_)
    x <- as.character(op)[1]
    x <- gsub("&lt;", "<", x, fixed = TRUE)
    x <- gsub("&gt;", ">", x, fixed = TRUE)
    x <- gsub("\u2264", "<=", x, fixed = TRUE)  # ≤
    x <- gsub("\u2265", ">=", x, fixed = TRUE)  # ≥
    x <- trimws(x)
    if (x %in% c("<", "<=", ">", ">=", "==")) x else NA_character_
  }

  # Helper: build a logical mask for a single column condition WITHOUT eval(parse())
.wi_build_col_mask <- function(df, col, op, val, type = "numeric") {
    op <- .wi_normalize_op(op)
    if (is.na(op)) return(rep(FALSE, nrow(df)))
    if (!col %in% names(df)) return(rep(FALSE, nrow(df)))
    x <- df[[col]]
    if (type == "numeric") {
      val <- as.numeric(val)
      switch(op,
        "<"  = !is.na(x) & x <  val,
        "<=" = !is.na(x) & x <= val,
        ">"  = !is.na(x) & x >  val,
        ">=" = !is.na(x) & x >= val,
        "==" = !is.na(x) & x == val,
        rep(FALSE, nrow(df)))
    } else {
      !is.na(x) & x == val
    }
  }

  # Helper: persist added policies and their closures to disk
  wi_persist_policies <- function() {
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

  # Initialize current_params with active policy overrides so the
  # active-policy thresholds are loaded on first visit instead of the
  # manifest "disabled" sentinels (99L).
  # Helper: build a params list seeded from the active policy for the
  # selected product. Used for reset/restore/label logic.
  wi_pl_current_params <- function() {
    p <- build_default_params(policy_manifest)
    prod <- tryCatch(input$wi_product, error = function(e) "Personal Loan")
    active_pid <- get_active_policy_id(prod %||% "Personal Loan")
    .so <- tryCatch(get0("strategy_overrides", envir = .GlobalEnv, inherits = FALSE),
                    error = function(e) NULL)
    if (!is.null(.so) && !is.null(.so[[active_pid]])) {
      p <- modifyList(p, .so[[active_pid]]$params)
    }
    p
  }

  .wi_initial_params <- wi_pl_current_params()

  wi_rv <- reactiveValues(
    current_params  = .wi_initial_params,
    segment_rules   = list(),
    show_cond2      = FALSE,
    active_group    = "Decline Rules",
    active_subtab   = NULL,
    scenario_df     = NULL,
    scenario_full   = NULL,
    scenario_funnel = NULL,
    baseline_funnel = NULL,
    baseline_cd     = NULL,
    baseline_full   = NULL,
    bps_adj         = 0L,
    # Sample preview state
    sample_preview  = NULL,
    # Dual scenario slots
    sim_A           = NULL,
    sim_B           = NULL
  )

  # ---- Smart Suggestions: toggle panel ----
  observeEvent(input$wi_toggle_suggestions, {
    toggle("wi_suggestions_panel")
  })

  # ---- Smart Suggestions: compute on tab activation ----
  wi_suggestions <- reactiveVal(NULL)
  wi_sug_computed <- reactiveVal(FALSE)

  observe({
    req(input$sidebar_menu == "whatif_tab")
    if (wi_sug_computed()) return()
    wi_sug_computed(TRUE)

    df <- credit_data
    pid <- policy_input_data

    # Sample for speed — 5K rows is enough for directional estimates
    if (nrow(pid) > 5000) {
      set.seed(42)
      sample_ids <- sample(pid$loan_id, 5000)
      pid <- pid[pid$loan_id %in% sample_ids, ]
      df <- df[df$loan_id %in% sample_ids, ]
    }

    default_params <- wi_pl_current_params()

    # Parameter sweep candidates
    sweep_specs <- list(
      list(param = "fico_min",         values = c(580, 600, 620, 640),
           label = "FICO Floor",       slider_id = "wi_p_fico_min"),
      list(param = "dti_cutoff",       values = c(0.35, 0.40, 0.45),
           label = "DTI Cap",          slider_id = "wi_p_dti_cutoff"),
      list(param = "delinquency_max",  values = c(0),
           label = "Delinquency Max",  slider_id = "wi_p_delinquency_max"),
      list(param = "thin_file_months", values = c(36, 42, 48),
           label = "Thin File Cutoff", slider_id = "wi_p_thin_file_months")
    )

    # Join performance columns from credit_data
    perf_cols <- c("loan_id", "issued_flag", "performance_delinquency_flag")
    perf_join <- df[, intersect(perf_cols, names(df)), drop = FALSE]

    # Helper: compute approval rate and bad rate among approved+issued loans
    sug_kpis <- function(result_df) {
      n_total <- nrow(result_df)
      approved <- result_df$offered_decision_flag == "APPR"
      appr_rate <- sum(approved) / n_total
      # Bad rate: among loans that are approved AND were historically issued
      issued_mask <- !is.na(result_df$issued_flag) & result_df$issued_flag == 1
      pool <- approved & issued_mask
      bad_rate <- if (sum(pool) > 0)
        mean(result_df$performance_delinquency_flag[pool], na.rm = TRUE)
      else NA_real_
      list(appr_rate = appr_rate, bad_rate = bad_rate, n_approved = sum(approved))
    }

    baseline <- apply_policy_with_params(pid, default_params)
    baseline <- merge(baseline, perf_join, by = "loan_id", all.x = TRUE, suffixes = c("", ".perf"))
    base_kpi <- sug_kpis(baseline)
    if (is.na(base_kpi$bad_rate) || base_kpi$n_approved == 0) return()

    suggestions <- list()

    for (spec in sweep_specs) {
      current_val <- default_params[[spec$param]]
      for (new_val in spec$values) {
        if (identical(new_val, current_val)) next
        test_params <- default_params
        test_params[[spec$param]] <- new_val
        scenario <- apply_policy_with_params(pid, test_params)
        scenario <- merge(scenario, perf_join, by = "loan_id", all.x = TRUE, suffixes = c("", ".perf"))
        scen_kpi <- sug_kpis(scenario)

        if (is.na(scen_kpi$bad_rate)) next
        bad_delta <- scen_kpi$bad_rate - base_kpi$bad_rate
        appr_delta <- scen_kpi$appr_rate - base_kpi$appr_rate

        if (bad_delta < -0.001) {
          efficiency <- abs(bad_delta) / max(abs(appr_delta), 0.001)
          suggestions[[length(suggestions) + 1]] <- list(
            param     = spec$param,
            label     = spec$label,
            slider_id = spec$slider_id,
            old_val   = current_val,
            new_val   = new_val,
            bad_delta = bad_delta,
            appr_delta = appr_delta,
            efficiency = efficiency
          )
        }
      }
    }

    if (length(suggestions) > 0) {
      suggestions <- suggestions[order(-sapply(suggestions, function(s) s$efficiency))]
      suggestions <- head(suggestions, 5)
    }
    wi_suggestions(suggestions)
  })

  # ---- Smart Suggestions: render cards ----
  output$wi_suggestions_cards <- renderUI({
    sugs <- wi_suggestions()
    if (is.null(sugs)) {
      return(p("Analyzing portfolio for quick wins...",
        style = "color: #9ca3af; text-align: center; padding: 20px;"))
    }
    if (length(sugs) == 0) {
      return(p("No quick wins found \u2014 the current policy is already well-optimized for this population.",
        style = "color: #9ca3af; text-align: center; padding: 20px;"))
    }

    fmt_param <- function(param, val) {
      if (param == "dti_cutoff") return(scales::percent(val, accuracy = 1))
      if (grepl("apr", param)) return(scales::percent(val, accuracy = 0.1))
      as.character(round(val))
    }

    n_sugs <- length(sugs)
    col_width <- if (n_sugs <= 3) 4 else if (n_sugs == 4) 3 else 2

    cards <- lapply(seq_along(sugs), function(i) {
      s <- sugs[[i]]
      column(col_width,
        div(style = "background: #1f2937; border-radius: 8px; padding: 15px; margin-bottom: 10px; border-left: 3px solid #fb6e36;",
          h5(s$label, style = "color: #fb6e36; font-weight: bold; margin: 0 0 8px 0;"),
          p(paste(fmt_param(s$param, s$old_val), "\u2192", fmt_param(s$param, s$new_val)),
            style = "color: #ffffff; font-size: 16px; font-weight: bold; margin: 0 0 8px 0;"),
          p(paste0(round(s$bad_delta * 10000), "bps 2+ rate"),
            style = paste0("color: ", if (s$bad_delta < 0) "#10b981" else "#ef4444",
                           "; font-size: 13px; margin: 2px 0;")),
          p(paste0(ifelse(s$appr_delta < 0, "", "+"),
                   round(s$appr_delta * 100, 1), "pp approval rate"),
            style = paste0("color: ", if (s$appr_delta < 0) "#ef4444" else "#10b981",
                           "; font-size: 13px; margin: 2px 0 10px 0;")),
          actionButton(paste0("wi_sug_apply_", i), "Apply",
            class = "btn btn-sm",
            style = "background: #fb6e36; color: white; border: none; width: 100%;")
        )
      )
    })

    do.call(fluidRow, cards)
  })

  # ---- Smart Suggestions: apply buttons ----
  observe({
    sugs <- wi_suggestions()
    req(sugs)
    lapply(seq_along(sugs), function(i) {
      observeEvent(input[[paste0("wi_sug_apply_", i)]], {
        s <- sugs[[i]]
        updateSliderInput(session, s$slider_id, value = s$new_val)
        wi_rv$current_params[[s$param]] <- s$new_val
        showNotification(
          paste0("Applied: ", s$label, " \u2192 ",
                 if (s$param == "dti_cutoff") scales::percent(s$new_val, accuracy = 1) else s$new_val,
                 ". Click Simulate to see results."),
          type = "message", duration = 4
        )
      }, ignoreInit = TRUE, once = TRUE)
    })
  })

  # TAB 2 – WHAT-IF SIMULATOR
  # ============================================================

  # ---- Populate top population filters from credit_data ----
  observe({
    cd <- credit_data
    if ("fico_band" %in% names(cd)) {
      fb <- sort(unique(as.character(cd$fico_band[!is.na(cd$fico_band)])))
      updatePickerInput(session, "wi_fico",
        choices = c("All" = "all", setNames(fb, fb)), selected = "all")
    }
    if ("partner_channel" %in% names(cd)) {
      pc <- sort(unique(as.character(cd$partner_channel[!is.na(cd$partner_channel)])))
      updatePickerInput(session, "wi_partner",
        choices = c("All" = "all", setNames(pc, pc)), selected = "all")
    }
    if ("borrower_repeat_type" %in% names(cd)) {
      rt <- sort(unique(as.character(cd$borrower_repeat_type[!is.na(cd$borrower_repeat_type)])))
      updatePickerInput(session, "wi_repeat",
        choices = c("All" = "all", setNames(rt, rt)), selected = "all")
    }
    if ("mob_bins" %in% names(cd)) {
      mb <- sort(unique(as.character(cd$mob_bins[!is.na(cd$mob_bins)])))
      updatePickerInput(session, "wi_mob",
        choices = c("All" = "all", setNames(mb, mb)), selected = "all")
    }
  })

  # ---- Subtab pills for groups with multiple subtabs ----
  output$wi_subtab_pills <- renderUI({
    grp <- input$wi_group
    req(grp)
    subtabs <- unique(sapply(
      Filter(function(p) p$group == grp, policy_manifest), `[[`, "subtab"
    ))
    if (length(subtabs) <= 1) return(NULL)
    radioGroupButtons(
      "wi_subtab", NULL,
      choices  = subtabs,
      selected = subtabs[1],
      size     = "sm",
      status   = "default"
    )
  })

  # ---- Dynamic sliders from policy_manifest ----
  output$wi_sliders_dynamic <- renderUI({
    grp <- input$wi_group
    req(grp)
    subtab <- input$wi_subtab

    entries <- Filter(function(p) p$group == grp, policy_manifest)
    # Filter by subtab if multiple exist
    subtabs <- unique(sapply(entries, `[[`, "subtab"))
    if (length(subtabs) > 1 && !is.null(subtab)) {
      entries <- Filter(function(p) p$subtab == subtab, entries)
    }

    # Build UI elements
    ui_list <- lapply(entries, function(p) {
      if (identical(p$type, "display")) {
        # Read-only display
        tags$div(style = "margin-bottom:8px; padding:6px 10px; background:#1f2937; border-radius:4px;",
          tags$span(style = "color:#9ca3af; font-size:12px;", p$label),
          tags$span(style = "color:#e5e7eb; font-weight:bold; margin-left:8px;", as.character(p$default)),
          if (nchar(p$tip) > 0) tags$span(style = "color:#6b7280; font-size:11px; margin-left:8px;",
                                           paste0("(", p$tip, ")"))
        )
      } else if (identical(p$type, "slider")) {
        current_val <- wi_rv$current_params[[p$id]]
        if (is.null(current_val)) current_val <- p$default
        # Format label with unit
        lbl <- if (!is.null(p$unit) && nchar(p$unit) > 0 && p$unit != "") {
          paste0(p$label, " (", p$unit, ")")
        } else p$label
        slider_el <- sliderInput(
          inputId = paste0("wi_p_", p$id),
          label   = lbl,
          min     = p$min,
          max     = p$max,
          step    = p$step,
          value   = current_val
        )
        slider_el
      } else if (identical(p$type, "select")) {
        current_val <- wi_rv$current_params[[p$id]]
        if (is.null(current_val)) current_val <- p$default
        lbl <- if (!is.null(p$unit) && nchar(p$unit) > 0 && p$unit != "") {
          paste0(p$label, " (", p$unit, ")")
        } else p$label
        selectInput(
          inputId  = paste0("wi_p_", p$id),
          label    = lbl,
          choices  = p$choices,
          selected = current_val
        )
      }
    })

    # Arrange in 2-column layout for sliders, single column for display
    if (length(ui_list) <= 3) {
      do.call(tagList, ui_list)
    } else {
      # 2-column grid
      n <- length(ui_list)
      left  <- ui_list[seq(1, n, by = 2)]
      right <- ui_list[seq(2, n, by = 2)]
      fluidRow(
        column(6, do.call(tagList, left)),
        column(6, do.call(tagList, right))
      )
    }
  })

  # ---- Sync slider values back to wi_rv$current_params ----
  # Use individual observeEvent per parameter to avoid a reactive dependency
  # on ALL inputs at once (which causes a feedback loop with renderUI and
  # makes sliders visually jitter).
  local({
    adjustable <- Filter(function(p) p$type %in% c("slider", "select"), policy_manifest)
    for (p in adjustable) {
      local({
        .p <- p
        .input_id <- paste0("wi_p_", .p$id)
        observeEvent(input[[.input_id]], {
          val <- input[[.input_id]]
          if (!is.null(val)) {
            if (.p$type == "select") val <- as.integer(val)
            wi_rv$current_params[[.p$id]] <- val
          }
        }, ignoreInit = TRUE)
      })
    }
  })

  # thin_file_reentry_delinq_gate is now a standard manifest slider —
  # no custom gate UI sync needed (handled by the generic slider observer)

  # ---- Cascade impact badges ----
  output$wi_cascade_badges <- renderUI({
    params <- reactiveValuesToList(wi_rv)$current_params
    if (is.null(params)) return(NULL)
    # Compare against active policy baseline, not manifest defaults.
    defaults <- build_default_params(policy_manifest)
    .active_cb <- get_active_policy_id(input$wi_product %||% "Personal Loan")
    .so_cb <- get0("strategy_overrides", envir = .GlobalEnv, inherits = FALSE)
    if (!is.null(.so_cb) && !is.null(.so_cb[[.active_cb]])) {
      defaults <- modifyList(defaults, .so_cb[[.active_cb]]$params)
    }
    impact <- compute_cascade_impact(params, defaults, policy_manifest)

    if (impact$impact == "none") return(NULL)

    badge_color <- switch(impact$impact,
      high   = "#dc2626",
      medium = "#f59e0b",
      low    = "#10b981",
      "#6b7280"
    )

    layer_chips <- lapply(impact$all_layers, function(l) {
      is_direct <- l %in% impact$direct_layers
      tags$span(
        style = sprintf(
          "display:inline-block; padding:2px 8px; margin:2px; border-radius:12px; font-size:11px; %s",
          if (is_direct) sprintf("background:%s; color:white;", badge_color)
          else "background:#374151; color:#9ca3af;"
        ),
        layer_names[as.character(l)]
      )
    })

    n_changed <- length(impact$changed_ids)
    tags$div(style = "margin-top:10px; padding:8px 12px; background:#111827; border-radius:6px;",
      tags$span(
        style = sprintf("color:%s; font-weight:bold; font-size:12px;", badge_color),
        icon("circle"),
        sprintf(" %s impact \u2014 %d param%s changed",
                toupper(impact$impact), n_changed,
                if (n_changed == 1) "" else "s")
      ),
      tags$div(style = "margin-top:4px;", layer_chips)
    )
  })

  # ---- Validation messages ----
  output$wi_validation_msgs <- renderUI({
    params <- reactiveValuesToList(wi_rv)$current_params
    if (is.null(params)) return(NULL)
    v <- validate_params(params, policy_manifest)

    msgs <- list()
    if (length(v$errors) > 0) {
      msgs <- c(msgs, lapply(v$errors, function(e) {
        tags$div(style = "color:#dc2626; font-size:12px; padding:2px 0;",
                 icon("times-circle"), " ", e)
      }))
    }
    if (length(v$warnings) > 0) {
      msgs <- c(msgs, lapply(v$warnings, function(w) {
        tags$div(style = "color:#f59e0b; font-size:12px; padding:2px 0;",
                 icon("exclamation-triangle"), " ", w)
      }))
    }
    if (length(msgs) == 0) return(NULL)
    tags$div(style = "margin-top:8px; padding:8px 12px; background:#111827; border-radius:6px;",
      do.call(tagList, msgs)
    )
  })

  # ---- Segment Rules: toggle panel ----
  observeEvent(input$wi_toggle_rules, {
    toggle("wi_rules_panel")
  })

  # ---- Cutoff Explorer: toggle panel ----
  observeEvent(input$wi_toggle_cutoff, {
    toggle("wi_cutoff_panel")
  })

  # ---- Cutoff Explorer: 2+ rate vs variable plot ----
  # For issued loans only, bin the chosen variable into ~10 quantile buckets
  # and compute the chosen 2+ rate definition per bucket. The result is a line
  # showing where a cutoff would do the most work — the user picks a variable
  # surfaced by the Driver Agent and reads off the spike.
  output$wi_cutoff_plot <- plotly::renderPlotly({
    req(input$wi_cutoff_var, input$wi_cutoff_mob)
    df <- credit_data
    if (is.null(df) || !nrow(df)) return(plotly::plot_ly())

    # Only issued loans have realised performance.
    if ("issued_flag" %in% names(df)) {
      df <- df[!is.na(df$issued_flag) & df$issued_flag == 1, , drop = FALSE]
    }
    var <- input$wi_cutoff_var
    if (!var %in% names(df)) return(plotly::plot_ly())
    vals <- df[[var]]
    keep <- !is.na(vals)
    df   <- df[keep, , drop = FALSE]
    vals <- vals[keep]
    if (length(vals) < 50) return(plotly::plot_ly())

    # 2+ target vector — mirrors Driver Agent definitions.
    mob <- input$wi_cutoff_mob
    if (mob == "lifetime") {
      target <- as.numeric(df$performance_delinquency_flag %||% 0)
    } else {
      mob_map <- c(bad_3mob = 3L, bad_6mob = 6L, bad_9mob = 9L, bad_12mob = 12L)
      m <- mob_map[[mob]]
      target <- as.numeric(!is.na(df$first_delinquency_mob) &
                           df$first_delinquency_mob <= m)
    }

    # Quantile bins. Numeric-typed variables only (rule vars are all numeric
    # except a couple of categorical ones — fall back to grouping by value).
    n_unique <- length(unique(vals))
    if (n_unique <= 8) {
      grp <- factor(vals)
    } else {
      brks <- unique(stats::quantile(vals, probs = seq(0, 1, length.out = 11),
                                     na.rm = TRUE))
      if (length(brks) < 3) return(plotly::plot_ly())
      grp <- cut(vals, breaks = brks, include.lowest = TRUE)
    }

    agg <- data.frame(grp = grp, target = target, val = vals,
                      stringsAsFactors = FALSE)
    agg <- agg[!is.na(agg$grp), , drop = FALSE]
    by_bin <- do.call(rbind, lapply(split(agg, agg$grp), function(s) {
      data.frame(
        bin   = as.character(s$grp[1]),
        x_mid = if (is.numeric(s$val)) stats::median(s$val, na.rm = TRUE)
                else NA_real_,
        rate  = mean(s$target, na.rm = TRUE),
        n     = nrow(s),
        stringsAsFactors = FALSE
      )
    }))
    by_bin <- by_bin[order(by_bin$x_mid), , drop = FALSE]

    var_label <- {
      meta <- Filter(function(v) v$id == var, wi_rule_vars)
      if (length(meta)) meta[[1]]$label else var
    }
    mob_label <- switch(mob,
      bad_3mob  = "2+ Rate @3 MOB",
      bad_6mob  = "2+ Rate @6 MOB",
      bad_9mob  = "2+ Rate @9 MOB",
      bad_12mob = "2+ Rate @12 MOB",
      lifetime  = "2+ Rate (Lifetime)")

    plotly::plot_ly(by_bin, x = ~x_mid, y = ~rate, type = "scatter",
                    mode = "lines+markers",
                    marker = list(color = "#fb6e36", size = 8),
                    line   = list(color = "#fb6e36", width = 2),
                    customdata = ~n,
                    hovertemplate = paste0(
                      "<b>", var_label, "</b>: %{x}<br>",
                      mob_label, ": %{y:.1%}<br>",
                      "N = %{customdata:,}<extra></extra>")) |>
      plotly::layout(
        title = list(text = paste(mob_label, "vs", var_label),
                     font = list(size = 13)),
        xaxis = list(title = var_label),
        yaxis = list(title = mob_label, tickformat = ".1%"),
        margin = list(l = 60, r = 20, t = 40, b = 50),
        paper_bgcolor = "white", plot_bgcolor = "white"
      ) |>
      plotly::config(displayModeBar = FALSE)
  })

  # ---- Manual Rules: dynamic value input for condition 1 ----
  output$wi_seg_val1_ui <- renderUI({
    req(input$wi_seg_dim1)
    var_meta <- Filter(function(v) v$id == input$wi_seg_dim1, wi_rule_vars)
    if (length(var_meta) == 0) return(NULL)
    var_meta <- var_meta[[1]]
    if (var_meta$type == "numeric") {
      col_data <- credit_data[[var_meta$id]]
      col_data <- col_data[!is.na(col_data)]
      numericInput("wi_seg_val1", "Value",
        value = round(median(col_data), 2),
        min = round(min(col_data), 2),
        max = round(max(col_data), 2))
    } else {
      vals <- sort(unique(as.character(credit_data[[var_meta$id]])))
      vals <- vals[!is.na(vals)]
      selectInput("wi_seg_val1", "Value", choices = vals)
    }
  })

  # ---- Manual Rules: dynamic value input for condition 2 ----
  output$wi_seg_val2_ui <- renderUI({
    req(input$wi_seg_dim2)
    var_meta <- Filter(function(v) v$id == input$wi_seg_dim2, wi_rule_vars)
    if (length(var_meta) == 0) return(NULL)
    var_meta <- var_meta[[1]]
    if (var_meta$type == "numeric") {
      col_data <- credit_data[[var_meta$id]]
      col_data <- col_data[!is.na(col_data)]
      numericInput("wi_seg_val2", "Value",
        value = round(median(col_data), 2),
        min = round(min(col_data), 2),
        max = round(max(col_data), 2))
    } else {
      vals <- sort(unique(as.character(credit_data[[var_meta$id]])))
      vals <- vals[!is.na(vals)]
      selectInput("wi_seg_val2", "Value", choices = vals)
    }
  })

  # ---- Manual Rules: update operator choices based on variable type ----
  observeEvent(input$wi_seg_dim1, {
    var_meta <- Filter(function(v) v$id == input$wi_seg_dim1, wi_rule_vars)
    if (length(var_meta) > 0 && var_meta[[1]]$type == "categorical") {
      updateSelectInput(session, "wi_seg_op1", choices = "==", selected = "==")
    } else {
      updateSelectInput(session, "wi_seg_op1",
        choices = c("<", "<=", ">", ">=", "=="), selected = "<")
    }
  })

  observeEvent(input$wi_seg_dim2, {
    var_meta <- Filter(function(v) v$id == input$wi_seg_dim2, wi_rule_vars)
    if (length(var_meta) > 0 && var_meta[[1]]$type == "categorical") {
      updateSelectInput(session, "wi_seg_op2", choices = "==", selected = "==")
    } else {
      updateSelectInput(session, "wi_seg_op2",
        choices = c("<", "<=", ">", ">=", "=="), selected = "<")
    }
  })

  # ---- Segment Rules: toggle second condition ----
  observeEvent(input$wi_seg_add_cond, {
    showElement("wi_seg_cond2_row")
  })
  observeEvent(input$wi_seg_rm_cond, {
    hideElement("wi_seg_cond2_row")
  })


  # ---- Manual Rules: add rule ----
  observeEvent(input$wi_seg_add_rule, {
    req(input$wi_seg_dim1, input$wi_seg_val1, input$wi_seg_action)

    # Build condition 1
    var1_meta <- Filter(function(v) v$id == input$wi_seg_dim1, wi_rule_vars)
    var1_label <- if (length(var1_meta) > 0) var1_meta[[1]]$label else input$wi_seg_dim1
    var1_type  <- if (length(var1_meta) > 0) var1_meta[[1]]$type else "categorical"

    val1 <- if (var1_type == "numeric") as.numeric(input$wi_seg_val1) else input$wi_seg_val1
    op1  <- .wi_normalize_op(input$wi_seg_op1)
    if (is.na(op1)) {
      showNotification("Invalid operator — defaulting to '<'.",
                       type = "warning", duration = 3)
      op1 <- "<"
    }

    cond_str <- paste0(var1_label, " ", op1, " ", val1)

    conditions <- list(
      dim1  = input$wi_seg_dim1,
      op1   = op1,
      val1  = val1,
      type1 = var1_type,
      logic = NULL,
      dim2  = NULL,
      op2   = NULL,
      val2  = NULL,
      type2 = NULL
    )

    # Check if AND condition is visible and populated
    cond2_visible <- tryCatch(shinyjs::is.visible("wi_seg_cond2_row"), error = function(e) FALSE)
    if (cond2_visible && !is.null(input$wi_seg_dim2) && !is.null(input$wi_seg_val2) &&
        nchar(as.character(input$wi_seg_val2 %||% "")) > 0) {
      var2_meta <- Filter(function(v) v$id == input$wi_seg_dim2, wi_rule_vars)
      var2_label <- if (length(var2_meta) > 0) var2_meta[[1]]$label else input$wi_seg_dim2
      var2_type  <- if (length(var2_meta) > 0) var2_meta[[1]]$type else "categorical"

      val2 <- if (var2_type == "numeric") as.numeric(input$wi_seg_val2) else input$wi_seg_val2
      op2  <- .wi_normalize_op(input$wi_seg_op2)
      if (is.na(op2)) op2 <- "<"

      logic_op <- input$wi_seg_logic %||% "AND"
      conditions$logic <- logic_op
      conditions$dim2  <- input$wi_seg_dim2
      conditions$op2   <- op2
      conditions$val2  <- val2
      conditions$type2 <- var2_type

      cond_str <- paste0(cond_str, " ", logic_op, " ", var2_label, " ", op2, " ", val2)
    }

    # Action info
    action <- input$wi_seg_action
    bps <- 0
    elasticity <- 5
    adverse_sel <- 5
    if (action == "Pricing") {
      bps <- if (input$wi_seg_pricing_bps == "Custom") {
        suppressWarnings(as.numeric(input$wi_seg_custom_bps))
      } else {
        suppressWarnings(as.numeric(input$wi_seg_pricing_bps))
      }
      if (is.na(bps)) bps <- 100
      # Grade-weighted elasticity from consultant config
      # Build segment mask safely from structured condition data (no eval/parse)
      seg_mask <- .wi_build_col_mask(credit_data, input$wi_seg_dim1, op1, val1, var1_type)
      if (!is.null(conditions$logic) && !is.null(conditions$dim2)) {
        mask2 <- .wi_build_col_mask(credit_data, conditions$dim2, conditions$op2,
                                     conditions$val2, conditions$type2)
        if (conditions$logic == "AND") {
          seg_mask <- seg_mask & mask2
        } else {
          seg_mask <- seg_mask | mask2
        }
      }
      seg_grades <- credit_data$risk_grade[seg_mask & credit_data$issued_flag == 1]

      # Pass the primary segmentation variable so the elasticity waterfall's
      # Layer 1 (V4 direct test/control) can fire when the segment is built
      # around an attribute the bank has actually price-tested. If only one
      # dimension is used, that's primary_var; if two are AND-combined we use
      # dim1; OR-combined → leave NA (no single dominant variable).
      primary_var <- if (is.null(conditions$logic) ||
                         identical(conditions$logic, "AND")) {
        as.character(input$wi_seg_dim1)
      } else {
        NA_character_
      }
      elasticity  <- get_segment_assumption(seg_grades, "elasticity",
                                            primary_var = primary_var)
      adverse_sel <- get_segment_assumption(seg_grades, "adverse_selection")
    }

    action_str <- if (action == "Decline") "Decline" else paste0("+", bps, " BPS")

    rule <- list(
      conditions  = conditions,
      cond_str    = cond_str,
      action      = action,
      action_str  = action_str,
      bps         = bps,
      elasticity  = elasticity,
      adverse_sel = adverse_sel
    )

    wi_rv$segment_rules <- c(wi_rv$segment_rules, list(rule))
  })

  # ---- Segment Rules: remove selected rule ----
  observeEvent(input$wi_seg_rm_rule, {
    sel <- input$wi_seg_rules_table_rows_selected
    if (!is.null(sel) && length(sel) > 0 && sel <= length(wi_rv$segment_rules)) {
      wi_rv$segment_rules <- wi_rv$segment_rules[-sel]
    }
  })

  # ---- Manual Rules: render active rules table ----
  output$wi_seg_rules_table <- DT::renderDT({
    rules <- wi_rv$segment_rules
    if (length(rules) == 0) {
      tbl <- data.frame(`#` = integer(0), Condition = character(0),
                        Action = character(0),
                        check.names = FALSE, stringsAsFactors = FALSE)
    } else {
      tbl <- data.frame(
        `#`        = seq_along(rules),
        Condition  = sapply(rules, `[[`, "cond_str"),
        Action     = sapply(rules, `[[`, "action_str"),
        check.names = FALSE, stringsAsFactors = FALSE
      )
    }
    DT::datatable(tbl, rownames = FALSE, selection = "single",
      options = list(dom = "t", pageLength = 10,
        initComplete = JS(
          "function(s,j){",
          "$(this.api().table().header()).css(",
          "  {'background-color':'#fb6e36','color':'white'});",
          "}"
        )))
  })

  # ---- Helper: restore slider UI from params ----
  restore_params_to_ui <- function(params, bps) {
    wi_rv$current_params <- params
    wi_rv$bps_adj <- bps
    lbl <- if (bps >= 0) paste0("+", bps, " bps") else paste0(bps, " bps")
    shinyjs::html("wi_bps_display", lbl)
    current_grp <- input$wi_group
    updateRadioGroupButtons(session, "wi_group", selected = current_grp)
  }

  # ---- Slot switch: restore saved params or reset to baseline ----
  observeEvent(input$wi_sim_slot, {
    slot <- input$wi_sim_slot
    saved <- wi_rv[[paste0("sim_", slot)]]
    if (!is.null(saved) && !is.null(saved$params)) {
      restore_params_to_ui(saved$params, saved$bps_adj %||% 0L)
      wi_rv$segment_rules <- saved$rules %||% list()
      showNotification(
        paste0("Restored ", if (slot == "A") "Primary" else "Alternative",
               " scenario parameters."),
        type = "message", duration = 2)
    } else {
      restore_params_to_ui(wi_pl_current_params(), 0L)
      wi_rv$segment_rules  <- list()
      showNotification(
        paste0("Starting fresh for ", if (slot == "A") "Primary" else "Alternative",
               " scenario."),
        type = "message", duration = 2)
    }
  }, ignoreInit = TRUE)

  # ---- Reset to Baseline ----
  observeEvent(input$wi_reset, {
    # Reset to PL-CURRENT (active baseline), not the manifest disabled sentinels
    wi_rv$current_params  <- wi_pl_current_params()
    wi_rv$segment_rules   <- list()
    wi_rv$scenario_df     <- NULL
    wi_rv$scenario_funnel <- NULL
    wi_rv$scenario_full   <- NULL
    wi_rv$baseline_cd     <- NULL
    wi_rv$baseline_full   <- NULL
    wi_rv$bps_adj         <- 0L
    wi_rv$sim_A           <- NULL
    wi_rv$sim_B           <- NULL
    hideElement("wi_results_panel")
    hideElement("wi_sample_preview_panel")
    shinyjs::html("wi_bps_display", "+0 bps")
    # thin_file_reentry_delinq_gate reset handled by generic slider reset loop
    current_grp <- input$wi_group
    updateRadioGroupButtons(session, "wi_group", selected = current_grp)
  })

  # ---- Quick Pricing: helper to shift all grade APRs ----
  wi_shift_apr <- function(delta) {
    params <- wi_rv$current_params
    # APR bounds from policy_manifest
    apr_bounds <- list(
      apr_high = c(min = 0.04, max = 0.15),
      apr_med  = c(min = 0.08, max = 0.22),
      apr_low  = c(min = 0.10, max = 0.25)
    )
    for (pid in names(apr_bounds)) {
      old_val <- params[[pid]]
      if (is.null(old_val)) next
      new_val <- max(apr_bounds[[pid]]["min"],
                     min(apr_bounds[[pid]]["max"], old_val + delta))
      params[[pid]] <- new_val
    }
    wi_rv$current_params <- params
    wi_rv$bps_adj <- wi_rv$bps_adj + as.integer(delta * 10000)
    # Update display
    adj <- wi_rv$bps_adj
    lbl <- if (adj >= 0) paste0("+", adj, " bps") else paste0(adj, " bps")
    shinyjs::html("wi_bps_display", lbl)
    # Re-render sliders if currently on Pricing group
    if (identical(input$wi_group, "Pricing")) {
      updateRadioGroupButtons(session, "wi_group", selected = "Pricing")
    }
  }

  observeEvent(input$wi_bps_minus, { wi_shift_apr(-0.0025) })
  observeEvent(input$wi_bps_plus,  { wi_shift_apr( 0.0025) })
  observeEvent(input$wi_bps_reset, {
    # Reset APRs to PL-CURRENT baseline (not manifest defaults)
    defaults <- wi_pl_current_params()
    params <- wi_rv$current_params
    for (pid in c("apr_high", "apr_med", "apr_low")) {
      params[[pid]] <- defaults[[pid]]
    }
    wi_rv$current_params <- params
    wi_rv$bps_adj <- 0L
    shinyjs::html("wi_bps_display", "+0 bps")
    if (identical(input$wi_group, "Pricing")) {
      updateRadioGroupButtons(session, "wi_group", selected = "Pricing")
    }
  })

  # ---- Simulate button ----
  observeEvent(input$wi_simulate, {
    params <- wi_rv$current_params
    req(params)

    # Pre-flight: validate
    v <- validate_params(params, policy_manifest)
    if (!v$valid) {
      showNotification(
        paste("Cannot simulate \u2014 fix errors:", paste(v$errors, collapse = "; ")),
        type = "error", duration = 8)
      return()
    }

    # ---- Show analysis pipeline ----
    pipeline_show(session, "wi_sim_pipeline", list(
      list(label = "Filtering population and applying scope constraints"),
      list(label = "Running baseline policy engine (full L10\u201390 cascade)"),
      list(label = "Running scenario policy engine with adjusted parameters"),
      list(label = "Applying segment-level rules (decline / reprice)"),
      list(label = "Computing impact metrics and funnel comparison")
    ))

    {
      # Apply population filters
      pop_keep <- rep(TRUE, nrow(credit_data))
      # Filter to active policy for the selected product
      if ("policy_id" %in% names(credit_data)) {
        active_pid <- get_active_policy_id(input$wi_product %||% "Personal Loan")
        pop_keep <- pop_keep & (credit_data$policy_id == active_pid)
      }
      # Product filter
      if (!is.null(input$wi_product))
        pop_keep <- pop_keep & (as.character(credit_data$product_type) == input$wi_product)
      # Date range filter
      if (!is.null(input$wi_date_range))
        pop_keep <- pop_keep & (is.na(credit_data$date) |
                   (credit_data$date >= input$wi_date_range[1] & credit_data$date <= input$wi_date_range[2]))
      # FICO filter
      if (!is.null(input$wi_fico) && !("all" %in% input$wi_fico) && length(input$wi_fico) > 0)
        pop_keep <- pop_keep & (as.character(credit_data$fico_band) %in% input$wi_fico)
      # Grade filter
      if (!is.null(input$wi_grade) && !("all" %in% input$wi_grade) && length(input$wi_grade) > 0)
        pop_keep <- pop_keep & (as.character(credit_data$risk_grade) %in% input$wi_grade)
      # Partner filter
      if (!is.null(input$wi_partner) && !("all" %in% input$wi_partner) && length(input$wi_partner) > 0)
        pop_keep <- pop_keep & (as.character(credit_data$partner_channel) %in% input$wi_partner)
      # Repeat type filter
      if (!is.null(input$wi_repeat) && !("all" %in% input$wi_repeat) && length(input$wi_repeat) > 0)
        pop_keep <- pop_keep & (as.character(credit_data$borrower_repeat_type) %in% input$wi_repeat)
      # MOB filter
      if (!is.null(input$wi_mob) && !("all" %in% input$wi_mob) && length(input$wi_mob) > 0)
        pop_keep <- pop_keep & (as.character(credit_data$mob_bins) %in% input$wi_mob)

      # Engine ALWAYS runs on the full filtered pool so the funnel reads the
      # true Apps -> Approved -> Issued pipeline. The data-scope toggle only
      # narrows the cohort used for downstream risk/yield metrics (Avg APR,
      # 2+ Rate, IRR), which require realized performance data. Scope is
      # captured here and applied after the take-up simulation below.
      scope_sel <- input$wi_data_scope %||% "issued"
      cd_filtered  <- credit_data[pop_keep, , drop = FALSE]
      pid_filtered <- policy_input_data[pop_keep, , drop = FALSE]
      historical_issued <- if ("issued_flag" %in% names(cd_filtered)) {
        as.logical(cd_filtered$issued_flag == TRUE | cd_filtered$issued_flag == 1)
      } else {
        rep(FALSE, nrow(cd_filtered))
      }
      historical_issued[is.na(historical_issued)] <- FALSE

      pipeline_step_done(session, "wi_sim_pipeline", 0L,
        paste0(format(nrow(pid_filtered), big.mark = ","), " applications in scope"))

      # Build symmetric baseline: fresh engine run with active policy params.
      default_params <- build_default_params(policy_manifest)
      active_pid_bl <- get_active_policy_id(input$wi_product %||% "Personal Loan")
      .so_bl <- get0("strategy_overrides", envir = .GlobalEnv, inherits = FALSE)
      if (!is.null(.so_bl) && !is.null(.so_bl[[active_pid_bl]])) {
        default_params <- modifyList(default_params, .so_bl[[active_pid_bl]]$params)
      }
      baseline_df <- apply_policy_with_params(pid_filtered, default_params)

      pipeline_step_done(session, "wi_sim_pipeline", 1L,
        paste0(sum(baseline_df$offered_decision_flag == "APPR"), " baseline approvals"))

      # Pass 1: Global simulation (scenario)
      scenario_df <- apply_policy_with_params(pid_filtered, params)

      pipeline_step_done(session, "wi_sim_pipeline", 2L,
        paste0(sum(scenario_df$offered_decision_flag == "APPR"), " scenario approvals"))

      # Carry over historical/performance/display columns from credit_data into both
      carry_cols <- c("issued_flag", "issued_amount", "performance_delinquency_flag",
                     "booking_vintage", "fico_band",
                     "partner_channel", "borrower_repeat_type", "pricing_test_variant",
                     "mob_bins", "date")
      for (col in carry_cols) {
        if (col %in% names(cd_filtered)) {
          if (!(col %in% names(baseline_df))) baseline_df[[col]] <- cd_filtered[[col]]
          if (!(col %in% names(scenario_df))) scenario_df[[col]] <- cd_filtered[[col]]
        }
      }

      # ---- Baseline-owned policy outputs (from engine, not pre-baked) ----
      # issued_flag = (historically issued) AND (this policy approves). The AND
      # gate prevents engine drift — when the parameterised engine cannot
      # reproduce a historical APPR decision, the loan would otherwise show as
      # issued-but-not-approved (Take Rate > 100%, Issued > Approved). The
      # gate keeps the funnel internally consistent.
      baseline_df$issued_flag <- as.integer(
        cd_filtered$issued_flag == 1L &
        !is.na(baseline_df$offered_decision_flag) &
        baseline_df$offered_decision_flag == "APPR"
      )
      baseline_df$interest_rate_apr <- ifelse(
        baseline_df$offered_decision_flag == "APPR",
        baseline_df$offered_final_apr,
        cd_filtered$interest_rate_apr
      )

      # ---- Scenario-owned policy outputs ----
      # Same AND gate as baseline: issued only if historically issued AND the
      # scenario approves. This subsumes the previous "newly declined" zeroing
      # (baseline APPR → scenario DECL) and also handles the engine-drift case
      # (baseline DECL but historically issued).
      scenario_df$issued_flag <- as.integer(
        cd_filtered$issued_flag == 1L &
        !is.na(scenario_df$offered_decision_flag) &
        scenario_df$offered_decision_flag == "APPR"
      )

      # (b) Simulate take-up for newly eligible loans
      #     (baseline DECL → scenario APPR; historical issued = 0 because never funded)
      newly_eligible <- !is.na(scenario_df$offered_decision_flag) &
        scenario_df$offered_decision_flag == "APPR" &
        !is.na(baseline_df$offered_decision_flag) &
        baseline_df$offered_decision_flag != "APPR"

      if (sum(newly_eligible, na.rm = TRUE) > 0) {
        bl_appr_mask <- !is.na(baseline_df$offered_decision_flag) &
          baseline_df$offered_decision_flag == "APPR"
        grade_take <- tapply(
          cd_filtered$issued_flag[bl_appr_mask] == 1L,
          baseline_df$risk_grade[bl_appr_mask],
          mean, na.rm = TRUE
        )
        overall_take <- mean(cd_filtered$issued_flag[bl_appr_mask] == 1L, na.rm = TRUE)

        bl_issued_mask <- bl_appr_mask & cd_filtered$issued_flag == 1L
        grade_del <- tapply(
          cd_filtered$performance_delinquency_flag[bl_issued_mask],
          baseline_df$risk_grade[bl_issued_mask],
          mean, na.rm = TRUE
        )

        new_idx <- which(newly_eligible)
        new_grades <- scenario_df$risk_grade[new_idx]
        for (g in unique(new_grades[!is.na(new_grades)])) {
          g_pos   <- which(new_grades == g & !is.na(new_grades))
          g_idx   <- new_idx[g_pos]
          g_rate  <- if (g %in% names(grade_take)) grade_take[[g]] else overall_take
          n_take  <- round(length(g_idx) * g_rate)
          if (n_take > 0) {
            fico_ord <- order(scenario_df$fico_score[g_idx], decreasing = TRUE)
            take_idx <- g_idx[fico_ord[seq_len(min(n_take, length(g_idx)))]]
            scenario_df$issued_flag[take_idx]   <- 1L
            scenario_df$issued_amount[take_idx]  <- scenario_df$maximum_offered_amount[take_idx]
            g_del <- if (g %in% names(grade_del)) grade_del[[g]] else 0
            n_del <- round(length(take_idx) * g_del)
            if (n_del > 0) {
              del_ord <- order(scenario_df$fico_score[take_idx])
              scenario_df$performance_delinquency_flag[take_idx[del_ord[seq_len(n_del)]]] <- 1L
            }
          }
        }
      }

      # (c) Pricing elasticity: APR increase on existing issued loans → some drop off
      # NOTE: get_segment_assumption() returns a grade-weighted SCALAR. Coerce
      # defensively via [1] so any future signature drift (e.g. returning a
      # per-loan vector) cannot silently break the elasticity/adverse-selection
      # math below — drop_probs and the `if (adv_pct > 0)` gate both assume
      # these are length-1 scalars.
      elast_pct <- as.numeric(get_segment_assumption(
        cd_filtered$risk_grade[cd_filtered$issued_flag == 1], "elasticity"))[1] / 100
      adv_pct   <- as.numeric(get_segment_assumption(
        cd_filtered$risk_grade[cd_filtered$issued_flag == 1], "adverse_selection"))[1] / 100
      if (is.na(elast_pct)) elast_pct <- 0
      if (is.na(adv_pct))   adv_pct   <- 0
      both_appr_issued <- !is.na(baseline_df$offered_decision_flag) &
        baseline_df$offered_decision_flag == "APPR" &
        !is.na(scenario_df$offered_decision_flag) &
        scenario_df$offered_decision_flag == "APPR" &
        cd_filtered$issued_flag == 1L
      if (sum(both_appr_issued, na.rm = TRUE) > 0) {
        apr_delta <- scenario_df$offered_final_apr[both_appr_issued] -
                     baseline_df$offered_final_apr[both_appr_issued]
        price_up <- !is.na(apr_delta) & apr_delta > 0.001
        if (sum(price_up) > 0) {
          up_idx   <- which(both_appr_issued)[price_up]
          up_delta <- apr_delta[price_up]
          drop_probs <- pmin(up_delta * 100 * elast_pct, 0.50)
          n_drop <- round(sum(drop_probs))
          if (n_drop > 0) {
            drop_ord <- order(up_delta, decreasing = TRUE)
            drop_idx <- up_idx[drop_ord[seq_len(min(n_drop, length(up_idx)))]]
            scenario_df$issued_flag[drop_idx] <- 0L
          }
        }
        # (d) Adverse selection: APR increase causes worst borrowers to stay
        if (adv_pct > 0 && any(price_up)) {
          bps_shift <- abs(wi_rv$bps_adj)
          adverse_adj <- (bps_shift / 100) * adv_pct
          still_issued <- both_appr_issued & scenario_df$issued_flag == 1L
          good_in_pool <- which(still_issued & scenario_df$performance_delinquency_flag == 0)
          n_flip <- round(length(good_in_pool) * adverse_adj)
          if (n_flip > 0 && length(good_in_pool) > 0) {
            set.seed(123)
            fico_ord <- order(scenario_df$fico_score[good_in_pool])
            flip_idx <- good_in_pool[fico_ord[seq_len(min(n_flip, length(good_in_pool)))]]
            scenario_df$performance_delinquency_flag[flip_idx] <- 1L
          }
        }
      }

      # interest_rate_apr: use scenario APR for approved, baseline APR for unchanged
      scenario_df$interest_rate_apr <- ifelse(
        scenario_df$offered_decision_flag == "APPR",
        scenario_df$offered_final_apr,
        baseline_df$interest_rate_apr
      )

      # (e) Cap issued_amount at scenario's maximum_offered_amount
      # When loan limits are tightened (e.g. exposure cap reduced), loans that
      # remain issued should reflect the lower offered amount.
      still_issued_sc <- scenario_df$issued_flag == 1L &
        !is.na(scenario_df$maximum_offered_amount)
      scenario_df$issued_amount[still_issued_sc] <- pmin(
        scenario_df$issued_amount[still_issued_sc],
        scenario_df$maximum_offered_amount[still_issued_sc])

      # Symmetric cap for baseline so comparison is apples-to-apples
      still_issued_bl <- baseline_df$issued_flag == 1L &
        !is.na(baseline_df$maximum_offered_amount)
      baseline_df$issued_amount[still_issued_bl] <- pmin(
        baseline_df$issued_amount[still_issued_bl],
        baseline_df$maximum_offered_amount[still_issued_bl])

      # Pass 2: Manual rules (Decline / Pricing actions)
      rules <- wi_rv$segment_rules
      if (length(rules) > 0) {
        incProgress(0.3, detail = paste("Applying", length(rules), "manual rule(s)"))
        for (rule in rules) {
          mask <- evaluate_segment_conditions(scenario_df, rule$conditions)
          if (sum(mask) == 0) next

          if (rule$action == "Decline") {
            # Decline targets all APPROVED applicants in the segment
            # (not just issued — user is adding a new decline rule)
            to_decline <- mask &
              !is.na(scenario_df$offered_decision_flag) &
              scenario_df$offered_decision_flag == "APPR"
            scenario_df$offered_decision_flag[to_decline] <- "DECL"
            scenario_df$issued_flag[to_decline] <- 0L
          } else if (rule$action == "Pricing" && rule$bps != 0) {
            # Pricing: bump/cut the policy-derived APR (adjusted_apr_pre_term_cap)
            # for all approved loans in the segment. Do NOT use interest_rate_apr
            # (original CSV column) — it has a different base than the policy APR.
            in_segment_issued <- mask & (scenario_df$issued_flag == 1L)
            to_price <- mask &
              !is.na(scenario_df$offered_decision_flag) &
              scenario_df$offered_decision_flag == "APPR"
            scenario_df$adjusted_apr_pre_term_cap[to_price] <-
              scenario_df$adjusted_apr_pre_term_cap[to_price] + rule$bps / 10000

            # Replay downstream policy layers with user's decline thresholds
            scenario_df <- replay_layers_50_80_90(scenario_df,
              fico_min = params$fico_min %||% 300, fico_max = params$fico_max %||% 850,
              thin_file_mo = params$thin_file_months %||% 30,
              delinq_thresh = params$delinquency_max %||% 1,
              dti_thresh = params$dti_cutoff %||% 0.50)

            # Re-sync interest_rate_apr from offered_final_apr so KPIs
            # and sample preview reflect the repriced APR
            scenario_df$interest_rate_apr <- ifelse(
              scenario_df$offered_decision_flag == "APPR",
              scenario_df$offered_final_apr,
              scenario_df$interest_rate_apr)

            # Sync issued_flag: newly declined loans can no longer be issued
            newly_declined <- scenario_df$offered_decision_flag != "APPR" &
                              baseline_df$offered_decision_flag == "APPR"
            scenario_df$issued_flag[newly_declined] <- 0L

            in_seg_issued <- in_segment_issued & baseline_df$issued_flag == 1
            n_seg_issued  <- sum(in_seg_issued, na.rm = TRUE)
            elast_pct <- (rule$elasticity %||% 5) / 100
            adv_pct   <- (rule$adverse_sel %||% 5) / 100

            if (rule$bps > 0) {
              # PRICE UP: rate-sensitive takers walk away
              drop_rate <- min((rule$bps / 100) * elast_pct, 0.50)
              n_drop    <- round(n_seg_issued * drop_rate)

              if (n_drop > 0 && n_seg_issued > 0) {
                seg_issued_idx <- which(in_seg_issued)
                fico_order <- order(baseline_df$fico_score[seg_issued_idx],
                                    decreasing = TRUE)
                drop_idx <- seg_issued_idx[fico_order[seq_len(n_drop)]]
                scenario_df$issued_flag[drop_idx] <- 0L
              }

              # Adverse selection: remaining pool enriched with risk
              if (adv_pct > 0) {
                adverse_adj <- (rule$bps / 100) * adv_pct
                still_issued <- mask & scenario_df$issued_flag == 1L
                good_in_seg  <- which(still_issued &
                  scenario_df$performance_delinquency_flag == 0)
                n_flip <- round(length(good_in_seg) * adverse_adj)
                if (n_flip > 0 && length(good_in_seg) > 0) {
                  set.seed(123)
                  fico_ord <- order(scenario_df$fico_score[good_in_seg])
                  flip_idx <- good_in_seg[fico_ord[seq_len(min(n_flip, length(good_in_seg)))]]
                  scenario_df$performance_delinquency_flag[flip_idx] <- 1L
                }
              }
            } else if (rule$bps < 0) {
              # PRICE DOWN: positive selection — some marginal bad loans
              # would not have occurred at the better price. Flip
              # lowest-FICO bad loans to good within the segment.
              # Matches Scanner's tab_smart_recommender.R:1225-1243 exactly.
              # No volume capture here — swap-in of new volume stays in
              # Opportunity Finder.
              if (adv_pct > 0) {
                positive_adj <- (abs(rule$bps) / 100) * adv_pct
                still_issued <- mask & scenario_df$issued_flag == 1L
                bad_in_seg   <- which(still_issued &
                  scenario_df$performance_delinquency_flag == 1)
                n_rescue <- round(length(bad_in_seg) * positive_adj)
                if (n_rescue > 0 && length(bad_in_seg) > 0) {
                  set.seed(123)
                  fico_ord <- order(scenario_df$fico_score[bad_in_seg],
                                    decreasing = TRUE)
                  rescue_idx <- bad_in_seg[fico_ord[seq_len(min(n_rescue, length(bad_in_seg)))]]
                  scenario_df$performance_delinquency_flag[rescue_idx] <- 0L
                }
              }
            }
          }
        }
      }

      pipeline_step_done(session, "wi_sim_pipeline", 3L,
        paste0(length(wi_rv$segment_rules), " rule(s) applied"))

      # Identify newly-eligible loan IDs (for simulated vintage curves)
      new_loan_ids <- character(0)
      if (exists("newly_eligible") && any(newly_eligible, na.rm = TRUE)) {
        take_mask <- newly_eligible & scenario_df$issued_flag == 1L
        new_loan_ids <- as.character(scenario_df$loan_id[take_mask])
      }

      # Apply data-scope filter for risk/yield metrics. The full-pool engine
      # outputs above feed the funnel (Apps/Approved/Issued/Approval Rate/Take
      # Rate). The scope-filtered slices feed Avg APR, Avg Loan Size, 2+ Rate,
      # and IRR.
      scope_mask <- if (identical(scope_sel, "issued")) historical_issued
                    else rep(TRUE, nrow(baseline_df))
      baseline_scoped <- baseline_df[scope_mask, , drop = FALSE]
      scenario_scoped <- scenario_df[scope_mask, , drop = FALSE]

      # Store results. baseline_cd / scenario_df keep their existing
      # scope-filtered semantics so existing consumers (KPI tiles' risk
      # metrics, IRR chart, vintage curves) are unchanged. baseline_full /
      # scenario_full are the full-pool engine outputs used by the funnel
      # and the rate KPIs.
      wi_rv$baseline_cd     <- baseline_scoped
      wi_rv$scenario_df     <- scenario_scoped
      wi_rv$baseline_full   <- baseline_df
      wi_rv$scenario_full   <- scenario_df
      wi_rv$baseline_funnel <- .compute_funnel_merged(baseline_df, baseline_scoped)
      wi_rv$scenario_funnel <- .compute_funnel_merged(scenario_df, scenario_scoped)

      # Store into dual simulation slot
      slot <- input$wi_sim_slot %||% "A"
      # Build descriptive label from changed params and segment rules.
      # Compare against PL-CURRENT (active baseline), not the manifest disabled
      # sentinels — otherwise the label shows spurious "changes" for params
      # that already match PL-CURRENT (e.g. n_hard_inq_6m_max=4, gate=0).
      default_p <- wi_pl_current_params()
      changed_ids <- names(params)[vapply(names(params), function(nm) {
        v <- params[[nm]]; d <- default_p[[nm]]
        if (is.null(v) || is.null(d)) return(!is.null(v) || !is.null(d))
        if (is.numeric(v) && is.numeric(d)) return(abs(v - d) > 1e-9)
        !identical(v, d)
      }, logical(1))]
      # Map IDs to human-readable labels
      id_to_label <- setNames(
        vapply(policy_manifest, function(p) p$label, character(1)),
        vapply(policy_manifest, function(p) p$id, character(1))
      )
      changed_labels <- vapply(changed_ids, function(id) {
        if (id %in% names(id_to_label)) id_to_label[[id]] else id
      }, character(1), USE.NAMES = FALSE)
      n_rules <- length(wi_rv$segment_rules)
      label_parts <- c()
      if (length(changed_labels) > 0) {
        shown <- paste(head(changed_labels, 2), collapse = ", ")
        if (length(changed_labels) > 2) shown <- paste0(shown, " +", length(changed_labels) - 2)
        label_parts <- c(label_parts, shown)
      }
      if (n_rules > 0) label_parts <- c(label_parts, paste0(n_rules, " segment rule", if (n_rules > 1) "s"))
      wi_label <- if (length(label_parts) > 0) paste(label_parts, collapse = " | ") else "What-If Simulation"
      sim_data <- list(
        scenario_df    = scenario_scoped,
        baseline_cd    = baseline_scoped,
        scenario_full  = scenario_df,
        baseline_full  = baseline_df,
        params         = params,
        rules          = wi_rv$segment_rules,
        bps_adj        = wi_rv$bps_adj,
        new_loan_ids   = new_loan_ids,
        label          = wi_label
      )
      wi_rv[[paste0("sim_", slot)]] <- sim_data
    }

    # Populate performance filter choices
    sc <- wi_rv$scenario_df
    if (!is.null(sc)) {
      if ("fico_band" %in% names(sc)) {
        fb <- sort(unique(as.character(sc$fico_band[!is.na(sc$fico_band)])))
        updatePickerInput(session, "wi_perf_fico",
          choices = c("All" = "all", setNames(fb, fb)), selected = "all")
      }
      if ("partner_channel" %in% names(sc)) {
        pc <- sort(unique(as.character(sc$partner_channel[!is.na(sc$partner_channel)])))
        updatePickerInput(session, "wi_perf_partner",
          choices = c("All" = "all", setNames(pc, pc)), selected = "all")
      }
      if ("borrower_repeat_type" %in% names(sc)) {
        rt <- sort(unique(as.character(sc$borrower_repeat_type[!is.na(sc$borrower_repeat_type)])))
        updatePickerInput(session, "wi_perf_repeat",
          choices = c("All" = "all", setNames(rt, rt)), selected = "all")
      }
      if ("risk_grade" %in% names(sc)) {
        rg <- sort(unique(as.character(sc$risk_grade[!is.na(sc$risk_grade)])))
        updatePickerInput(session, "wi_perf_grade",
          choices = c("All" = "all", setNames(rg, rg)), selected = "all")
      }
      if ("mob_bins" %in% names(sc)) {
        mb <- sort(unique(as.character(sc$mob_bins[!is.na(sc$mob_bins)])))
        updatePickerInput(session, "wi_perf_mob",
          choices = c("All" = "all", setNames(mb, mb)), selected = "all")
      }
    }

    # ---- Show live flowchart modal ----
    # Use active policy as baseline so deltas reflect scenario vs active policy
    .fc_defaults <- build_default_params(policy_manifest)
    .fc_pid <- get_active_policy_id(input$wi_product %||% "Personal Loan")
    .so_fc <- get0("strategy_overrides", envir = .GlobalEnv, inherits = FALSE)
    if (!is.null(.so_fc) && !is.null(.so_fc[[.fc_pid]])) {
      .fc_defaults <- modifyList(.fc_defaults, .so_fc[[.fc_pid]]$params)
    }
    flow_html <- build_flowchart_html(
      params          = params,
      default_params  = .fc_defaults,
      segment_rules   = wi_rv$segment_rules,
      n_apps          = nrow(pid_filtered),
      baseline_funnel = wi_rv$baseline_funnel,
      scenario_funnel = wi_rv$scenario_funnel
    )

    showModal(modalDialog(
      title = tagList(
        tags$span(style = "color:#fb6e36;", icon("microchip")),
        tags$span(" Policy Engine Trace", style = "font-weight:bold; color:#f3f4f6;")
      ),
      flow_html,
      size = "m",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))

    pipeline_step_done(session, "wi_sim_pipeline", 4L, "impact computed")
    pipeline_complete(session, "wi_sim_pipeline")

    showElement("wi_results_panel")

    # ---- Build sample preview of affected loans ----
    bl <- wi_rv$baseline_cd
    sc <- wi_rv$scenario_df

    # Identify declined loans (baseline approved -> scenario declined)
    declined_idx <- which(
      !is.na(bl$offered_decision_flag) & bl$offered_decision_flag == "APPR" &
      !is.na(sc$offered_decision_flag) & sc$offered_decision_flag == "DECL"
    )

    # Identify repriced loans (approved in both, APR changed)
    repriced_idx <- which(
      !is.na(bl$offered_decision_flag) & bl$offered_decision_flag == "APPR" &
      !is.na(sc$offered_decision_flag) & sc$offered_decision_flag == "APPR" &
      !is.na(bl$interest_rate_apr) & !is.na(sc$interest_rate_apr) &
      abs(sc$interest_rate_apr - bl$interest_rate_apr) > 1e-6
    )

    # Identify amount-changed loans (approved in both, same APR, but offered amount changed)
    bl_amt <- if ("maximum_offered_amount" %in% names(bl)) bl$maximum_offered_amount
              else if ("offered_amount" %in% names(bl)) bl$offered_amount else NULL
    sc_amt <- if ("maximum_offered_amount" %in% names(sc)) sc$maximum_offered_amount
              else if ("offered_amount" %in% names(sc)) sc$offered_amount else NULL
    amt_changed_idx <- integer(0)
    if (!is.null(bl_amt) && !is.null(sc_amt)) {
      amt_changed_idx <- which(
        !is.na(bl$offered_decision_flag) & bl$offered_decision_flag == "APPR" &
        !is.na(sc$offered_decision_flag) & sc$offered_decision_flag == "APPR" &
        !is.na(bl_amt) & !is.na(sc_amt) &
        abs(sc_amt - bl_amt) > 0.01
      )
      # Exclude loans already in repriced_idx
      amt_changed_idx <- setdiff(amt_changed_idx, repriced_idx)
    }

    sample_rows <- NULL
    if (length(declined_idx) > 0) {
      decl_df <- data.frame(
        loan_id           = bl$loan_id[declined_idx],
        fico_score        = bl$fico_score[declined_idx],
        risk_grade        = bl$risk_grade[declined_idx],
        change_type       = "Declined",
        original_decision = "APPR",
        new_decision      = "DECL",
        original_apr      = round(bl$interest_rate_apr[declined_idx] * 100, 2),
        new_apr           = NA_real_,
        apr_change_bps    = NA_integer_,
        stringsAsFactors  = FALSE
      )
      sample_rows <- decl_df
    }
    if (length(repriced_idx) > 0) {
      price_df <- data.frame(
        loan_id           = bl$loan_id[repriced_idx],
        fico_score        = bl$fico_score[repriced_idx],
        risk_grade        = bl$risk_grade[repriced_idx],
        change_type       = "Repriced",
        original_decision = "APPR",
        new_decision      = "APPR",
        original_apr      = round(bl$interest_rate_apr[repriced_idx] * 100, 2),
        new_apr           = round(sc$interest_rate_apr[repriced_idx] * 100, 2),
        apr_change_bps    = round(
          (sc$interest_rate_apr[repriced_idx] - bl$interest_rate_apr[repriced_idx]) * 10000),
        stringsAsFactors  = FALSE
      )
      sample_rows <- if (is.null(sample_rows)) price_df else rbind(sample_rows, price_df)
    }
    if (length(amt_changed_idx) > 0) {
      amt_df <- data.frame(
        loan_id           = bl$loan_id[amt_changed_idx],
        fico_score        = bl$fico_score[amt_changed_idx],
        risk_grade        = bl$risk_grade[amt_changed_idx],
        change_type       = "Amount Changed",
        original_decision = "APPR",
        new_decision      = "APPR",
        original_apr      = round(bl$interest_rate_apr[amt_changed_idx] * 100, 2),
        new_apr           = round(sc$interest_rate_apr[amt_changed_idx] * 100, 2),
        apr_change_bps    = NA_integer_,
        stringsAsFactors  = FALSE
      )
      sample_rows <- if (is.null(sample_rows)) amt_df else rbind(sample_rows, amt_df)
    }

    if (!is.null(sample_rows) && nrow(sample_rows) > 0) {
      set.seed(42)
      sample_full <- head(sample_rows[sample(nrow(sample_rows)), ], 50)
      n_affected  <- nrow(sample_rows)
    } else {
      sample_full <- NULL
      n_affected  <- 0
    }

    # Store sample data into the simulation slot
    slot <- input$wi_sim_slot %||% "A"
    sim <- wi_rv[[paste0("sim_", slot)]]
    if (!is.null(sim)) {
      sim$sample_full <- sample_full
      sim$sample_all  <- sample_rows
      sim$n_affected  <- n_affected
      wi_rv[[paste0("sim_", slot)]] <- sim
    }

    # Show sample panel if ANY slot has data
    has_sample <- !is.null(wi_rv$sim_A$sample_full) || !is.null(wi_rv$sim_B$sample_full)
    if (has_sample) showElement("wi_sample_preview_panel")
    else hideElement("wi_sample_preview_panel")

  })

  # ---- Clear individual simulation slots ----
  wi_hide_if_both_cleared <- function() {
    if (is.null(wi_rv$sim_A) && is.null(wi_rv$sim_B)) {
      hideElement("wi_sample_preview_panel")
    } else if (is.null(wi_rv$sim_A$sample_full) && is.null(wi_rv$sim_B$sample_full)) {
      hideElement("wi_sample_preview_panel")
    }
  }
  observeEvent(input$wi_clear_A, {
    wi_rv$sim_A <- NULL
    wi_hide_if_both_cleared()
    showNotification("Primary action cleared.", type = "message", duration = 2)
  })
  observeEvent(input$wi_clear_B, {
    wi_rv$sim_B <- NULL
    wi_hide_if_both_cleared()
    showNotification("Alternative action cleared.", type = "message", duration = 2)
  })

  # ---- Filtered data reactive (applies performance filters) ----
  wi_perf_data <- reactive({
    req(wi_rv$scenario_df)
    bl_scoped <- if (!is.null(wi_rv$baseline_cd))   wi_rv$baseline_cd   else credit_data
    sc_scoped <- wi_rv$scenario_df
    bl_full   <- if (!is.null(wi_rv$baseline_full)) wi_rv$baseline_full else bl_scoped
    sc_full   <- if (!is.null(wi_rv$scenario_full)) wi_rv$scenario_full else sc_scoped

    .build_keep <- function(df) {
      keep <- rep(TRUE, nrow(df))
      if (!("all" %in% input$wi_perf_fico) && length(input$wi_perf_fico) > 0)
        keep <- keep & (as.character(df$fico_band) %in% input$wi_perf_fico)
      if (!("all" %in% input$wi_perf_partner) && length(input$wi_perf_partner) > 0)
        keep <- keep & (as.character(df$partner_channel) %in% input$wi_perf_partner)
      if (!("all" %in% input$wi_perf_repeat) && length(input$wi_perf_repeat) > 0)
        keep <- keep & (as.character(df$borrower_repeat_type) %in% input$wi_perf_repeat)
      if (!("all" %in% input$wi_perf_grade) && length(input$wi_perf_grade) > 0)
        keep <- keep & (as.character(df$risk_grade) %in% input$wi_perf_grade)
      if (!("all" %in% input$wi_perf_mob) && length(input$wi_perf_mob) > 0)
        keep <- keep & (as.character(df$mob_bins) %in% input$wi_perf_mob)
      keep
    }
    keep_scoped <- .build_keep(bl_scoped)
    keep_full   <- .build_keep(bl_full)

    list(baseline      = bl_scoped[keep_scoped, , drop = FALSE],
         scenario      = sc_scoped[keep_scoped, , drop = FALSE],
         baseline_full = bl_full[keep_full, , drop = FALSE],
         scenario_full = sc_full[keep_full, , drop = FALSE])
  })

  # ---- What-If KPI Boxes (dual-aware) ----
  build_wi_kpi_row <- function(sim, label_prefix = "") {
    # Funnel/rate KPIs read full-pool numbers; risk KPIs (Bad Rate, Avg APR)
    # stay tied to the scope-filtered cohort.
    bl_full <- sim$baseline_full %||% sim$baseline_cd
    pr_full <- sim$scenario_full %||% sim$scenario_df
    bl <- .compute_funnel_merged(bl_full, sim$baseline_cd)
    pr <- .compute_funnel_merged(pr_full, sim$scenario_df)
    bl_issued <- sum(bl_full$issued_flag == 1, na.rm = TRUE)
    bl_appr   <- sum(bl_full$offered_decision_flag == "APPR", na.rm = TRUE)

    kpi1 <- kpi_box(
      fmt_k(bl_issued), "Issued Loans",
      sub = paste0(fmt_k(bl_appr), " approved (baseline)"))
    kpi2 <- kpi_box(
      sprintf("%.1f%% → %.1f%%", bl$appr_rate * 100, pr$appr_rate * 100),
      "Approval Rate",
      sub = sprintf("%+.1f pp", (pr$appr_rate - bl$appr_rate) * 100))
    kpi3 <- kpi_box(
      sprintf("%.1f%% → %.1f%%", bl$conv_rate * 100, pr$conv_rate * 100),
      "Take Rate",
      sub = sprintf("%+.1f pp", (pr$conv_rate - bl$conv_rate) * 100))
    # Compute projected bad rate subtitle only when feature flag is enabled.
    # Disabled by default for demo performance — see .WI_USE_PROJECTED_BAD_RATE.
    observed_delta_sub <- sprintf("%+.1f pp", (pr$bad_rate - bl$bad_rate) * 100)
    proj_sub <- if (!isTRUE(.WI_USE_PROJECTED_BAD_RATE)) {
      observed_delta_sub
    } else {
      tape <- tryCatch(perf_tape(), error = function(e) NULL)
      tryCatch({
        if (!is.null(tape)) {
          bl_ids <- as.character(sim$baseline_cd$loan_id[sim$baseline_cd$issued_flag == 1])
          pr_ids <- as.character(sim$scenario_df$loan_id[sim$scenario_df$issued_flag == 1])
          # Fast path: skip when too many ids to keep renders snappy.
          if (length(bl_ids) > 2000 || length(pr_ids) > 2000) {
            observed_delta_sub
          } else {
            setTimeLimit(cpu = 5, elapsed = 5, transient = TRUE)
            on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE), add = TRUE)
            bl_proj_res <- compute_projected_bad_rate(bl_ids, tape, sim$baseline_cd)
            pr_proj_res <- compute_projected_bad_rate(pr_ids, tape, sim$scenario_df)
            bl_p <- if (nrow(bl_proj_res) > 0) mean(bl_proj_res$projected_lifetime_prob, na.rm = TRUE) else NA_real_
            pr_p <- if (nrow(pr_proj_res) > 0) mean(pr_proj_res$projected_lifetime_prob, na.rm = TRUE) else NA_real_
            if (!is.na(bl_p) && !is.na(pr_p) && abs(bl_p - bl$bad_rate) > 0.001)
              sprintf("projected: %.1f%% \u2192 %.1f%%", bl_p * 100, pr_p * 100)
            else observed_delta_sub
          }
        } else {
          observed_delta_sub
        }
      }, error = function(e) {
        message("[WhatIf] wi_kpi_panel projected bad rate: ", conditionMessage(e))
        observed_delta_sub
      })
    }

    kpi4 <- kpi_box(
      sprintf("%.1f%% → %.1f%%", bl$bad_rate * 100, pr$bad_rate * 100),
      "Bad Rate (Issued)",
      sub = proj_sub)
    delta_bps <- (pr$avg_apr - bl$avg_apr) * 10000
    kpi5 <- kpi_box(
      sprintf("%.1f%% → %.1f%%", bl$avg_apr * 100, pr$avg_apr * 100),
      "Avg APR",
      sub = sprintf("%+.0f bps", delta_bps))

    tags$div(
      if (nchar(label_prefix) > 0) {
        tags$div(style = "padding:0 15px; margin-bottom:4px;",
          tags$strong(label_prefix,
            style = paste0("font-size:14px; color:",
              if (grepl("A", label_prefix)) "#f59e0b" else "#3b82f6", ";")))
      },
      tags$div(style = "display:flex; gap:10px; padding:0 15px; margin-bottom:10px;",
        tags$div(style = "flex:1; min-width:0;", kpi1),
        tags$div(style = "flex:1; min-width:0;", kpi2),
        tags$div(style = "flex:1; min-width:0;", kpi3),
        tags$div(style = "flex:1; min-width:0;", kpi4),
        tags$div(style = "flex:1; min-width:0;", kpi5))
    )
  }

  # ---- Scenario Summary (reminds user what each slot contains) ----
  output$wi_scenario_summary <- renderUI({
    simA <- wi_rv$sim_A
    simB <- wi_rv$sim_B
    if (is.null(simA) && is.null(simB)) return(NULL)
    rows <- list()
    for (slot in c("A", "B")) {
      sim <- wi_rv[[paste0("sim_", slot)]]
      if (is.null(sim)) next
      label <- if (slot == "A") "Primary" else "Alternative"

      # Describe what changed — compare to the CURRENT ACTIVE policy for the
      # selected product, not a hardcoded PL-CURRENT. Promoting a different
      # policy to Active via the Policy Repository should change this
      # baseline so the scenario-summary deltas stay meaningful.
      params <- sim$params
      defaults <- build_default_params(policy_manifest)
      .active_pid <- tryCatch(
        get_active_policy_id(input$wi_product %||% "Personal Loan"),
        error = function(e) "PL-CURRENT")
      .so_sd <- get0("strategy_overrides", envir = .GlobalEnv, inherits = FALSE)
      if (!is.null(.so_sd) && !is.null(.so_sd[[.active_pid]])) {
        defaults <- modifyList(defaults, .so_sd[[.active_pid]]$params)
      }
      impact <- compute_cascade_impact(params, defaults)
      n_changed <- length(impact$changed_ids)
      n_rules <- length(sim$rules %||% list())
      bps <- sim$bps_adj %||% 0L

      desc_parts <- c()
      if (bps != 0) desc_parts <- c(desc_parts,
        paste0(if (bps > 0) "+" else "", bps, " bps pricing"))
      if (n_changed > 0) desc_parts <- c(desc_parts,
        paste0(n_changed, " param", if (n_changed > 1) "s", " changed"))
      if (n_rules > 0) desc_parts <- c(desc_parts,
        paste0(n_rules, " manual rule", if (n_rules > 1) "s"))
      if (length(desc_parts) == 0) desc_parts <- "No changes from baseline"
      desc <- paste(desc_parts, collapse = " | ")

      n_aff <- sim$n_affected %||% 0
      aff_text <- if (n_aff > 0) paste0(format(n_aff, big.mark = ","), " affected") else "0 affected"

      rows[[length(rows) + 1]] <- div(
        style = "display:flex; gap:16px; align-items:center; padding:8px 14px; border-top:1px solid #e5e7eb;",
        tags$div(style = "min-width:88px; font-weight:700; color:#0f172a;", label),
        tags$div(style = "min-width:110px; color:#334155;", aff_text),
        tags$div(style = "flex:1; color:#64748b; font-size:12px;", desc),
        tags$div(style = "min-width:120px; color:#334155; font-size:12px; text-align:right;",
          sim$label %||% "")
      )
    }
    div(class = "card", style = "margin-bottom:8px;",
      h5(icon("clipboard-list"), " Scenario Summary",
        style = "font-weight:bold; margin:6px 0 4px 14px; font-size:14px;"),
      div(style = "background:#f8fafc; border-radius:8px; overflow:hidden;", rows)
    )
  })

  output$wi_kpi_panel <- renderUI({
    simA <- wi_rv$sim_A
    simB <- wi_rv$sim_B
    if (!is.null(simA) && !is.null(simB)) {
      tagList(
        build_wi_kpi_row(simA, simA$label %||% "Primary Action"),
        build_wi_kpi_row(simB, simB$label %||% "Alternative Action")
      )
    } else if (!is.null(simA)) {
      build_wi_kpi_row(simA)
    } else if (!is.null(simB)) {
      build_wi_kpi_row(simB)
    } else {
      # Fallback: use legacy wi_rv state
      req(wi_rv$scenario_df)
      build_wi_kpi_row(list(
        scenario_df = wi_rv$scenario_df,
        baseline_cd = if (!is.null(wi_rv$baseline_cd)) wi_rv$baseline_cd else credit_data
      ))
    }
  })

  # ---- AI Insights for What-If (UC3) ----
  output$wi_ai_insights <- renderUI({
    simA <- wi_rv$sim_A
    req(simA)
    bl <- .compute_funnel_merged(simA$baseline_full %||% simA$baseline_cd, simA$baseline_cd)
    pr <- .compute_funnel_merged(simA$scenario_full %||% simA$scenario_df, simA$scenario_df)
    kpis <- list(
      baseline_appr_rate = bl$appr_rate,
      scenario_appr_rate = pr$appr_rate,
      baseline_bad_rate  = bl$bad_rate,
      scenario_bad_rate  = pr$bad_rate,
      baseline_avg_apr   = bl$avg_apr,
      scenario_avg_apr   = pr$avg_apr,
      baseline_issued    = bl$issued,
      scenario_issued    = pr$issued
    )
    changed <- if (!is.null(simA$params)) {
      paste(names(simA$params), collapse = ", ")
    } else "none"
    insights_text <- tryCatch(
      generate_insights("simulation", kpis, details = list(changed_params = changed)),
      error = function(e) paste0("- Could not generate insights: ", conditionMessage(e))
    )
    tags$details(
      style = "margin-top:12px; background:#1a1f2e; border-left:3px solid #fb6e36; border-radius:6px; padding:12px 16px;",
      open = "open",
      tags$summary(style = "color:#fb6e36; font-weight:600; cursor:pointer;",
        icon("sparkles"), " AI Insights"),
      tags$div(style = "color:#d1d5db; font-size:13px; margin-top:8px; white-space:pre-wrap;",
        HTML(insights_text))
    )
  })

  # ---- Talking Points for What-If (UC7) ----
  observeEvent(input$wi_talking_pts, {
    simA <- wi_rv$sim_A
    req(simA)
    bl <- .compute_funnel_merged(simA$baseline_full %||% simA$baseline_cd, simA$baseline_cd)
    pr <- .compute_funnel_merged(simA$scenario_full %||% simA$scenario_df, simA$scenario_df)
    tp_data <- list(
      baseline_appr_rate = bl$appr_rate,
      scenario_appr_rate = pr$appr_rate,
      baseline_bad_rate  = bl$bad_rate,
      scenario_bad_rate  = pr$bad_rate,
      baseline_avg_apr   = bl$avg_apr,
      scenario_avg_apr   = pr$avg_apr
    )
    tp_text <- tryCatch(
      generate_talking_points("scenario", tp_data),
      error = function(e) paste0("1. Error generating talking points: ", conditionMessage(e))
    )
    showModal(modalDialog(
      title = "Talking Points",
      tags$pre(id = "wi_tp_text",
        style = "background:#111827; color:#d1d5db; padding:16px; border-radius:6px; white-space:pre-wrap; font-size:13px;",
        tp_text),
      footer = tagList(
        actionButton("wi_tp_copy", "Copy to Clipboard", icon = icon("clipboard")),
        modalButton("Close")
      )
    ))
  })

  observeEvent(input$wi_tp_copy, {
    shinyjs::runjs("navigator.clipboard.writeText(document.getElementById('wi_tp_text').innerText);")
    showNotification("Copied!", type = "message", duration = 2)
  })

  # ---- Sample preview cards (dual-slot, Opportunity Explorer pattern) ----
  wi_sample_slot_data <- function(slot) {
    wi_rv[[paste0("sim_", slot)]]
  }

  wi_sample_slot_title <- function(slot) {
    sim <- wi_sample_slot_data(slot)
    if (is.null(sim) || is.null(sim$sample_full)) return(NULL)
    label <- if (slot == "A") "Primary" else "Alternative"
    n <- sim$n_affected %||% 0
    types <- unique(sim$sample_full$change_type)
    verb <- tolower(paste(types, collapse = " & "))
    paste0(label, " \u2014 ", format(n, big.mark = ","), " loans ", verb)
  }

  output$wi_sample_preview_cards <- renderUI({
    cards <- list()
    for (slot in c("A", "B")) {
      sim <- wi_sample_slot_data(slot)
      if (is.null(sim) || is.null(sim$sample_full)) next
      both_have <- !is.null(wi_sample_slot_data("A")$sample_full) &&
                   !is.null(wi_sample_slot_data("B")$sample_full)
      cards[[length(cards) + 1]] <- column(
        if (both_have) 6 else 12,
        div(class = "card", style = "padding:10px 15px;",
          fluidRow(
            column(8,
              h5(icon("table"), wi_sample_slot_title(slot),
                style = "font-weight:bold; margin:0; display:inline-block;")
            ),
            column(4, style = "text-align:right;",
              actionLink(
                paste0("wi_sample_expand_btn_", slot),
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

  show_wi_sample_modal <- function(slot) {
    sim <- wi_sample_slot_data(slot)
    req(sim$sample_full)
    label <- if (slot == "A") "Primary" else "Alternative"
    types <- unique(sim$sample_full$change_type)
    verb <- tolower(paste(types, collapse = " & "))
    display_df <- if (!is.null(sim$sample_all)) sim$sample_all else sim$sample_full
    output[[paste0("wi_sample_modal_table_", slot)]] <- DT::renderDT({
      DT::datatable(display_df,
        selection = "none", rownames = FALSE,
        options = list(pageLength = 25, dom = "tip", scrollX = TRUE,
                       columnDefs = list(list(className = "dt-center", targets = "_all"))))
    })
    wi_rv[[paste0("modal_download_", slot)]] <- display_df
    showModal(modalDialog(
      title = h4(icon("table"), paste0(
        label, " \u2014 ", format(sim$n_affected %||% 0, big.mark = ","),
        " total (", verb, ")")),
      size = "l", easyClose = TRUE,
      footer = tagList(
        downloadButton(paste0("wi_download_affected_", slot),
          "Download CSV", class = "btn-sm",
          style = "background-color:#10b981; color:white; border:none; margin-right:8px;"),
        modalButton("Close")
      ),
      DTOutput(paste0("wi_sample_modal_table_", slot))
    ))
  }

  observeEvent(input$wi_sample_expand_btn_A, { show_wi_sample_modal("A") })
  observeEvent(input$wi_sample_expand_btn_B, { show_wi_sample_modal("B") })

  # Download handlers for affected loans CSV
  output$wi_download_affected_A <- downloadHandler(
    filename = function() {
      sim <- wi_sample_slot_data("A")
      verb <- if ("Declined" %in% sim$sample_full$change_type) "declined" else "repriced"
      paste0("affected_loans_primary_", verb, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      df <- wi_rv$modal_download_A
      if (!is.null(df)) write.csv(df, file, row.names = FALSE)
    }
  )
  output$wi_download_affected_B <- downloadHandler(
    filename = function() {
      sim <- wi_sample_slot_data("B")
      verb <- if ("Declined" %in% sim$sample_full$change_type) "declined" else "repriced"
      paste0("affected_loans_alternative_", verb, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      df <- wi_rv$modal_download_B
      if (!is.null(df)) write.csv(df, file, row.names = FALSE)
    }
  )

  # ---- What-If Funnel Table ----
  output$wi_funnel_title <- renderUI({
    simA <- wi_rv$sim_A; simB <- wi_rv$sim_B
    dual <- !is.null(simA) && !is.null(simB)
    h4(if (dual) paste0("Funnel Metrics: Baseline vs ",
         substr(simA$label %||% "Primary", 1, 20), " vs ",
         substr(simB$label %||% "Alternative", 1, 20))
       else "Funnel Metrics: Baseline vs Scenario",
       style = "font-weight:bold;")
  })

  output$wi_funnel_subtitle <- renderUI({
    req(wi_rv$scenario_df)
    parts <- c()
    if (!("all" %in% input$wi_perf_fico) && length(input$wi_perf_fico) > 0)
      parts <- c(parts, paste0("FICO: ", paste(input$wi_perf_fico, collapse = ", ")))
    if (!("all" %in% input$wi_perf_partner) && length(input$wi_perf_partner) > 0)
      parts <- c(parts, paste0("Partner: ", paste(input$wi_perf_partner, collapse = ", ")))
    if (!("all" %in% input$wi_perf_grade) && length(input$wi_perf_grade) > 0)
      parts <- c(parts, paste0("Grade: ", paste(input$wi_perf_grade, collapse = ", ")))
    if (!("all" %in% input$wi_perf_repeat) && length(input$wi_perf_repeat) > 0)
      parts <- c(parts, paste0("Repeat: ", paste(input$wi_perf_repeat, collapse = ", ")))
    if (!("all" %in% input$wi_perf_mob) && length(input$wi_perf_mob) > 0)
      parts <- c(parts, paste0("MOB: ", paste(input$wi_perf_mob, collapse = ", ")))
    if (length(parts) == 0) return(NULL)
    tags$p(style = "color:#9ca3af; font-size:11px; margin:0 0 8px 0;",
      icon("filter"), " Filtered: ", paste(parts, collapse = " | "))
  })

  output$wi_funnel_table <- renderUI({
    req(wi_rv$scenario_df)

    simA <- wi_rv$sim_A
    simB <- wi_rv$sim_B
    dual <- !is.null(simA) && !is.null(simB)

    fp  <- function(x) if (is.na(x)) "\u2014" else sprintf("%.1f%%", x * 100)
    fc  <- function(x) if (is.na(x)) "\u2014" else fmt_k(x)

    # Guard tape access — any failure here must not blank the funnel table.
    tape <- tryCatch(perf_tape(), error = function(e) {
      message("[WhatIf] wi_funnel_table perf_tape: ", conditionMessage(e))
      NULL
    })
    compute_wi_xirr <- function(df_src) {
      if (is.null(tape)) return(compute_funnel(df_src)$avg_apr)
      tryCatch({
        ids <- as.character(df_src$loan_id[df_src$issued_flag == 1])
        compute_portfolio_xirr_v2(tape, loan_ids = ids)
      }, error = function(e) {
        message("[WhatIf] wi_funnel_table xirr: ", conditionMessage(e))
        compute_funnel(df_src)$avg_apr
      })
    }
    irr_label <- if (!is.null(tape)) "Portfolio IRR" else "Avg IRR (APR)"

    # Projected bad rate is gated behind the feature flag. When disabled we
    # skip the "2+ Rate (Projected)" row entirely so the table still renders.
    compute_funnel_proj <- function(df_src) {
      if (!isTRUE(.WI_USE_PROJECTED_BAD_RATE)) return(NA_real_)
      if (is.null(tape)) return(NA_real_)
      tryCatch({
        ids <- as.character(df_src$loan_id[df_src$issued_flag == 1])
        if (length(ids) == 0 || length(ids) > 2000) return(NA_real_)
        setTimeLimit(cpu = 5, elapsed = 5, transient = TRUE)
        on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE), add = TRUE)
        proj <- compute_projected_bad_rate(ids, tape, df_src)
        if (!is.null(proj) && nrow(proj) > 0)
          mean(proj$projected_lifetime_prob, na.rm = TRUE)
        else NA_real_
      }, error = function(e) {
        message("[WhatIf] wi_funnel_table projected bad rate: ", conditionMessage(e))
        NA_real_
      })
    }

    if (dual) {
      bl  <- .compute_funnel_merged(simA$baseline_full %||% simA$baseline_cd, simA$baseline_cd)
      prA <- .compute_funnel_merged(simA$scenario_full %||% simA$scenario_df, simA$scenario_df)
      prB <- .compute_funnel_merged(simB$scenario_full %||% simB$scenario_df, simB$scenario_df)
      bl_irr  <- compute_wi_xirr(simA$baseline_cd)
      prA_irr <- compute_wi_xirr(simA$scenario_df)
      prB_irr <- compute_wi_xirr(simB$scenario_df)

      fd <- function(x) if (is.na(x)) "\u2014" else fmt_dollar_k(x)
      rows <- list(
        c("Applications",  fc(bl$apps),      fc(prA$apps),      fc(prB$apps)),
        c("Approved",      fc(bl$approved),  fc(prA$approved),  fc(prB$approved)),
        c("Issued",        fc(bl$issued),    fc(prA$issued),    fc(prB$issued)),
        c("Approval Rate", fp(bl$appr_rate), fp(prA$appr_rate), fp(prB$appr_rate)),
        c("Take Rate",     fp(bl$conv_rate), fp(prA$conv_rate), fp(prB$conv_rate)),
        c("Avg Loan Size", fd(bl$avg_amt),   fd(prA$avg_amt),   fd(prB$avg_amt)),
        c("2+ Rate",       fp(bl$bad_rate),  fp(prA$bad_rate),  fp(prB$bad_rate))
      )
      if (isTRUE(.WI_USE_PROJECTED_BAD_RATE)) {
        bl_proj <- compute_funnel_proj(simA$baseline_cd)
        prA_proj <- compute_funnel_proj(simA$scenario_df)
        prB_proj <- compute_funnel_proj(simB$scenario_df)
        if (!is.na(bl_proj))
          rows <- c(rows, list(c("2+ Rate (Projected)", fp(bl_proj), fp(prA_proj), fp(prB_proj))))
      }
      rows <- c(rows, list(
        c("Avg APR",       fp(bl$avg_apr),   fp(prA$avg_apr),   fp(prB$avg_apr)),
        c(irr_label,       fp(bl_irr),       fp(prA_irr),       fp(prB_irr))
      ))
      tbl <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
      names(tbl) <- c("Metric", "Baseline", "Primary", "Alternative")

    } else {
      d  <- wi_perf_data()
      bl <- .compute_funnel_merged(d$baseline_full %||% d$baseline, d$baseline)
      pr <- .compute_funnel_merged(d$scenario_full %||% d$scenario, d$scenario)
      bl_irr <- compute_wi_xirr(d$baseline)
      pr_irr <- compute_wi_xirr(d$scenario)

      fd <- function(x) if (is.na(x)) "\u2014" else fmt_dollar_k(x)
      rows <- list(
        c("Applications",    fc(bl$apps),      fc(pr$apps)),
        c("Approved",        fc(bl$approved),  fc(pr$approved)),
        c("Issued",          fc(bl$issued),    fc(pr$issued)),
        c("Approval Rate",   fp(bl$appr_rate), fp(pr$appr_rate)),
        c("Take Rate",       fp(bl$conv_rate), fp(pr$conv_rate)),
        c("Avg Loan Size",   fd(bl$avg_amt),   fd(pr$avg_amt)),
        c("2+ Rate",         fp(bl$bad_rate),  fp(pr$bad_rate))
      )
      if (isTRUE(.WI_USE_PROJECTED_BAD_RATE)) {
        bl_proj <- compute_funnel_proj(d$baseline)
        pr_proj <- compute_funnel_proj(d$scenario)
        if (!is.na(bl_proj))
          rows <- c(rows, list(c("2+ Rate (Projected)", fp(bl_proj), fp(pr_proj))))
      }
      rows <- c(rows, list(
        c("Avg APR",         fp(bl$avg_apr),   fp(pr$avg_apr)),
        c(irr_label,         fp(bl_irr),       fp(pr_irr))
      ))
      tbl <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
      slot_label <- if (!is.null(wi_rv$sim_A)) (wi_rv$sim_A$label %||% "Primary Action")
                    else if (!is.null(wi_rv$sim_B)) (wi_rv$sim_B$label %||% "Alternative Action")
                    else "Scenario"
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

    tags$div(style = "overflow-x:auto;",
      tags$table(
        style = "width:100%; border-collapse:collapse; table-layout:fixed;",
        tags$thead(tags$tr(header_cells)),
        tags$tbody(body_rows)
      )
    )
  })

  # ---- What-If IRR Chart ----
  output$wi_irr_plot <- renderPlotly({
    req(wi_rv$scenario_df)
    level <- input$wi_irr_level
    tape <- tryCatch(perf_tape(), error = function(e) {
      message("[WhatIf] wi_irr_plot perf_tape: ", conditionMessage(e))
      NULL
    })

    simA <- wi_rv$sim_A
    simB <- wi_rv$sim_B
    dual <- !is.null(simA) && !is.null(simB)

    # Helper: format N with K suffix (e.g. 2.3K)
    .fmt_n <- function(n) ifelse(n >= 1000,
      paste0(formatC(n / 1000, format = "f", digits = 1), "K"),
      as.character(n))

    compute_agg <- function(df_src) {
      issued <- df_src[df_src$issued_flag == 1, , drop = FALSE]
      if (nrow(issued) == 0)
        return(data.frame(Group = character(0), irr_val = numeric(0),
                          n_loans = integer(0), stringsAsFactors = FALSE))
      if (!is.null(tape)) {
        ids <- as.character(issued$loan_id)
        if (level == "Overall") {
          val <- compute_portfolio_xirr_v2(tape, loan_ids = ids)
          data.frame(Group = "Overall",
                     irr_val = ifelse(is.na(val), 0, val),
                     n_loans = length(ids), stringsAsFactors = FALSE)
        } else {
          grp <- compute_grouped_xirr_v2(tape, level, loan_ids = ids)
          if (nrow(grp) == 0) return(data.frame(Group = character(0),
                    irr_val = numeric(0), n_loans = integer(0),
                    stringsAsFactors = FALSE))
          # Count loans per group
          loan_grps <- issued %>%
            filter(!is.na(.data[[level]])) %>%
            group_by(Group = .data[[level]]) %>%
            summarise(n_loans = n(), .groups = "drop")
          out <- data.frame(Group = as.character(grp$group),
                            irr_val = ifelse(is.na(grp$xirr_value), 0, grp$xirr_value),
                            stringsAsFactors = FALSE)
          loan_grps$Group <- as.character(loan_grps$Group)
          out <- merge(out, loan_grps, by = "Group", all.x = TRUE)
          out$n_loans[is.na(out$n_loans)] <- 0L
          out
        }
      } else {
        if (level == "Overall") {
          data.frame(Group = "Overall",
                     irr_val = mean(issued$interest_rate_apr, na.rm = TRUE),
                     n_loans = nrow(issued), stringsAsFactors = FALSE)
        } else {
          issued %>%
            filter(!is.na(.data[[level]])) %>%
            group_by(Group = .data[[level]]) %>%
            summarise(irr_val = mean(interest_rate_apr, na.rm = TRUE),
                      n_loans = n(), .groups = "drop") %>%
            as.data.frame()
        }
      }
    }

    is_irr <- !is.null(tape)
    y_title <- if (is_irr) "IRR (annualised)" else "Avg APR"
    level_label <- switch(level,
      "Overall" = "Overall", "fico_band" = "FICO Band",
      "partner_channel" = "Partner Channel", "risk_grade" = "Grade",
      "borrower_repeat_type" = "Repeat Type", level)

    # Helper: build one bar trace
    .bar_trace <- function(p, agg, name, color) {
      p %>% add_bars(
        x = agg$Group, y = agg$irr_val, name = name,
        marker = list(color = color),
        customdata = agg$n_loans,
        hovertemplate = paste0("<b>%{x}</b><br>",
          if (is_irr) "IRR" else "APR", ": %{y:.1%}",
          "<br>N = %{customdata:,}<extra></extra>"))
    }

    if (dual) {
      bl_agg  <- compute_agg(simA$baseline_cd)
      scA_agg <- compute_agg(simA$scenario_df)
      scB_agg <- compute_agg(simB$scenario_df)

      y_max <- max(c(bl_agg$irr_val, scA_agg$irr_val, scB_agg$irr_val), na.rm = TRUE) * 1.3

      p <- plot_ly()
      p <- .bar_trace(p, bl_agg,  "Baseline",
                      "#6b7280")
      p <- .bar_trace(p, scA_agg, simA$label %||% "Primary Action",
                      "#f59e0b")
      p <- .bar_trace(p, scB_agg, simB$label %||% "Alternative Action",
                      "#3b82f6")
      p %>% layout(barmode = "group",
        title  = list(text = paste("Portfolio", if (is_irr) "IRR" else "APR", "by", level_label),
                      font = list(size = 12), y = 0.98),
        xaxis  = list(title = list(text = level_label, font = list(size = 10)),
                      tickfont = list(size = 9)),
        yaxis  = list(title = list(text = y_title, font = list(size = 10)),
                      tickformat = ".1%", tickfont = list(size = 9),
                      range = list(0, y_max)),
        legend = list(font = list(size = 9), orientation = "h", y = -0.25),
        margin = list(l = 50, r = 10, t = 30, b = 60),
        paper_bgcolor = "white", plot_bgcolor = "white")
    } else {
      d <- wi_perf_data()
      bl_agg <- compute_agg(d$baseline)
      sc_agg <- compute_agg(d$scenario)

      slot_label <- if (!is.null(wi_rv$sim_A)) (wi_rv$sim_A$label %||% "Primary Action")
                    else if (!is.null(wi_rv$sim_B)) (wi_rv$sim_B$label %||% "Alternative Action")
                    else "Scenario"
      sc_color <- if (!is.null(wi_rv$sim_A) && is.null(wi_rv$sim_B)) "#f59e0b"
                  else if (is.null(wi_rv$sim_A) && !is.null(wi_rv$sim_B)) "#3b82f6"
                  else "#fb6e36"

      y_max <- max(c(bl_agg$irr_val, sc_agg$irr_val), na.rm = TRUE) * 1.3

      p <- plot_ly()
      p <- .bar_trace(p, bl_agg, "Baseline", "#6b7280")
      p <- .bar_trace(p, sc_agg, slot_label, sc_color)
      p %>% layout(barmode = "group",
        title  = list(text = paste("Portfolio", if (is_irr) "IRR" else "APR", "by", level_label),
                      font = list(size = 12), y = 0.98),
        xaxis  = list(title = list(text = level_label, font = list(size = 10)),
                      tickfont = list(size = 9)),
        yaxis  = list(title = list(text = y_title, font = list(size = 10)),
                      tickformat = ".1%", tickfont = list(size = 9),
                      range = list(0, y_max)),
        legend = list(font = list(size = 9), orientation = "h", y = -0.25),
        margin = list(l = 50, r = 10, t = 30, b = 60),
        paper_bgcolor = "white", plot_bgcolor = "white")
    }
  })

  # ---- What-If Vintage helpers (mirror Opportunity Finder) ----
  .wi_build_vintage <- function(tape, ids, sel_vqs, scenario_label,
                                new_ids = character(0)) {
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

    # For newly-approved loans with no tape history, inject neighbor-band
    # delinquency timing (median MOB from actual delinquencies in same cohort)
    if (length(new_ids) > 0) {
      syn_mob <- if (nrow(first_dq) > 0) median(first_dq$first_dq_mob) else min_dq_mob
      new_in_tape <- rows_sub %>%
        filter(loan_id %in% new_ids) %>%
        distinct(loan_id, vintage_quarter) %>%
        anti_join(first_dq, by = "loan_id")
      if (nrow(new_in_tape) > 0) {
        new_dq <- new_in_tape %>%
          mutate(first_dq_mob = as.integer(round(syn_mob)))
        first_dq <- bind_rows(first_dq, new_dq)
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

  .wi_plot_vintage <- function(bl_data, sc_data, chart_title) {
    both <- bind_rows(bl_data, sc_data)
    if (nrow(both) == 0) {
      return(plot_ly() %>%
        layout(title = list(text = "No vintage data available",
                            font = list(size = 13)),
               paper_bgcolor = "white", plot_bgcolor = "white"))
    }
    vintages_in_data <- sort(unique(both$vintage_quarter))
    pal <- if (length(vintages_in_data) <= 10) {
      setNames(RColorBrewer::brewer.pal(max(3, length(vintages_in_data)), "Set1")[
        seq_along(vintages_in_data)], vintages_in_data)
    } else {
      setNames(colorRampPalette(RColorBrewer::brewer.pal(8, "Set1"))(
        length(vintages_in_data)), vintages_in_data)
    }

    p <- plot_ly()
    scenarios <- unique(both$Scenario)
    for (vq in vintages_in_data) {
      first_trace <- TRUE
      for (sc in scenarios) {
        sub <- both %>% filter(vintage_quarter == vq, Scenario == sc)
        if (nrow(sub) == 0) next
        is_baseline <- (sc == "Baseline")
        p <- p %>% add_trace(
          data = sub, x = ~month_on_books, y = ~bad_pct,
          type = "scatter", mode = "lines+markers",
          name = vq,
          showlegend = first_trace,
          line = list(color = pal[vq],
                      dash = if (is_baseline) "solid" else "dash",
                      width = 2, shape = "linear"),
          marker = list(color = pal[vq], size = 3),
          legendgroup = vq,
          hovertemplate = paste0(
            "<b>", vq, " (", sc, ")</b><br>",
            "MOB: %{x}<br>Delinquency: %{y:.1%}<br>",
            "Loans: ", fmt_k(sub$n_loans[1]), "<extra></extra>"))
        first_trace <- FALSE
      }
    }
    p %>% layout(
      title  = list(text = chart_title, font = list(size = 13)),
      xaxis  = list(title = "Month on Books"),
      yaxis  = list(title = "Cumulative Delinquency Rate", tickformat = ".1%"),
      paper_bgcolor = "white", plot_bgcolor = "white",
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.3,
                    font = list(size = 9)),
      annotations = list(list(
        text = "<i>solid = baseline, dashed = scenario</i>",
        x = 1, xref = "paper", xanchor = "right",
        y = -0.22, yref = "paper", yanchor = "top",
        showarrow = FALSE,
        font = list(size = 9, color = "#9ca3af"))))
  }

  # ---- What-If Vintage Curve ----
  observe({
    req(wi_rv$scenario_df)
    sc <- wi_rv$scenario_df
    tape <- tryCatch(perf_tape(), error = function(e) {
      message("[WhatIf] wi_vintage observe perf_tape: ", conditionMessage(e))
      NULL
    })
    if (!is.null(tape) && !is.null(tape$vintages)) {
      # Use perf tape's vintage_quarter labels (e.g. "2025Q1") so they match
      # the filtering in the chart renderer.
      #
      # IMPORTANT: Only offer vintages that actually contain loans from the
      # current scenario. The tape spans multiple policy regimes (V1..V4 +
      # PL-CURRENT) but What-If filters to PL-CURRENT only, which typically
      # lives in the most recent 4-6 vintages. Defaulting to the first 4
      # vintages (sorted alphabetically) would pick the 2021 regime and the
      # chart would render empty.
      all_vqs <- sort(tape$vintages)
      scen_ids <- as.character(sc$loan_id[!is.na(sc$loan_id)])
      scen_tape <- tape$rows[tape$rows$loan_id %in% scen_ids, ]
      coverage_vqs <- sort(unique(as.character(scen_tape$vintage_quarter)))
      # Fall back to all vintages if nothing in tape matches the scenario
      if (length(coverage_vqs) == 0) coverage_vqs <- all_vqs
      if (length(coverage_vqs) > 0) {
        # Select the 4 most recent vintages with coverage (tail of sorted list)
        n_sel <- min(4, length(coverage_vqs))
        default_sel <- utils::tail(coverage_vqs, n_sel)
        updatePickerInput(session, "wi_vintage_quarters",
          choices = coverage_vqs, selected = default_sel)
      }
    } else if ("booking_vintage" %in% names(sc)) {
      # Fallback: convert booking_vintage dates to quarter labels
      dates <- suppressWarnings(as.Date(sc$booking_vintage, format = "%d/%m/%Y"))
      dates <- dates[!is.na(dates)]
      if (length(dates) > 0) {
        vints <- sort(unique(vintage_quarter_label(dates)))
        n_sel <- min(4, length(vints))
        updatePickerInput(session, "wi_vintage_quarters",
          choices = vints, selected = utils::tail(vints, n_sel))
      }
    }
  })

  # ---- Toggle vintage layout: full width when single, side-by-side when dual ----
  observe({
    dual <- !is.null(wi_rv$sim_A) && !is.null(wi_rv$sim_B)
    if (dual) {
      shinyjs::runjs("$('#wi_vintage_col_a').removeClass('col-sm-12').addClass('col-sm-6');")
      showElement("wi_vintage_col_b")
    } else {
      shinyjs::runjs("$('#wi_vintage_col_a').removeClass('col-sm-6').addClass('col-sm-12');")
      hideElement("wi_vintage_col_b")
    }
  })

  # ---- What-If Vintage Chart A ----
  output$wi_vintage_plot_a <- renderPlotly({
    tape <- tryCatch(perf_tape(), error = function(e) {
      message("[WhatIf] wi_vintage_plot_a perf_tape: ", conditionMessage(e))
      NULL
    })
    if (is.null(tape)) {
      return(plotly_empty() %>%
        layout(title = list(text = "Upload a performance tape to see vintage curves",
               font = list(color = "#9ca3af")),
               paper_bgcolor = "white", plot_bgcolor = "white"))
    }
    sel_vints <- input$wi_vintage_quarters
    if (is.null(sel_vints) || length(sel_vints) == 0) {
      return(plotly_empty() %>%
        layout(title = list(text = "Select vintages to display",
               font = list(color = "#9ca3af")),
               paper_bgcolor = "white", plot_bgcolor = "white"))
    }

    simA <- wi_rv$sim_A
    simB <- wi_rv$sim_B
    dual <- !is.null(simA) && !is.null(simB)

    if (dual) {
      bl_ids <- as.character(simA$baseline_cd$loan_id[simA$baseline_cd$issued_flag == 1])
      pr_ids <- as.character(simA$scenario_df$loan_id[simA$scenario_df$issued_flag == 1])
      new_ids <- simA$new_loan_ids %||% character(0)
      sc_label <- simA$label %||% "Primary Action"
    } else {
      req(wi_rv$scenario_df)
      bl_src <- if (!is.null(wi_rv$baseline_cd)) wi_rv$baseline_cd else credit_data
      sc_src <- wi_rv$scenario_df
      bl_ids <- as.character(bl_src$loan_id[bl_src$issued_flag == 1])
      pr_ids <- as.character(sc_src$loan_id[sc_src$issued_flag == 1])
      new_ids <- if (!is.null(simA)) simA$new_loan_ids %||% character(0)
                 else if (!is.null(simB)) simB$new_loan_ids %||% character(0)
                 else character(0)
      sc_label <- if (!is.null(simA)) (simA$label %||% "Primary Action")
                  else if (!is.null(simB)) (simB$label %||% "Alternative Action")
                  else "Simulated"
    }

    tryCatch({
      bl_v <- .wi_build_vintage(tape, bl_ids, sel_vints, "Baseline")
      pr_v <- .wi_build_vintage(tape, pr_ids, sel_vints, sc_label, new_ids = new_ids)
      title <- paste("Baseline vs", sc_label)
      .wi_plot_vintage(bl_v, pr_v, title)
    }, error = function(e) {
      message("[WhatIf] wi_vintage_plot_a build: ", conditionMessage(e))
      plotly_empty() %>%
        layout(title = list(text = "Vintage curve unavailable",
               font = list(color = "#9ca3af")),
               paper_bgcolor = "white", plot_bgcolor = "white")
    })
  })

  # ---- What-If Vintage Chart B ----
  output$wi_vintage_plot_b <- renderPlotly({
    tape <- tryCatch(perf_tape(), error = function(e) {
      message("[WhatIf] wi_vintage_plot_b perf_tape: ", conditionMessage(e))
      NULL
    })
    simA <- wi_rv$sim_A
    simB <- wi_rv$sim_B

    if (is.null(tape) || is.null(simA) || is.null(simB)) {
      msg <- if (is.null(tape)) "Upload a performance tape"
             else "Run a second scenario to compare"
      return(plotly_empty() %>%
        layout(title = list(text = msg, font = list(size = 13, color = "#9ca3af")),
               paper_bgcolor = "white", plot_bgcolor = "white",
               xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)))
    }

    sel_vints <- input$wi_vintage_quarters
    if (is.null(sel_vints) || length(sel_vints) == 0) {
      return(plotly_empty() %>%
        layout(title = list(text = "Select vintages to display",
               font = list(color = "#9ca3af")),
               paper_bgcolor = "white", plot_bgcolor = "white"))
    }

    bl_ids <- as.character(simA$baseline_cd$loan_id[simA$baseline_cd$issued_flag == 1])
    pr_ids <- as.character(simB$scenario_df$loan_id[simB$scenario_df$issued_flag == 1])
    new_ids <- simB$new_loan_ids %||% character(0)

    label_b <- simB$label %||% "Alternative Action"
    tryCatch({
      bl_v <- .wi_build_vintage(tape, bl_ids, sel_vints, "Baseline")
      pr_v <- .wi_build_vintage(tape, pr_ids, sel_vints, label_b, new_ids = new_ids)
      .wi_plot_vintage(bl_v, pr_v, paste("Baseline vs", label_b))
    }, error = function(e) {
      message("[WhatIf] wi_vintage_plot_b build: ", conditionMessage(e))
      plotly_empty() %>%
        layout(title = list(text = "Vintage curve unavailable",
               font = list(color = "#9ca3af")),
               paper_bgcolor = "white", plot_bgcolor = "white")
    })
  })

  # ---- What-If Save to Repository (slot-aware) ----
  save_wi_scenario <- function(slot) {
    sim <- wi_rv[[paste0("sim_", slot)]]
    if (is.null(sim)) {
      showNotification(paste0("Scenario ", slot, " has no simulation. Run Simulate first."),
        type = "warning", duration = 3)
      return()
    }

    name <- trimws(input$wi_save_name)
    if (nchar(name) == 0) {
      showNotification("Please provide a scenario name.", type = "warning", duration = 3)
      return()
    }

    saved_params <- sim$params
    saved_rules  <- sim$rules %||% list()

    policy_fn <- function(data) {
      result <- apply_policy_with_params(data, saved_params)
      perf_cols <- c("issued_flag", "issued_amount", "performance_delinquency_flag",
                     "booking_vintage")
      for (col in perf_cols) {
        if (col %in% names(data) && !(col %in% names(result))) {
          result[[col]] <- data[[col]]
        }
      }
      for (rule in saved_rules) {
        mask <- evaluate_segment_conditions(result, rule$conditions)
        if (sum(mask) == 0) next
        if (rule$action == "Decline") {
          to_decline <- mask &
            !is.na(result$offered_decision_flag) &
            result$offered_decision_flag == "APPR"
          result$offered_decision_flag[to_decline] <- "DECL"
          result$issued_flag[to_decline] <- 0L
        } else if (rule$action == "Pricing" && rule$bps != 0) {
          in_segment_issued <- mask & (result$issued_flag == 1L)
          to_price <- mask &
            !is.na(result$offered_decision_flag) &
            result$offered_decision_flag == "APPR"
          result$adjusted_apr_pre_term_cap[to_price] <-
            result$adjusted_apr_pre_term_cap[to_price] + rule$bps / 10000
          result <- replay_layers_50_80_90(result,
            fico_min = saved_params$fico_min %||% 300,
            fico_max = saved_params$fico_max %||% 850,
            thin_file_mo = saved_params$thin_file_months %||% 30,
            delinq_thresh = saved_params$delinquency_max %||% 1,
            dti_thresh = saved_params$dti_cutoff %||% 0.50)
          result$interest_rate_apr <- ifelse(
            result$offered_decision_flag == "APPR",
            result$offered_final_apr,
            result$interest_rate_apr)
          newly_declined <- result$offered_decision_flag != "APPR" &
                            data$offered_decision_flag == "APPR"
          result$issued_flag[newly_declined] <- 0L

          in_seg_issued <- in_segment_issued & data$issued_flag == 1L
          n_seg_issued  <- sum(in_seg_issued, na.rm = TRUE)
          elast_pct <- (rule$elasticity %||% 5) / 100
          adv_pct   <- (rule$adverse_sel %||% 5) / 100

          if (rule$bps > 0) {
            # PRICE UP: rate-sensitive takers walk away
            drop_rate <- min((rule$bps / 100) * elast_pct, 0.50)
            n_drop    <- round(n_seg_issued * drop_rate)
            if (n_drop > 0 && n_seg_issued > 0) {
              seg_idx <- which(in_seg_issued)
              fico_order <- order(data$fico_score[seg_idx], decreasing = TRUE)
              drop_idx <- seg_idx[fico_order[seq_len(n_drop)]]
              result$issued_flag[drop_idx] <- 0L
            }
            # Adverse selection: remaining pool enriched with risk
            if (adv_pct > 0) {
              adverse_adj <- (rule$bps / 100) * adv_pct
              still_issued <- mask & result$issued_flag == 1L
              good_in_seg  <- which(still_issued &
                result$performance_delinquency_flag == 0)
              n_flip <- round(length(good_in_seg) * adverse_adj)
              if (n_flip > 0 && length(good_in_seg) > 0) {
                set.seed(123)
                fico_ord <- order(result$fico_score[good_in_seg])
                flip_idx <- good_in_seg[fico_ord[seq_len(min(n_flip, length(good_in_seg)))]]
                result$performance_delinquency_flag[flip_idx] <- 1L
              }
            }
          } else if (rule$bps < 0 && adv_pct > 0) {
            # PRICE DOWN: positive selection — flip lowest-FICO bad loans to
            # good within the segment. Mirrors Scanner's behaviour. No volume
            # capture here — swap-in of new volume stays in Opportunity Finder.
            positive_adj <- (abs(rule$bps) / 100) * adv_pct
            still_issued <- mask & result$issued_flag == 1L
            bad_in_seg   <- which(still_issued &
              result$performance_delinquency_flag == 1)
            n_rescue <- round(length(bad_in_seg) * positive_adj)
            if (n_rescue > 0 && length(bad_in_seg) > 0) {
              set.seed(123)
              fico_ord <- order(result$fico_score[bad_in_seg], decreasing = TRUE)
              rescue_idx <- bad_in_seg[fico_ord[seq_len(min(n_rescue, length(bad_in_seg)))]]
              result$performance_delinquency_flag[rescue_idx] <- 0L
            }
          }
        }
      }
      result
    }

    # Generate next policy ID
    new_id <- max(c(mockPolicies$id,
                    if (!is.null(added_policy())) max(added_policy()$id) else 0L),
                  na.rm = TRUE) + 1L
    pid <- paste0("PL-", sprintf("%03d", new_id))

    sc_funnel <- .compute_funnel_merged(sim$scenario_full %||% sim$scenario_df, sim$scenario_df)
    new_policy <- data.frame(
      id                  = new_id,
      policyId            = pid,
      policyName          = paste0(name, " (Scenario ", slot, ")"),
      status              = "simulated",
      product             = "PL",
      version             = "v1.0",
      updatedAt           = Sys.Date(),
      endDate             = as.Date(NA),
      grading_logic       = "What-If Scenario",
      pricing_logic       = sprintf("Scenario APR: %s", sprintf("%.1f%%", sc_funnel$avg_apr * 100)),
      decision_logic      = sprintf("Scenario approval: %s", sprintf("%.1f%%", sc_funnel$appr_rate * 100)),
      term_logic          = "Baseline (unchanged)",
      affordability_logic = "Baseline (unchanged)",
      max_amount_logic    = "Baseline (unchanged)",
      r_logic             = NA_character_,
      stringsAsFactors    = FALSE
    )

    # Append to added policies (reactiveVal)
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
    wi_persist_policies()

    showNotification(
      paste0("Scenario ", slot, " saved as '", name, "' (", pid, ") in Policy Repository."),
      type = "message", duration = 4)
    updateTextInput(session, "wi_save_name", value = "")
    showElement("wi_download_row")
  }

  observeEvent(input$wi_save_btn_A, { save_wi_scenario("A") })
  observeEvent(input$wi_save_btn_B, { save_wi_scenario("B") })

  # ---- NL Q&A current-view registration ----
  # Per spec §6: What-If "Current view" = the loans that pass the simulated
  # policy (wi_rv$scenario_df, i.e. the proposed scenario dataset).
  # When no simulation is active, fall through to full loan_data so the
  # NL bar still answers a portfolio-wide question.
  wi_nl_current_view <- reactive({
    proposed <- wi_rv$scenario_df
    if (!is.null(proposed) && is.data.frame(proposed) && nrow(proposed) > 0) {
      return(proposed)
    }
    loan_data
  })

  nl_register_current_view(session, "whatif_tab", wi_nl_current_view)

  # ---- SQL Modal helper ----
  show_wi_sql_modal <- function(sql_code, title) {
    showModal(modalDialog(
      title = tagList(icon("database"), title),
      size = "l", easyClose = TRUE,
      tags$div(
        style = "position:relative;",
        tags$button(
          id = "wi_copy_sql_btn", class = "btn btn-sm",
          style = paste0(
            "position:absolute; top:0; right:0; z-index:10;",
            "background:#f97316; color:white; border:none;",
            "padding:4px 12px; border-radius:4px; font-size:12px; cursor:pointer;"),
          icon("copy"), "Copy to Clipboard",
          onclick = "var t=document.getElementById('wi_sql_code_block');
                     navigator.clipboard.writeText(t.innerText);
                     this.innerHTML='<i class=\"fa fa-check\"></i> Copied!';
                     setTimeout(function(){
                       document.getElementById('wi_copy_sql_btn').innerHTML=
                         '<i class=\"fa fa-copy\"></i> Copy to Clipboard';
                     }, 2000);"
        ),
        tags$pre(id = "wi_sql_code_block",
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

  # ---- SQL buttons removed (baseline/primary/alternative SQL actions
  # removed from UI per UX feedback) ----

}
