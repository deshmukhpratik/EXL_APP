# ============================================================
# Tab: Policy Repository
# Extracted from app.R — UI and server logic
# (vs_heatmap output moved to tab_variable_analysis.R)
# ============================================================

policy_repo_ui <- function() {
  tabItem(
    tabName = "repos_1",
    tags$head(
      tags$style(HTML("
        .dataTables_wrapper tbody tr:hover:not(.selected) {
          background-color: #FFE0B2 !important; color: #333 !important;
        }
        .dataTables_wrapper tbody tr.selected {
          background-color: #FF8C00 !important; color: inherit !important;
        }
        #addPolicyFile + .btn-default {
          background-color: #FF6B35 !important; color: white !important;
          border: none !important; border-radius: 4px !important;
          padding: 6px 12px !important; font-size: 14px !important;
          width: 100% !important;
        }
        #addPolicyFile + .btn-default:hover { background-color: #e55a2b !important; }
        #downloadPolicy {
          background-color: #FF6B35 !important; color: white !important;
          border: none !important; padding: 6px 12px !important;
          border-radius: 4px !important; font-size: 14px !important;
        }
        #downloadPolicy:hover { background-color: #e55a2b !important; }
      "))
    ),

    tabBox(
      width = NULL, id = "RP1",

      # ---- Sub-tab 1: Policy Repository -------------------------
      tabPanel(
        title = "1. Policy Repository", value = "RP_tab1",
        useShinyjs(),

        # Product selector (full width)
        fluidRow(column(12, product_selector_ui("pr"))),

        fluidRow(
          column(width = 12,
            div(
              class = "filter-card",
              style = "display:flex; align-items:center; justify-content:center; gap:15px;",
              tags$label("Add New Policy",
                style = "font-weight:600; color:#1e293b; margin-bottom:0;"),
              fileInput(
                inputId     = "addPolicyFile",
                label       = NULL,
                accept      = c(".pdf", ".doc", ".docx"),
                placeholder = "Select file...",
                buttonLabel = tags$span("+ Add Policy", style = "color:white;"),
                width       = "300px"
              ),
              tags$p(style = "font-size:11px; color:#64748b; margin:0;",
                "Upload PDF/Word policy doc")
            )
          )
        ),

          fluidRow(column(12, uiOutput("activePolicyBanner"))),
          br(),
          uiOutput("statusCards"),
          br(),

          # Search bar
          fluidRow(column(12,
            div(
              style = "display:flex; align-items:center; gap:15px; margin:0 20px;",
              div(
                style = "flex-grow:1; position:relative;",
                tags$input(
                  type        = "text",
                  id          = "searchText",
                  class       = "form-control",
                  placeholder = "Search by Policy ID or name...",
                  style       = "padding-left:45px !important; width:100%; font-size:14px; height:38px;"
                ),
                tags$i(
                  class = "fa fa-search",
                  style = "position:absolute; left:14px; top:50%; transform:translateY(-50%); color:#666; font-size:14px; pointer-events:none;"
                )
              ),
              actionButton("filterBtn", "Search",
                class = "btn-outline-secondary",
                style = "border:1px solid #666; color:#666; height:38px;")
            )
          )),
          br(),

          # Policy table
          fluidRow(column(12,
            div(
              style = "margin:30px 20px 10px 20px; padding:15px; background-color:#f8f9fa; border-radius:8px; box-shadow:0 2px 4px rgba(0,0,0,0.1); overflow:hidden;",
              h4("Policy Overview", style = "margin-top:0; margin-bottom:15px;"),
              DT::dataTableOutput("policyTable"),
              div(
                style = "display:flex; justify-content:flex-end; margin-top:15px;",
                downloadButton("downloadPolicy", "Download Policy (.R)",
                  icon = icon("download"))
              )
            )
          )),
          br(),

          # KPI filter bar (conditional on row selection)
          conditionalPanel(
            condition = "output.policySelected",
            fluidRow(column(12,
              div(class = "card filter-card", style = "margin-top:15px; margin-bottom:15px;",
                fluidRow(
                  column(2, selectInput("timeRange_kpi", "Time Range",
                    choices  = c("Daily"="daily","Weekly"="weekly","Monthly"="monthly",
                                 "Quarterly"="quarterly","Yearly"="yearly"),
                    selected = "monthly", width = "100%")),
                  column(2, pickerInput("timecohort_kpi", "Time Cohorts",
                    choices = character(0),
                    options = list(`actions-box`=TRUE, `selected-text-format`="count > 3"),
                    multiple = TRUE, width = "100%")),
                  column(2, pickerInput("ficoRange_kpi", "FICO Range",
                    choices = character(0),
                    options = list(`actions-box`=TRUE),
                    multiple = TRUE, width = "100%")),
                  column(2, pickerInput("partner_kpi", "Partner",
                    choices = character(0),
                    options = list(`actions-box`=TRUE),
                    multiple = TRUE, width = "100%")),
                  column(2, pickerInput("repeatType_kpi", "Repeat Type",
                    choices = character(0),
                    options = list(`actions-box`=TRUE),
                    multiple = TRUE, width = "100%"))
                )
              )
            )),
            br(),
            uiOutput("policyKpiTiles"),
            br()
          ),

          # Charts (conditional on row selection)
          conditionalPanel(
            condition = "output.policySelected",
            fluidRow(
              column(6, div(class = "card",
                h4("Funnel Metrics", style = "font-weight:bold;"),
                DTOutput("repo_funnel_table")
              )),
              column(6, div(class = "card",
                uiOutput("repo_irr_title"),
                selectInput("repo_irr_level", NULL,
                  choices = c("Overall",
                              "FICO Band"        = "fico_band",
                              "Partner Channel"  = "partner_channel",
                              "Grade"            = "risk_grade",
                              "Repeat Type"      = "borrower_repeat_type"),
                  selected = "partner_channel", width = "200px"),
                plotlyOutput("repo_irr_plot", height = "280px")
              ))
            ),
            br(),
            fluidRow(
              column(12, div(class = "card",
                h4("Vintage Performance Curve", style = "font-weight:bold;"),
                pickerInput("repo_vintage_quarters", "Select Vintages:",
                  choices = NULL, selected = NULL, multiple = TRUE,
                  options = list(`actions-box` = TRUE, `live-search` = TRUE,
                                 `selected-text-format` = "count > 3",
                                 `count-selected-text`  = "{0} vintages selected"),
                  width = "100%"),
                plotlyOutput("repo_vintage_plot", height = "320px")
              ))
            )
          )
      ), # end tabPanel 1

      # ---- Sub-tab 2: Policy Overlap ----------------------------
      tabPanel(
        title = "2. Policy Overlap", value = "RP_tab2",
        fluidRow(column(width = 12,

          # Row 1: Product selector (full width)
          fluidRow(column(12, product_selector_ui("ro"))),

          # Row 2: Overlap Analysis section header
          tags$h4("Overlap Analysis",
            class = "jumbotron-heading text-bold text-center",
            style = "margin-bottom:0; margin-top:12px;"),
          tags$hr(style = "border-top:1px solid #ccc; width:98%; margin:10px auto;"),

          # Row 3: Policy checkbox group + Compare button
          fluidRow(class = "ro-filter-row",
            column(width = 9,
              div(class = "filter-card",
                uiOutput("ro_policy_filter_ui"))
            ),
            column(width = 3,
              div(style = "padding-top:24px;",
                actionButton("ro_run_overlap", "Compare Policies",
                  icon = icon("scale-balanced"),
                  style = "background-color:#fb6e36; color:white; border:none; font-weight:bold; padding:10px 24px; border-radius:6px; width:100%;")
              )
            )
          ),

          # Overlap correlation output
          # Always rendered; uiOutput("corr_plot1") shows a placeholder until
          # the user explicitly clicks Compare Policies.
          div(id = "corr_plot_block",
            uiOutput("corr_plot1")
          ),

          # Policy comparison section
          div(class = "card",
            fluidRow(
              tags$div(class = "text-center", style = "margin:0 10px;",
                tags$h4("Policy Comparison",
                  class = "jumbotron-heading text-bold text-center",
                  style = "margin-bottom:0; margin-top:0;"),
                tags$hr(style = "border-top:1px solid #ccc; width:98%; margin:10px auto;")
              )
            ),
            fluidRow(align = "center",
              column(width = 6, align = "center",
                uiOutput("ro_policy1_ui")
              ),
              column(width = 6, align = "center",
                style = "border-left:2px dashed #CBCBCB;",
                uiOutput("ro_policy2_ui")
              )
            ),
            fluidRow(align = "center", style = "margin-top:8px; margin-bottom:8px;",
              column(12, align = "center",
                actionButton("ro_compare_pair", "Compare these two",
                  icon = icon("scale-balanced"),
                  style = paste0(
                    "background-color:#FF6B35; color:white; border:none;",
                    " font-weight:bold; padding:8px 22px; border-radius:6px;"))
              )
            ),
            # Placeholder shown BEFORE two policies are selected and compared.
            # Previously this area rendered as two empty outlined boxes with no
            # context, which read as a broken UI. The placeholder explains the
            # next action; it hides the moment section2 is revealed.
            div(id = "ro_compare_placeholder",
              class = "card",
              style = paste0(
                "margin-top:10px; padding:24px; text-align:center;",
                "background:#f9fafb; border:1px dashed #cbd5e1;",
                "color:#475569; font-size:13px;"
              ),
              tags$div(style = "font-size:20px; color:#94a3b8; margin-bottom:6px;",
                icon("scale-balanced")),
              tags$div(tags$b("No comparison yet.")),
              tags$div(style = "margin-top:4px;",
                "Pick two policies above and click ",
                tags$b("Compare"), " to see side-by-side parameter, funnel, ",
                "and KPI differences.")
            ),
            hidden(div(id = "section2",
              # KPI info cards first
              fluidRow(class = "card", align = "center", style = "margin-top:10px;",
                fluidRow(
                  column(6, align = "center", uiOutput("rule1_metrics")),
                  column(6, align = "center", uiOutput("rule2_metrics"))
                )
              ),
              # Section comparison + Funnel metrics side by side
              fluidRow(class = "ro-tables-row", style = "margin-top:10px;",
                column(width = 6,
                  div(class = "card ro-table-card", style = "padding:10px;",
                    tags$h4("Sections within Policy",
                      class = "jumbotron-heading text-bold text-center",
                      style = "margin-bottom:0; margin-top:0; font-size:15px;"),
                    tags$hr(style = "border-top:1px solid #ccc; width:98%; margin:8px auto;"),
                    dataTableOutput("rule2table1")
                  )
                ),
                column(width = 6,
                  div(class = "card ro-table-card", style = "padding:10px;",
                    tags$h4("Funnel Metrics",
                      class = "jumbotron-heading text-bold text-center",
                      style = "margin-bottom:0; margin-top:0; font-size:15px;"),
                    tags$hr(style = "border-top:1px solid #ccc; width:98%; margin:8px auto;"),
                    DTOutput("ro_funnel_table")
                  )
                )
              ),
              # AI Policy Comparison Narrative (UC6)
              fluidRow(style = "margin-top:10px;",
                column(12, uiOutput("ro_comparison_narrative"))
              )
            ))
          )
        ))
      ) # end tabPanel 2
    ) # end tabBox
  ) # end tabItem repos_1
}

policy_repo_server <- function(input, output, session,
                               credit_data, policy_input_data, perf_tape,
                               added_policy, policy_fns_rv) {

  # Build from service functions instead of relying on globals
  mockPolicies <- build_mock_policies(strategy_overrides)
  base_loans   <- build_base_loans(credit_data)
  vintage_raw  <- build_vintage_data(base_loans, mockPolicies)

  # Reactive: currently selected product for the repository
  selected_product_PR  <- reactiveVal("PL")
  uploaded_policy_file <- reactiveVal(NULL)
  extracted_levers <- reactiveVal(NULL)  # set by addPolicyFile observer

  # Helper: persist added policies to disk (metadata only, no closures).
  # Closures capture entire Shiny environments and can balloon to 100s of MB.
  # Also persists the status_overrides map (policyId -> status) so promotions
  # via "Set as Active" survive app restarts.
  persist_policies <- function() {
    pol_df <- added_policy()
    so_rv  <- session$userData$policy_status_overrides
    so_map <- if (!is.null(so_rv)) so_rv() else list()
    tryCatch({
      payload <- list(
        policies         = if (is.null(pol_df)) data.frame() else pol_df,
        fns              = list(),
        status_overrides = so_map
      )
      saveRDS(payload, saved_policies_path)
      persisted_policies <<- if (is.null(pol_df)) persisted_policies else pol_df
      persisted_fns      <<- list()
      if (isTRUE(getOption("csm_debug"))) {
        message("[persist] Saved ", if (is.null(pol_df)) 0 else nrow(pol_df),
                " policies + ", length(so_map), " status overrides to ",
                saved_policies_path)
      }
    }, error = function(e) {
      # Keep error surfaced regardless of debug option — failed persistence
      # matters for governance and should be visible.
      message("[persist] Failed to save: ", e$message)
    })
  }

  # Helper: apply policy_status_overrides onto a policy data.frame. Used by
  # pr_filtered() and active_policy_banner() so "Set as Active" flips the
  # status in every view without mutating mockPolicies.
  apply_status_overrides <- function(df) {
    so_rv <- session$userData$policy_status_overrides
    if (is.null(so_rv)) return(df)
    so <- so_rv()
    if (length(so) == 0) return(df)
    ids <- names(so)
    m <- match(df$policyId, ids)
    df$status[!is.na(m)] <- unlist(so)[m[!is.na(m)]]
    df
  }

  # Product filter — uses shared product_selector_ui("pr")
  # Map full product name to code used by mockPolicies
  observeEvent(input$pr_product, {
    code <- product_name_to_code[input$pr_product]
    if (!is.na(code)) selected_product_PR(code)
  })

  # Filtered policy table (search + status normalisation)
  # Snapshot of added_policy that only updates when the user is on Policy Repository
  # This prevents expensive downstream renders (DT table, pickers, banners) from
  # firing while the user is on the Smart Recommender or other tabs.
  pr_added_snapshot <- reactiveVal(NULL)
  observe({
    ap <- added_policy()
    # Always update if we're on the Policy Repository tab
    if (isTRUE(input$sidebar_menu == "repos_1")) {
      pr_added_snapshot(ap)
    } else {
      # First load: set initial value
      if (is.null(isolate(pr_added_snapshot())) && !is.null(ap)) {
        pr_added_snapshot(ap)
      }
    }
  })
  # Refresh snapshot whenever user navigates to Policy Repository
  observeEvent(input$sidebar_menu, {
    if (isTRUE(input$sidebar_menu == "repos_1")) {
      pr_added_snapshot(added_policy())
    }
  }, ignoreInit = TRUE)

  pr_filtered <- reactive({
    df <- mockPolicies
    snap <- pr_added_snapshot()
    if (!is.null(snap)) df <- bind_rows(df, snap)

    # Apply user-driven status overrides ("Set as Active" promotions) first so
    # active/retired decisions survive downstream filters and de-dup logic.
    df <- apply_status_overrides(df)

    # Filter by selected product
    prod <- selected_product_PR()
    if (!is.null(prod) && prod != "") {
      df <- df %>% filter(product == prod)
    }

    # Ensure only one "active" per product
    df <- df %>%
      arrange(desc(status == "active"), desc(id)) %>%
      mutate(status = ifelse(status == "active" & row_number() > 1, "draft", status))

    if (!is.null(input$searchText) && nchar(trimws(input$searchText)) > 0) {
      s  <- tolower(trimws(input$searchText))
      df <- df %>% filter(str_detect(tolower(policyId), s) |
                            str_detect(tolower(policyName), s))
    }

    df
  })

  # Active policy banner for selected product.
  # Uses REAL effective dates sourced from strategy_manifest.R via mockPolicies
  # (effective_from / effective_to / updatedAt). No fabricated dates — this is a
  # governance tool and any banner value must be traceable to the manifest.
  active_policy_banner <- reactive({
    req(selected_product_PR(), selected_product_PR() != "")
    snap <- pr_added_snapshot()
    df <- if (!is.null(snap)) bind_rows(mockPolicies, snap) else mockPolicies
    df <- apply_status_overrides(df) %>%
      filter(product == selected_product_PR()) %>%
      arrange(desc(status == "active"), desc(id)) %>%
      mutate(status = ifelse(status == "active" & row_number() > 1, "draft", status))
    active_row <- df %>% filter(status == "active") %>% slice(1)
    if (nrow(active_row) == 0) return(NULL)

    # Real effective_from from strategy_manifest (may be NA for non-PL policies
    # that have not been assigned deployment dates yet).
    effective <- if ("effective_from" %in% names(active_row)) {
      as.Date(active_row$effective_from[[1]])
    } else {
      as.Date(NA)
    }
    # Real updatedAt (falls back to effective_from in 06_global_policy_repo_data.R
    # when effective_from is present, otherwise Sys.Date() at app build time).
    last_upd <- if ("updatedAt" %in% names(active_row)) {
      as.Date(active_row$updatedAt[[1]])
    } else {
      as.Date(NA)
    }
    list(policyId   = active_row$policyId[[1]],
         policyName = active_row$policyName[[1]],
         effectiveDate = effective,
         lastUpdated   = last_upd)
  })

  output$activePolicyBanner <- renderUI({
    info <- active_policy_banner()
    req(info)
    eff_txt <- if (is.null(info$effectiveDate) || is.na(info$effectiveDate)) {
      "Effective date not set"
    } else {
      paste("Effective", format(info$effectiveDate, "%Y-%m-%d"))
    }
    upd_txt <- if (is.null(info$lastUpdated) || is.na(info$lastUpdated)) {
      "\u2014"
    } else {
      format(info$lastUpdated, "%Y-%m-%d")
    }
    div(
      style = "background-color:#FF6B35; color:white; padding:15px; border-radius:8px; margin:0; display:flex; justify-content:space-between; align-items:center;",
      div(style = "display:flex; flex-direction:column;",
        span("Current Active Policy", style = "font-weight:500; font-size:14px;"),
        div(style = "display:flex; gap:10px; margin-top:5px;",
          span(info$policyId, style = "font-weight:500;"),
          span("\u2022"),
          span(info$policyName, style = "font-weight:500;"),
          span("\u2022"),
          span(eff_txt, style = "font-weight:500;")
        )
      ),
      div(style = "text-align:right;",
        span("Last Updated", style = "font-weight:500; font-size:14px; display:block;"),
        span(upd_txt, style = "font-weight:500; font-size:14px;")
      )
    )
  })

  # Status summary cards
  status_summary <- reactive({
    df <- pr_filtered()
    st <- tolower(as.character(df$status))
    st[is.na(st)] <- "unknown"
    list(
      accepted  = as.integer(sum(st == "active")),
      simulated = as.integer(sum(st == "simulated")),
      retired   = as.integer(sum(st == "retired"))
    )
  })

  output$statusCards <- renderUI({
    counts <- status_summary()
    fluidRow(
      column(4, div(class = "custom-height-box",
        div(class = "custom-value-box",
          icon("clock", style = "font-size:28px;"),
          div(span(paste0("Simulated (Pending) - ", counts$simulated),
            style = "font-weight:500; font-size:18px;"))
        )
      )),
      column(4, div(class = "custom-height-box",
        div(class = "custom-value-box",
          icon("check-circle", style = "font-size:28px;"),
          div(span(paste0("Accepted (Active) - ", counts$accepted),
            style = "font-weight:500; font-size:18px;"))
        )
      )),
      column(4, div(class = "custom-height-box",
        div(class = "custom-value-box",
          icon("archive", style = "font-size:28px;"),
          div(span(paste0("Retired - ", counts$retired),
            style = "font-weight:500; font-size:18px;"))
        )
      ))
    )
  })

  # Policy table
  output$policyTable <- DT::renderDT({
    df <- pr_filtered() %>%
      select(policyId, policyName, status, updatedAt, endDate)

    # endDate: show formatted date only for retired rows; blank otherwise
    df$endDate <- ifelse(
      tolower(df$status) == "retired" & !is.na(df$endDate),
      format(as.Date(df$endDate), "%Y-%m-%d"),
      ""
    )

    # "Set as Active" button — shown for every non-active row, including
    # retired ones so users can undo a mistaken promotion. Active row shows
    # a label instead of a button.
    df$activateBtn <- dplyr::case_when(
      tolower(df$status) == "active"  ~ '<span style="color:#137333;font-weight:600;font-size:12px;">\u2713 Active</span>',
      TRUE ~ paste0(
        '<button class="btn btn-sm activate-policy-btn" ',
        'style="background-color:#FF6B35;color:white;border:none;border-radius:4px;padding:3px 10px;font-size:12px;" ',
        'data-policy-id="', df$policyId, '">Set as Active</button>'
      )
    )

    df$status <- dplyr::case_when(
      tolower(df$status) == "active"    ~ '<span style="background-color:#e6f4ea;color:#137333;padding:3px 8px;border-radius:4px;">Active</span>',
      tolower(df$status) == "draft"     ~ '<span style="background-color:#fff4e6;color:#873e00;padding:3px 8px;border-radius:4px;">Draft</span>',
      tolower(df$status) == "simulated" ~ '<span style="background-color:#e8f0fe;color:#1967d2;padding:3px 8px;border-radius:4px;">Simulated</span>',
      tolower(df$status) == "retired"   ~ '<span style="background-color:#fce8e6;color:#a52a2a;padding:3px 8px;border-radius:4px;">Retired</span>',
      TRUE                              ~ '<span style="background-color:#f1f3f4;color:#5f6368;padding:3px 8px;border-radius:4px;">Unknown</span>'
    )
    DT::datatable(
      df,
      colnames = c(
        "POLICY ID"       = "policyId",
        "POLICY NAME"     = "policyName",
        "STATUS"          = "status",
        "EFFECTIVE START" = "updatedAt",
        "END DATE"        = "endDate",
        "ACTION"          = "activateBtn"
      ),
      escape    = -c(3, 6),
      selection = "none",
      rownames  = FALSE,
      filter    = "none",
      options   = list(
        pageLength = 10, scrollX = TRUE, dom = "tp",
        columnDefs = list(list(className = "dt-center", targets = c(3, 4, 5))),
        initComplete = JS("
          function(settings, json) {
            var table = this.api().table();
            $(table.body()).on('click', 'tr', function(e) {
              if ($(e.target).closest('button').length) return;
              $('tbody tr').removeClass('selected');
              $(this).addClass('selected');
              var policyId = table.row(this).data()[0];
              Shiny.setInputValue('policyTable_policy_id', policyId, {priority: 'event'});
            });
            $(table.body()).on('click', '.activate-policy-btn', function(e) {
              e.stopPropagation();
              var pid = $(this).data('policy-id');
              Shiny.setInputValue('policyTable_activate_id', pid, {priority: 'event'});
            });
          }
        ")
      )
    )
  }, server = TRUE)

  # "Set as Active" click: show a confirmation modal first. The actual
  # promotion happens in the confirm observer below so a stray click can't
  # silently flip the production policy.
  pending_activation <- reactiveVal(NULL)

  observeEvent(input$policyTable_activate_id, {
    pid <- input$policyTable_activate_id
    req(pid, nzchar(pid))

    so_rv <- session$userData$policy_status_overrides
    if (is.null(so_rv)) return()

    snap <- pr_added_snapshot()
    all_df <- if (!is.null(snap)) bind_rows(mockPolicies, snap) else mockPolicies
    all_df <- apply_status_overrides(all_df)
    target <- all_df %>% filter(policyId == pid)
    if (nrow(target) == 0) return()
    prod <- target$product[[1]]

    cur_active <- all_df %>%
      filter(product == prod, tolower(status) == "active")

    pending_activation(list(
      pid        = pid,
      policyName = target$policyName[[1]],
      product    = prod,
      curActive  = if (nrow(cur_active) > 0) cur_active$policyName[[1]] else NULL,
      curActiveId = if (nrow(cur_active) > 0) cur_active$policyId[[1]] else NULL
    ))

    showModal(modalDialog(
      title = "Confirm: Set as Active",
      tags$p(
        "You are about to make ",
        tags$b(target$policyName[[1]]),
        " the active policy for ", tags$b(prod), "."
      ),
      if (nrow(cur_active) > 0) tags$p(
        "The current active policy ",
        tags$b(cur_active$policyName[[1]]),
        " will be retired."
      ),
      tags$p(style = "color:#a52a2a;font-size:13px;",
             "This change is persisted and visible across the app."),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_activate_policy", "Set as Active",
                     class = "btn-primary",
                     style = "background-color:#FF6B35;border-color:#FF6B35;")
      ),
      easyClose = TRUE
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$confirm_activate_policy, {
    info <- pending_activation()
    req(info)
    removeModal()

    so_rv <- session$userData$policy_status_overrides
    if (is.null(so_rv)) return()
    so <- so_rv()
    if (!is.null(info$curActiveId)) {
      so[[info$curActiveId]] <- "retired"
    }
    so[[info$pid]] <- "active"
    so_rv(so)

    pr_added_snapshot(added_policy())
    persist_policies()

    showNotification(
      paste0("Policy '", info$policyName, "' set as Active for ", info$product,
             if (!is.null(info$curActive))
               paste0(" (retired '", info$curActive, "')") else ""),
      type = "message", duration = 4
    )
    pending_activation(NULL)
  }, ignoreInit = TRUE)

  output$policySelected <- reactive({ !is.null(input$policyTable_policy_id) })
  outputOptions(output, "policySelected",  suspendWhenHidden = FALSE)

  observe({
    enabled <- !is.null(input$policyTable_policy_id)
    if (enabled) {
      shinyjs::runjs("$('#downloadPolicy').removeClass('dl-disabled').css('pointer-events','auto').css('opacity','1');")
    } else {
      shinyjs::runjs("$('#downloadPolicy').addClass('dl-disabled').css('pointer-events','none').css('opacity','0.5');")
    }
  })


  # Download selected policy as .R code document
  output$downloadPolicy <- downloadHandler(
    filename = function() {
      pid <- isolate(input$policyTable_policy_id)
      if (is.null(pid)) return("policy.R")
      df  <- isolate(pr_filtered())
      row <- df[df$policyId == pid, , drop = FALSE]
      if (nrow(row) == 0) return("policy.R")
      nm  <- gsub("[^A-Za-z0-9_ -]", "_", row$policyName[1])
      paste0("credit_policy_", gsub("\\s+", "_", trimws(nm)), ".R")
    },
    content = function(file) {
      pid <- isolate(input$policyTable_policy_id)
      if (is.null(pid)) { writeLines("# No policy selected.", file); return() }
      df  <- isolate(pr_filtered())
      row <- df[df$policyId == pid, , drop = FALSE]
      if (nrow(row) == 0) { writeLines("# Policy not found.", file); return() }
      row <- row[1, ]
      gv  <- function(col) {
        if (!col %in% names(row)) return("[Not Set]")
        v <- row[[col]]
        if (is.null(v) || length(v) == 0) return("[Not Set]")
        if (is.na(v) || v == "") return("[Not Set]")
        as.character(v)
      }
      esc <- function(s) gsub('"', '\\\\"', s)

      # Header
      header <- c(
        paste0("# ", paste(rep("=", 70), collapse = "")),
        paste0("# Credit Policy: ", gv("policyName")),
        paste0("# Policy ID:     ", gv("policyId")),
        paste0("# Product:       ", gv("product")),
        paste0("# Status:        ", gv("status")),
        paste0("# Version:       ", gv("version")),
        paste0("# Effective:     ", gv("updatedAt")),
        paste0("# Exported:      ", Sys.time()),
        paste0("# ", paste(rep("=", 70), collapse = "")),
        ""
      )

      # Strategy 1: Policy has a source .R file on disk — include full code
      r_file <- gv("r_logic")
      if (r_file != "[Not Set]" && file.exists(r_file)) {
        source_lines <- readLines(r_file, warn = FALSE)
        lines <- c(header,
          paste0("# Source: ", r_file),
          paste0("# ", paste(rep("-", 70), collapse = "")),
          "", source_lines)
        writeLines(lines, file)
        return()
      }

      # Strategy 2: Policy has a function in policy_fns_rv — deparse it
      fn <- policy_fns_rv()[[pid]]
      if (!is.null(fn)) {
        fn_code <- tryCatch(deparse(fn, width.cutoff = 120), error = function(e) NULL)
        if (!is.null(fn_code)) {
          lines <- c(header,
            "# ---- Policy Logic ----",
            paste0("# Grading:       ", gv("grading_logic")),
            paste0("# Pricing:       ", gv("pricing_logic")),
            paste0("# Decision:      ", gv("decision_logic")),
            "",
            "apply_credit_policy <- ", fn_code,
            "",
            "# ---- Usage ----",
            "# result <- apply_credit_policy(loan_data)")
          writeLines(lines, file)
          return()
        }
      }

      # Strategy 3: Fallback — metadata-only export
      lines <- c(header,
        "# No executable R logic available for this policy.",
        "# Policy description follows.",
        "",
        paste0("# Grading:       ", gv("grading_logic")),
        paste0("# Pricing:       ", gv("pricing_logic")),
        paste0("# Decision:      ", gv("decision_logic")),
        paste0("# Term:          ", gv("term_logic")),
        paste0("# Affordability: ", gv("affordability_logic")),
        paste0("# Max Amount:    ", gv("max_amount_logic"))
      )
      writeLines(lines, file)
    }
  )
  outputOptions(output, "downloadPolicy", suspendWhenHidden = FALSE)

  # File upload -> modal
  observeEvent(input$addPolicyFile, {
    file <- input$addPolicyFile
    if (is.null(file)) return()
    if (!grepl("\\.(pdf|doc|docx)$", file$name, ignore.case = TRUE)) {
      showNotification("Only .pdf, .doc, or .docx files allowed.",
                       type = "error", duration = 4)
      return()
    }
    ext <- tolower(tools::file_ext(file$name))
    txt <- extract_text_from_file(file$datapath, ext)
    if (is.null(txt)) {
      showNotification(
        "Could not read this file. It may be corrupt, encrypted, or empty.",
        type = "error", duration = 6)
      return()
    }
    levers <- extract_levers(txt)
    uploaded_policy_file(file)
    extracted_levers(levers)
    showModal(policyEditModal())
  })

  policyEditModal <- function() {
    file   <- uploaded_policy_file()
    levers <- extracted_levers()
    req(file, levers)

    n_levers <- levers$n_levers %||% 0L
    banner <- if (n_levers >= 1) {
      tags$div(style = paste0(
        "background:#e8f5e9; border-left:3px solid #2e7d32;",
        " padding:8px 12px; margin-bottom:10px; border-radius:0 4px 4px 0;",
        " color:#1b5e20; font-size:12px;"),
        tags$b("Auto-detected ", n_levers, " lever",
               if (n_levers == 1) "" else "s",
               " from your document."), " Review and edit before saving.")
    } else {
      tags$div(style = paste0(
        "background:#fff8e1; border-left:3px solid #f9a825;",
        " padding:8px 12px; margin-bottom:10px; border-radius:0 4px 4px 0;",
        " color:#7c5e00; font-size:12px;"),
        tags$b("No levers detected in this document."),
        " Please fill in the policy logic manually below.")
    }

    detected_summary <- if (n_levers >= 1) {
      bullets <- c()
      if (!is.na(levers$values$fico_min)) bullets <- c(bullets,
        sprintf("FICO floor: %d", levers$values$fico_min))
      if (!is.na(levers$values$dti_max)) bullets <- c(bullets,
        sprintf("DTI cap: %g%%", levers$values$dti_max))
      if (!is.na(levers$values$hard_inq_max)) bullets <- c(bullets,
        sprintf("Hard inquiries (6M): %d", levers$values$hard_inq_max))
      if (!is.na(levers$values$util_max)) bullets <- c(bullets,
        sprintf("Utilization: %g%%", levers$values$util_max))
      if (!is.na(levers$values$base_apr)) bullets <- c(bullets,
        sprintf("Base APR: %g%%", levers$values$base_apr))
      if (length(bullets) == 0) bullets <- "(see boxes on the right)"
      tagList(lapply(bullets, function(b) tags$li(b, style = "font-size:12px;")))
    } else {
      tags$em("No levers auto-detected.", style = "color:#94a3b8; font-size:12px;")
    }

    modalDialog(
      title     = h4("Add New Policy", style = "margin-top:0; color:#1e293b;"),
      size      = "l",
      easyClose = FALSE,
      footer    = tagList(
        actionButton("savePolicyBtn", "Save Changes",
          style = "background-color:#FF6B35; color:white; border:none; padding:6px 20px; border-radius:4px;"),
        modalButton("Cancel")
      ),
      banner,
      fluidRow(
        column(6,
          div(style = "border:1px solid #e2e8f0; border-radius:8px; padding:15px; background:#f8fafc; height:100%;",
            h5("Document Preview", style = "color:#475569; margin-bottom:12px;"),
            div(style = "background:white; border-radius:6px; padding:12px; min-height:300px; overflow-y:auto;",
              tags$p(strong("File:"), file$name),
              tags$p(strong("Size:"), format(file$size / 1024, digits = 2), "KB"),
              tags$hr(),
              tags$div(style = "background:#fff7ed; border-left:3px solid #FF6B35; padding:10px; border-radius:0 4px 4px 0;",
                tags$p(tags$b("Detected levers:")),
                tags$ul(detected_summary)
              )
            )
          )
        ),
        column(6,
          div(style = "border:1px solid #e2e8f0; border-radius:8px; padding:15px; background:white; height:100%;",
            h5("Human in the Loop", style = "color:#475569; margin-bottom:12px;"),
            textInput("edit_policyName", "Policy Name *",
              value = tools::file_path_sans_ext(file$name), width = "100%"),
            selectInput("edit_product", "Product *",
              choices  = c("Personal Loan"="PL","Top-Up"="TU","Credit Card"="CC","Mortgage"="MTG"),
              selected = "PL", width = "100%"),
            textInput("edit_version", "Version", value = "v1.0", width = "100%"),
            dateInput("edit_effectiveDate", "Effective Date *",
              value = Sys.Date() + 30, width = "100%"),
            selectInput("edit_status", "Policy Type",
              choices  = c("Simulated","Active"),
              selected = "Simulated", width = "100%"),
            tags$label("Grading Logic", style = "font-weight:600; margin-top:8px;"),
            textAreaInput("edit_grading", label = NULL,
              value = levers$lines$grading %||% "",
              height = "70px", width = "100%"),
            tags$label("Pricing Logic", style = "font-weight:600; margin-top:8px;"),
            textAreaInput("edit_pricing", label = NULL,
              value = levers$lines$pricing %||% "",
              height = "60px", width = "100%"),
            tags$label("Decision Logic", style = "font-weight:600; margin-top:8px;"),
            textAreaInput("edit_decision", label = NULL,
              value = levers$lines$decision %||% "",
              height = "70px", width = "100%"),
            tags$label("R Logic (Risk Rules)", style = "font-weight:600; margin-top:8px;"),
            textAreaInput("edit_r_logic", label = NULL,
              value = levers$lines$r_logic %||% "",
              height = "60px", width = "100%")
          )
        )
      )
    )
  }

  observeEvent(input$savePolicyBtn, {
    req(input$edit_policyName, input$edit_product, input$edit_effectiveDate)
    new_id <- max(c(mockPolicies$id,
                    if (!is.null(added_policy())) max(added_policy()$id) else 0L),
                  na.rm = TRUE) + 1L
    new_policy <- data.frame(
      id            = new_id,
      policyId      = paste0(input$edit_product, "-", sprintf("%03d", new_id)),
      policyName    = trimws(input$edit_policyName),
      status        = "simulated",
      product       = input$edit_product,
      version       = trimws(if (nchar(input$edit_version) > 0) input$edit_version else "v1.0"),
      updatedAt     = as.Date(input$edit_effectiveDate),
      grading_logic  = trimws(input$edit_grading  %||% ""),
      pricing_logic  = trimws(input$edit_pricing  %||% ""),
      decision_logic = trimws(input$edit_decision %||% ""),
      r_logic        = trimws(input$edit_r_logic  %||% ""),
      stringsAsFactors = FALSE
    )
    existing <- added_policy()
    if (!is.null(existing)) {
      added_policy(bind_rows(existing, new_policy))
    } else {
      added_policy(new_policy)
    }

    # Register the baseline policy function for the new policy so it appears
    # in the Policy Overlap tab and can be simulated
    pid <- new_policy$policyId
    baseline_fn <- policy_fns_rv()[["PL-001"]]
    if (!is.null(baseline_fn) && new_policy$product == "PL") {
      fns <- policy_fns_rv()
      fns[[pid]] <- baseline_fn
      policy_fns_rv(fns)
    }

    # Persist to disk so policies survive app restart
    persist_policies()

    removeModal()
    showNotification("Policy added to repository with status: Simulated",
      type = "message", duration = 3)
  })

  # Dynamic filter choices for KPI panel
  observe({
    fico_ch    <- sort(unique(na.omit(as.character(base_loans$score_band))))
    partner_ch <- sort(unique(na.omit(as.character(base_loans$partner))))
    repeat_ch  <- sort(unique(na.omit(as.character(base_loans$repeat_type))))
    updatePickerInput(session, "ficoRange_kpi",  choices = fico_ch,    selected = fico_ch)
    updatePickerInput(session, "partner_kpi",    choices = partner_ch, selected = partner_ch)
    updatePickerInput(session, "repeatType_kpi", choices = repeat_ch,  selected = repeat_ch)
  })

  # Dynamic time cohort choices
  observe({
    req(input$timeRange_kpi)
    dates <- base_loans$vintage_date[!is.na(base_loans$vintage_date)]
    cohorts <- switch(input$timeRange_kpi,
      monthly   = format(floor_date(dates, "month"), "%b'%y"),
      yearly    = as.character(year(dates)),
      quarterly = paste0("Q", quarter(floor_date(dates, "quarter")), "-",
                         format(floor_date(dates, "quarter"), "%y")),
      weekly    = paste0(isoyear(floor_date(dates, "week", week_start = 1)), "-W",
                         sprintf("%02d", isoweek(floor_date(dates, "week", week_start = 1)))),
      daily     = format(dates, "%d %b'%y")
    )
    choices  <- sort(unique(cohorts))
    selected <- tail(choices, min(18L, length(choices)))
    updatePickerInput(session, "timecohort_kpi", choices = choices, selected = selected)
  })

  # Filtered base_loans for KPI calculation (funded-only, for vintage chart)
  filtered_base_loans_kpi <- reactive({
    req(input$timeRange_kpi, input$timecohort_kpi)
    df <- base_loans %>% filter(!is.na(vintage_date))
    # Filter by selected product (consistent with filtered_all_loans_kpi).
    # A code (e.g. "PL") can map to multiple display names ("Personal Loan",
    # "Personal Loan - Prime", "Personal Loan - Near Prime"); the data uses
    # the canonical "Personal Loan", so match against the full alias set.
    prod_code <- selected_product_PR()
    allowed_names <- names(product_name_to_code)[product_name_to_code == prod_code]
    if (length(allowed_names) > 0 && "product_type" %in% names(df))
      df <- df %>% filter(product_type %in% allowed_names)

    df <- switch(input$timeRange_kpi,
      monthly   = df %>% mutate(cohort = format(floor_date(vintage_date, "month"), "%b'%y")),
      yearly    = df %>% mutate(cohort = as.character(year(vintage_date))),
      quarterly = df %>% mutate(cohort = paste0("Q", quarter(floor_date(vintage_date, "quarter")),
                                                 "-", format(floor_date(vintage_date, "quarter"), "%y"))),
      weekly    = df %>% mutate(cohort = paste0(isoyear(floor_date(vintage_date, "week", week_start = 1)),
                                                "-W", sprintf("%02d", isoweek(floor_date(vintage_date, "week", week_start = 1))))),
      daily     = df %>% mutate(cohort = format(vintage_date, "%d %b'%y"))
    )
    df <- df %>% filter(cohort %in% input$timecohort_kpi)

    if (length(input$ficoRange_kpi)  > 0) df <- df %>% filter(score_band  %in% input$ficoRange_kpi)
    if (length(input$partner_kpi)    > 0) df <- df %>% filter(partner     %in% input$partner_kpi)
    if (length(input$repeatType_kpi) > 0) df <- df %>% filter(repeat_type %in% input$repeatType_kpi)
    df
  })

  # Full application pool (funded + unfunded) for funnel/KPI computation
  # Uses FICO/partner/repeat filters but NOT vintage_date filter,
  # so approval rate and take rate are meaningful (not always 100%).
  filtered_all_loans_kpi <- reactive({
    df <- base_loans
    # Filter by selected product so KPIs match the policy table's product scope.
    # See note above: code -> set of display-name aliases, filter via %in%.
    prod_code <- selected_product_PR()
    allowed_names <- names(product_name_to_code)[product_name_to_code == prod_code]
    if (length(allowed_names) > 0 && "product_type" %in% names(df))
      df <- df %>% filter(product_type %in% allowed_names)
    if (length(input$ficoRange_kpi)  > 0) df <- df %>% filter(score_band  %in% input$ficoRange_kpi)
    if (length(input$partner_kpi)    > 0) df <- df %>% filter(partner     %in% input$partner_kpi)
    if (length(input$repeatType_kpi) > 0) df <- df %>% filter(repeat_type %in% input$repeatType_kpi)
    df
  })

  # Baseline funnel data (reused by KPI tiles and funnel table) — full pool
  baseline_funnel_data <- reactive({
    req(filtered_all_loans_kpi())
    compute_funnel(filtered_all_loans_kpi())
  })

  # Selected policy row (look up by policyId, not row index, to handle pagination)
  selected_policy <- reactive({
    req(input$policyTable_policy_id)
    pid <- input$policyTable_policy_id
    df  <- pr_filtered()
    df[df$policyId == pid, , drop = FALSE]
  })

  # Simulated results: apply selected policy's R function to the filtered data
  simulated_results <- reactive({
    req(selected_policy())
    policy_row <- selected_policy()
    req(nrow(policy_row) > 0)
    pol_id     <- trimws(as.character(policy_row$policyId[[1]]))

    df <- tryCatch(filtered_all_loans_kpi(), error = function(e) base_loans)

    # Apply the selected policy's actual R function if available
    fn <- policy_fns_rv()[[pol_id]]
    if (!is.null(fn)) {
      # R-file policies run the full 9-layer engine from scratch, so they must
      # receive only the 16 raw input columns.  Passing credit_data (which has
      # stale policy output columns like maximum_offered_amount) causes subtle
      # differences on re-run.  Opp-Finder saved closures, on the other hand,
      # need full credit_data (interest_rate_apr, offered_decision_flag, etc.).
      # Strategy: try clean inputs first; fall back to full credit_data.
      ids_mask <- policy_input_data$loan_id %in% df$loan_id
      canon_clean <- policy_input_data[ids_mask, , drop = FALSE]
      pol_result <- tryCatch(fn(canon_clean), error = function(e_clean) {
        canon_full <- credit_data[credit_data$loan_id %in% df$loan_id, , drop = FALSE]
        tryCatch(fn(canon_full), error = function(e_full) {
          # Runtime policy error — always surface for visibility.
          message("[simulated_results] Policy fn ERROR for ", pol_id, ": ", conditionMessage(e_full))
          NULL
        })
      })
      if (isTRUE(getOption("csm_debug"))) {
        message("[simulated_results] pol_id=", pol_id,
                " fn_is_null=", is.null(fn),
                " result_is_null=", is.null(pol_result),
                if (!is.null(pol_result)) paste0(
                  " result_ncol=", ncol(pol_result),
                  " has_offered_final_apr=", "offered_final_apr" %in% names(pol_result)
                ) else "")
        message("[DEBUG] df issued_flag==1 BEFORE merge: ", sum(df$issued_flag == 1, na.rm=TRUE))
      }

      if (!is.null(pol_result)) {
        # Columns to bring across from policy output (include issued_flag
        # and interest_rate_apr so funnel metrics match Opportunity Finder)
        merge_cols <- intersect(
          c("loan_id", "offered_decision_flag", "offered_final_apr",
            "risk_grade", "offered_term_months",
            "issued_flag", "interest_rate_apr",
            "maximum_offered_amount", "affordability_limit_cap",
            "maximum_approved_amount", "final_apr",
            "income_based_limit_cap"),
          names(pol_result)
        )
        # Drop old versions from df (keep loan_id for join)
        df <- df %>%
          select(-any_of(setdiff(merge_cols, "loan_id"))) %>%
          left_join(pol_result[, merge_cols, drop = FALSE], by = "loan_id")

        if (isTRUE(getOption("csm_debug"))) {
          message("[DEBUG] merge_cols: ", paste(merge_cols, collapse=", "))
          message("[DEBUG] df issued_flag==1 AFTER merge: ", sum(df$issued_flag == 1, na.rm=TRUE))
        }

        # For R-file policies that don't output issued_flag: infer it.
        # A loan can't be issued if the new policy declines it.
        if (!"issued_flag" %in% merge_cols && "offered_decision_flag" %in% names(df)) {
          declined_now <- !is.na(df$offered_decision_flag) &
                          df$offered_decision_flag != "APPR"
          if (isTRUE(getOption("csm_debug"))) {
            message("[DEBUG] fallback fired. declined_now count: ", sum(declined_now))
            message("[DEBUG] declined_now AND issued_flag==1: ", sum(declined_now & df$issued_flag == 1, na.rm=TRUE))
          }
          df$issued_flag[declined_now] <- 0L
          if (isTRUE(getOption("csm_debug"))) {
            message("[DEBUG] issued_flag==1 AFTER fallback: ", sum(df$issued_flag == 1, na.rm=TRUE))
          }
        } else if (isTRUE(getOption("csm_debug"))) {
          message("[DEBUG] fallback NOT fired. issued_flag in merge_cols: ", "issued_flag" %in% merge_cols)
        }

        # For R-file policies that don't output interest_rate_apr: sync
        # from offered_final_apr so compute_funnel sees pricing changes.
        if (!"interest_rate_apr" %in% merge_cols && "offered_final_apr" %in% names(df)) {
          approved <- !is.na(df$offered_decision_flag) &
                      df$offered_decision_flag == "APPR"
          df$interest_rate_apr[approved] <- df$offered_final_apr[approved]
        }

        # Sync legacy aliases so KPI tiles and funnel table agree
        if ("offered_final_apr" %in% names(df))
          df$interest_rate <- df$offered_final_apr * 100
        if ("risk_grade" %in% names(df))
          df$grade <- df$risk_grade
        df$take_flag <- df$issued_flag
        if (isTRUE(getOption("csm_debug"))) {
          message("[DEBUG] take_flag sum: ", sum(df$take_flag, na.rm=TRUE))
        }
        df$issue_amount <- ifelse(df$issued_flag == 1, df$issued_amount, 0)
        if (isTRUE(getOption("csm_debug"))) {
          message("[DEBUG] issue_amount total: ", sum(df$issue_amount, na.rm=TRUE))
        }
        df$delinquency_flag <- ifelse(df$issued_flag == 1,
          df$performance_delinquency_flag, 0L)

        # Refresh offer_generated_flag based on new decision
        df$offer_generated_flag <- as.integer(
          !is.na(df$offered_decision_flag) & df$offered_decision_flag == "APPR"
        )
      }
    }

    # proposed_flag = policy decision (for decision breakdown chart)
    df$proposed_flag <- ifelse(
      !is.na(df$offered_decision_flag), df$offered_decision_flag, "APPR"
    )

    # delinquency_amount (always recompute — policy may change issued set)
    df$delinquency_amount <- ifelse(
      !is.na(df$delinquency_flag) & df$delinquency_flag == 1,
      df$issue_amount, 0
    )

    # vintage_year_month for time-series charts
    df$vintage_year_month <- format(
      suppressWarnings(as.Date(df$vintage, "%m/%d/%Y")), "%Y-%m"
    )
    df
  })

  # KPI summary
  # If the selected policy corresponds to a historical regime that exists as a
  # `policy_id` value in the data, compute KPIs from the actual loans that ran
  # under that policy (more accurate than re-simulating).  Otherwise fall back
  # to simulated_results().
  selected_policy_kpis <- reactive({
   tryCatch({
    pol_row <- tryCatch(selected_policy(), error = function(e) NULL)
    pid <- if (!is.null(pol_row) && nrow(pol_row) > 0)
      trimws(as.character(pol_row$policyId[[1]])) else NA_character_

    # Try data-driven path first: filter base_loans by policy_id
    df_actual <- NULL
    if (!is.na(pid) && "policy_id" %in% names(base_loans)) {
      df_actual <- base_loans[base_loans$policy_id == pid, , drop = FALSE]
      # Apply the same product / FICO / partner / repeat filters the KPI
      # panel was using so sidebar slicers still work.
      prod_code <- selected_product_PR()
      allowed_names <- names(product_name_to_code)[product_name_to_code == prod_code]
      if (length(allowed_names) > 0 && "product_type" %in% names(df_actual))
        df_actual <- df_actual %>% filter(product_type %in% allowed_names)
      if (length(input$ficoRange_kpi)  > 0) df_actual <- df_actual %>% filter(score_band  %in% input$ficoRange_kpi)
      if (length(input$partner_kpi)    > 0) df_actual <- df_actual %>% filter(partner     %in% input$partner_kpi)
      if (length(input$repeatType_kpi) > 0) df_actual <- df_actual %>% filter(repeat_type %in% input$repeatType_kpi)
      if (nrow(df_actual) == 0) df_actual <- NULL
    }

    if (!is.null(df_actual)) {
      df <- df_actual
      total_issuance <- sum(df$issued_amount, na.rm = TRUE)
      n_offers <- sum(df$offered_decision_flag == "APPR", na.rm = TRUE)
      n_issued <- sum(df$issued_flag == 1, na.rm = TRUE)
      take_pct <- if (n_offers > 0) 100 * n_issued / n_offers else 0
      issued_mask <- df$issued_flag == 1
      avg_int <- if (sum(issued_mask, na.rm = TRUE) > 0)
        mean(df$interest_rate_apr[issued_mask], na.rm = TRUE) * 100 else 0
      bad_rate <- if (sum(issued_mask, na.rm = TRUE) > 0)
        mean(df$performance_delinquency_flag[issued_mask], na.rm = TRUE) * 100 else 0

      tape <- perf_tape()
      if (!is.null(tape)) {
        irr_raw <- tryCatch(
          compute_adjusted_xirr_v2(df, tape, df),
          error = function(e) NA_real_)
        portfolio_irr <- if (!is.na(irr_raw)) irr_raw * 100 else NA_real_
      } else {
        portfolio_irr <- avg_int
      }

      # Approval rate (for header label)
      n_apps <- nrow(df)
      appr_rate <- if (n_apps > 0) 100 * n_offers / n_apps else NA_real_

      return(list(
        issuance = total_issuance, take_pct = take_pct,
        int_rate = avg_int, bad_rate = bad_rate,
        portfolio_irr = portfolio_irr,
        policy_id = pid, source = "data",
        appr_rate = appr_rate, n_loans = n_apps
      ))
    }

    # ---- Fallback: re-simulation path (unchanged legacy behaviour) ----
    req(simulated_results())
    df <- simulated_results()
    total_issuance <- sum(df$issue_amount,         na.rm = TRUE)
    total_offers   <- sum(df$offer_generated_flag, na.rm = TRUE)
    total_takes    <- sum(df$take_flag,            na.rm = TRUE)
    take_pct    <- if (total_offers > 0) (total_takes / total_offers) * 100 else 0
    # Avg interest rate: simple mean of APR for issued loans (matches funnel table)
    issued_mask  <- df$issued_flag == 1
    avg_int     <- if (sum(issued_mask, na.rm = TRUE) > 0)
                     mean(df$interest_rate_apr[issued_mask], na.rm = TRUE) * 100 else 0
    # Bad rate: loan-count basis (# delinquent issued / # issued)
    bad_rate     <- if (sum(issued_mask, na.rm = TRUE) > 0)
                      mean(df$performance_delinquency_flag[issued_mask], na.rm = TRUE) * 100 else 0

    # Portfolio XIRR from tape (if available), adjusted for APR changes
    tape <- perf_tape()
    if (!is.null(tape)) {
      df_bl <- tryCatch(filtered_all_loans_kpi(), error = function(e) NULL)
      irr_raw <- compute_adjusted_xirr_v2(df, tape, df_bl)
      portfolio_irr <- if (!is.na(irr_raw)) irr_raw * 100 else NA_real_
    } else {
      portfolio_irr <- avg_int
    }

    n_apps <- nrow(df)
    appr_rate <- if (n_apps > 0) 100 * total_offers / n_apps else NA_real_

    list(issuance = total_issuance, take_pct = take_pct,
         int_rate = avg_int, bad_rate = bad_rate,
         portfolio_irr = portfolio_irr,
         policy_id = pid, source = "simulated",
         appr_rate = appr_rate, n_loans = n_apps)
   }, error = function(e) {
     message("[PolicyRepo] selected_policy_kpis: ", conditionMessage(e))
     list(status = "error", message = conditionMessage(e))
   })
  })

  output$policyKpiTiles <- renderUI({
    sim_kpis <- tryCatch(selected_policy_kpis(), error = function(e) {
      message("[PolicyRepo] policyKpiTiles outer: ", conditionMessage(e))
      list(status = "error", message = conditionMessage(e))
    })
    if (is.null(sim_kpis))
      return(div("Loading KPIs...", style = "padding:20px; text-align:center;"))
    if (identical(sim_kpis$status, "error")) {
      return(div(
        style = "padding:14px 18px; margin-bottom:10px; background:#fff1f2; border-left:4px solid #dc2626; border-radius:4px; color:#7f1d1d;",
        tags$b("KPI computation failed: "),
        tags$span(sim_kpis$message %||% "unknown error")
      ))
    }

    irr_label <- if (!is.null(perf_tape())) "Portfolio IRR" else "IRR (APR)"

    # Build a descriptive summary banner when the policy has actual data behind it
    summary_banner <- NULL
    if (!is.null(sim_kpis$policy_id) && !is.na(sim_kpis$policy_id) &&
        identical(sim_kpis$source, "data")) {
      appr_txt  <- if (!is.na(sim_kpis$appr_rate)) sprintf("%.0f%% approval", sim_kpis$appr_rate) else "approval N/A"
      take_txt  <- sprintf("%.0f%% take",   sim_kpis$take_pct)
      bad_txt   <- sprintf("%.1f%% bad rate", sim_kpis$bad_rate)
      n_txt     <- format(sim_kpis$n_loans, big.mark = ",")
      summary_banner <- div(
        style = "margin-bottom:10px; padding:10px 14px; background:#fff8f5; border-left:4px solid #fb6e36; border-radius:4px;",
        tags$b(sprintf("%s actuals:", sim_kpis$policy_id),
               style = "color:#fb6e36;"),
        tags$span(sprintf(" %s, %s, %s  (%s loans)",
                          appr_txt, take_txt, bad_txt, n_txt),
                  style = "color:#333; margin-left:6px;")
      )
    }

    tiles <- fluidRow(
      column(2, kpi_box(
        fmt_dollar_k(sim_kpis$issuance),
        "Issuance"
      )),
      column(2, kpi_box(
        paste0(round(sim_kpis$take_pct, 1), "%"),
        "Take Rate"
      )),
      column(3, kpi_box(
        paste0(round(sim_kpis$int_rate, 1), "%"),
        "Avg Interest Rate"
      )),
      column(2, kpi_box(
        if (is.na(sim_kpis$bad_rate)) "N/A" else paste0(round(sim_kpis$bad_rate, 1), "%"),
        "Bad Rate"
      )),
      column(3, kpi_box(
        if (is.na(sim_kpis$portfolio_irr)) "N/A" else paste0(round(sim_kpis$portfolio_irr, 1), "%"),
        irr_label
      ))
    )
    tagList(summary_banner, tiles)
  })

  # ---- Funnel Metrics table (single column: selected policy results) ----
  output$repo_funnel_table <- DT::renderDT({
   tryCatch({
    req(simulated_results())
    df  <- simulated_results()
    fnl <- compute_funnel(df)
    tape <- perf_tape()

    fp <- function(x) if (is.na(x)) "\u2014" else sprintf("%.1f%%", x * 100)
    fc <- function(x) if (is.na(x)) "\u2014" else fmt_k(x)

    # Compute portfolio XIRR if tape is available (APR-adjusted)
    if (!is.null(tape)) {
      df_bl <- tryCatch(filtered_all_loans_kpi(), error = function(e) NULL)
      irr_val   <- compute_adjusted_xirr_v2(df, tape, df_bl)
      irr_label <- "Portfolio IRR"
    } else {
      irr_val   <- fnl$avg_apr
      irr_label <- "Avg IRR (APR)"
    }

    # Dollar amounts at each funnel level
    issued_mask  <- df$issued_flag == 1
    approved_mask <- !is.na(df$offered_decision_flag) & df$offered_decision_flag == "APPR"

    amt_approved <- if ("maximum_offered_amount" %in% names(df))
                      sum(df$maximum_offered_amount[approved_mask], na.rm = TRUE) else NA_real_
    amt_issued   <- sum(df$issued_amount[issued_mask], na.rm = TRUE)
    amt_delinq   <- sum(df$issued_amount[issued_mask &
                      df$performance_delinquency_flag == 1], na.rm = TRUE)

    # Dollar-weighted rates
    bad_rate_dollar  <- if (amt_issued > 0) amt_delinq / amt_issued else NA_real_
    take_rate_dollar <- if (!is.na(amt_approved) && amt_approved > 0)
                          amt_issued / amt_approved else NA_real_

    # Dollar-weighted avg APR (weighted by issued_amount)
    avg_apr_dollar <- if (amt_issued > 0)
                        weighted.mean(df$interest_rate_apr[issued_mask],
                                      df$issued_amount[issued_mask], na.rm = TRUE)
                      else NA_real_

    # Dollar-weighted IRR: same scalar value (already dollar-weighted by cashflows)
    irr_val_dollar <- irr_val

    fd <- function(x) if (is.na(x)) "\u2014" else fmt_dollar_k(x)

    rows <- list(
      c("Applications",  fc(fnl$apps),       "\u2014"),
      c("Approved",       fc(fnl$approved),   fd(amt_approved)),
      c("Issued",         fc(fnl$issued),     fd(amt_issued)),
      c("Approval Rate",  fp(fnl$appr_rate),  "\u2014"),
      c("Take Rate",      fp(fnl$conv_rate),  fp(take_rate_dollar)),
      c("Bad Rate",        fp(fnl$bad_rate),   fp(bad_rate_dollar)),
      c("Avg APR",        fp(fnl$avg_apr),    fp(avg_apr_dollar)),
      c(irr_label,        fp(irr_val),        fp(irr_val_dollar))
    )
    tbl <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
    names(tbl) <- c("Metric", "# Loans", "$ Amount")

    DT::datatable(tbl, rownames = FALSE, selection = "none",
      options = list(
        dom = "t", pageLength = 10,
        initComplete = JS(
          "function(s,j){",
          "$(this.api().table().header()).css(",
          "  {'background-color':'#fb6e36','color':'white'});",
          "}"
        )
      )
    )
   }, error = function(e) {
     message("[PolicyRepo] repo_funnel_table: ", conditionMessage(e))
     DT::datatable(
       data.frame(Error = paste("Could not compute funnel:", conditionMessage(e))),
       rownames = FALSE, selection = "none",
       options = list(dom = "t")
     )
   })
  })

  # ---- IRR title (dynamic based on tape availability) ----
  output$repo_irr_title <- renderUI({
    tape <- perf_tape()
    if (!is.null(tape)) {
      h4("Portfolio IRR (Cashflow-based)", style = "font-weight:bold;")
    } else {
      h4("Average IRR (APR proxy)", style = "font-weight:bold;")
    }
  })

  # ---- IRR bar chart (single-series: selected policy) ----
  output$repo_irr_plot <- renderPlotly({
   tryCatch({
    req(simulated_results())
    df_sim <- simulated_results()
    level  <- input$repo_irr_level %||% "Overall"
    tape   <- perf_tape()

    level_label <- switch(level,
      "Overall"              = "Overall",
      "fico_band"            = "FICO Band",
      "partner_channel"      = "Partner Channel",
      "risk_grade"           = "Grade",
      "borrower_repeat_type" = "Repeat Type",
      level
    )

    if (!is.null(tape)) {
      # ---- Real XIRR from tape cashflows (APR-adjusted) ----
      df_bl <- tryCatch(filtered_all_loans_kpi(), error = function(e) NULL)
      rows_sub <- compute_adjusted_xirr_v2(df_sim, tape, df_bl, return_rows = TRUE)
      if (is.null(rows_sub) || nrow(rows_sub) == 0)
        return(plot_ly() %>%
                 layout(title = list(text = "IRR could not be computed"),
                        paper_bgcolor = "white", plot_bgcolor = "white"))

      # Join grouping column for non-Overall levels
      if (level != "Overall" && !level %in% names(rows_sub)) {
        grp_map <- df_sim %>%
          filter(issued_flag == 1) %>%
          select(loan_id, all_of(level)) %>%
          mutate(loan_id = as.character(loan_id))
        rows_sub <- rows_sub %>%
          left_join(grp_map, by = "loan_id")
      }

      # Helper: combine observed + projected cashflows for a set of loan IDs
      .combine_obs_proj <- function(obs_rows, ids) {
        obs_agg <- obs_rows %>%
          group_by(payment_date) %>%
          summarise(cf = sum(adj_cf, na.rm = TRUE), .groups = "drop")
        proj_sub <- if (!is.null(tape$projected)) {
          tape$projected[tape$projected$loan_id %in% ids, ]
        } else data.frame(payment_date = as.Date(character()), proj_cf = numeric())
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
        irr_df <- data.frame(Group = "Overall",
                             irr_val = tryCatch(xirr(port$cf, port$payment_date),
                                                error = function(e) NA_real_),
                             n_loans = length(unique(rows_sub$loan_id)),
                             stringsAsFactors = FALSE)
      } else {
        if (!level %in% names(rows_sub))
          return(plot_ly() %>%
                   layout(title = list(text = "No grouping data available"),
                          paper_bgcolor = "white", plot_bgcolor = "white"))
        irr_df <- rows_sub %>%
          filter(!is.na(.data[[level]])) %>%
          group_by(grp = .data[[level]]) %>%
          group_modify(~ {
            port <- .combine_obs_proj(.x, unique(.x$loan_id))
            data.frame(irr_val = tryCatch(xirr(port$cf, port$payment_date),
                                          error = function(e) NA_real_),
                       n_loans = length(unique(.x$loan_id)))
          }) %>%
          ungroup() %>%
          transmute(Group = grp, irr_val, n_loans)
      }

      irr_df <- irr_df %>% filter(!is.na(irr_val))
      if (nrow(irr_df) == 0)
        return(plot_ly() %>%
                 layout(title = list(text = "IRR could not be computed"),
                        paper_bgcolor = "white", plot_bgcolor = "white"))

      # Format N with K suffix (e.g. 2.3K)
      .fmt_n <- function(n) ifelse(n >= 1000, paste0(formatC(n/1000, format = "f", digits = 1), "K"), as.character(n))
      y_max <- max(irr_df$irr_val, na.rm = TRUE) * 1.25  # 25% headroom for labels

      plot_ly(irr_df, x = ~Group, y = ~irr_val, type = "bar",
              marker = list(color = "#fb6e36"),
              customdata = ~n_loans,
              hovertemplate = paste0("<b>%{x}</b><br>IRR: %{y:.1%}",
                                    "<br>N = %{customdata:,}<extra></extra>")) %>%
        layout(
          title   = list(text = paste("Portfolio IRR by", level_label),
                         font = list(size = 12), y = 0.98),
          xaxis   = list(title = list(text = level_label, font = list(size = 10)),
                         tickfont = list(size = 9)),
          yaxis   = list(title = list(text = "IRR (annualised)", font = list(size = 10)),
                         tickformat = ".1%", tickfont = list(size = 9),
                         range = list(0, y_max)),
          legend  = list(font = list(size = 9)),
          margin  = list(l = 50, r = 10, t = 30, b = 40),
          paper_bgcolor = "white", plot_bgcolor = "white"
        )

    } else {
      # ---- Fallback: average APR when no tape ----
      issued <- df_sim %>% filter(issued_flag == 1, !is.na(interest_rate_apr))
      if (nrow(issued) == 0)
        return(plot_ly() %>%
                 layout(title = list(text = "No IRR data available"),
                        paper_bgcolor = "white", plot_bgcolor = "white"))

      if (level == "Overall") {
        irr_df <- data.frame(Group = "Overall",
                             irr_val = mean(issued$interest_rate_apr, na.rm = TRUE),
                             n_loans = nrow(issued), stringsAsFactors = FALSE)
      } else {
        if (!level %in% names(issued))
          return(plot_ly() %>%
                   layout(title = list(text = "No grouping data available"),
                          paper_bgcolor = "white", plot_bgcolor = "white"))
        irr_df <- issued %>%
          filter(!is.na(.data[[level]])) %>%
          group_by(Group = .data[[level]]) %>%
          summarise(irr_val = mean(interest_rate_apr, na.rm = TRUE),
                    n_loans = n(), .groups = "drop")
      }

      .fmt_n2 <- function(n) ifelse(n >= 1000, paste0(formatC(n/1000, format = "f", digits = 1), "K"), as.character(n))
      y_max2 <- max(irr_df$irr_val, na.rm = TRUE) * 1.25

      plot_ly(irr_df, x = ~Group, y = ~irr_val, type = "bar",
              marker = list(color = "#fb6e36"),
              customdata = ~n_loans) %>%
        layout(
          title   = list(text = paste("Avg APR by", level_label, "(no tape)"),
                         font = list(size = 12), y = 0.98),
          xaxis   = list(title = list(text = level_label, font = list(size = 10)),
                         tickfont = list(size = 9)),
          yaxis   = list(title = list(text = "Avg APR", font = list(size = 10)),
                         tickformat = ".1%", tickfont = list(size = 9),
                         range = list(0, y_max2)),
          legend  = list(font = list(size = 9)),
          margin  = list(l = 50, r = 10, t = 30, b = 40),
          paper_bgcolor = "white", plot_bgcolor = "white"
        )
    }
   }, error = function(e) {
     message("[PolicyRepo] repo_irr_plot: ", conditionMessage(e))
     plot_ly() %>%
       layout(
         title = list(text = paste("IRR plot error:", conditionMessage(e)),
                      font = list(size = 12)),
         paper_bgcolor = "white", plot_bgcolor = "white"
       )
   })
  })

  # ---- Vintage quarter picker population ----
  observeEvent(perf_tape(), {
    tape <- perf_tape()
    if (!is.null(tape) && length(tape$vintages) > 0) {
      vqs <- tape$vintages
      updatePickerInput(session, "repo_vintage_quarters",
                        choices = vqs, selected = vqs)
    } else {
      # Synthetic fallback: use vintage months from vintage_raw
      v_months <- sort(unique(format(
        as.Date(vintage_raw$vintage_date, "%m/%d/%Y"), "%Y-%m"
      )))
      v_months <- v_months[!is.na(v_months)]
      updatePickerInput(session, "repo_vintage_quarters",
                        choices = v_months, selected = v_months)
    }
  }, ignoreNULL = FALSE)

  # ---- Vintage performance curve ----
  output$repo_vintage_plot <- renderPlotly({
    tape <- perf_tape()

    sel_vqs <- input$repo_vintage_quarters
    if (is.null(sel_vqs) || length(sel_vqs) == 0) {
      return(plot_ly() %>%
               layout(title = list(text = "Select at least one vintage",
                                   font = list(size = 13)),
                      paper_bgcolor = "white", plot_bgcolor = "white"))
    }

    if (!is.null(tape)) {
      # ---- Real tape path ----
      req(simulated_results())
      df     <- simulated_results()
      ids    <- as.character(df$loan_id[df$issued_flag == 1])
      rows_sub <- tape$rows[tape$rows$loan_id %in% ids &
                            tape$rows$vintage_quarter %in% sel_vqs, ]

      if (nrow(rows_sub) == 0) {
        return(plot_ly() %>%
                 layout(title = list(text = "No vintage data available"),
                        paper_bgcolor = "white", plot_bgcolor = "white"))
      }

      min_dq_mob <- 4L

      # Fixed denominator: cohort size at MOB 0
      cohort_size <- rows_sub %>%
        filter(month_on_books == 0) %>%
        group_by(vintage_quarter) %>%
        summarise(n_loans = n_distinct(loan_id), .groups = "drop")

      # First delinquency MOB per loan (co_amount event, floored at min_dq_mob)
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

      # Max MOB per vintage
      max_mob_v <- rows_sub %>%
        group_by(vintage_quarter) %>%
        summarise(max_mob = max(month_on_books), .groups = "drop")

      # Build MOB grid
      grid <- max_mob_v %>%
        rowwise() %>%
        do({ data.frame(vintage_quarter = .$vintage_quarter,
                        month_on_books = 0:.$max_mob, stringsAsFactors = FALSE) }) %>%
        ungroup() %>%
        left_join(cohort_size, by = "vintage_quarter")

      # Cumulative bad counts
      dq_counts <- first_dq %>%
        group_by(vintage_quarter) %>%
        arrange(first_dq_mob) %>%
        mutate(cum_bad = row_number()) %>%
        group_by(vintage_quarter, first_dq_mob) %>%
        summarise(cum_bad = max(cum_bad), .groups = "drop")

      vintage_ever <- grid %>%
        left_join(dq_counts %>% rename(month_on_books = first_dq_mob),
                  by = c("vintage_quarter", "month_on_books")) %>%
        arrange(vintage_quarter, month_on_books) %>%
        group_by(vintage_quarter) %>%
        tidyr::fill(cum_bad, .direction = "down") %>%
        mutate(cum_bad = ifelse(is.na(cum_bad), 0, cum_bad),
               bad_pct = cum_bad / n_loans) %>%
        ungroup()

      colorsd <- c("#dcf3fa","#2e3643","#FB4E0B","#C0C3C7","#B3CBD4","#82868E","#005071")
      plot_ly(data = vintage_ever,
        x = ~month_on_books, y = ~bad_pct,
        color = ~vintage_quarter, colors = colorsd,
        type = "scatter", mode = "lines+markers",
        hovertemplate = paste0(
          "<b>%{data.name}</b><br>MOB: %{x}<br>",
          "Delinquency: %{y:.1%}<extra></extra>")
      ) %>% layout(
        title  = list(text = "Cumulative Delinquency by Vintage (Performance Tape)",
                      font = list(size = 13)),
        xaxis  = list(title = "Month on Books"),
        yaxis  = list(title = "Cumulative Delinquency Rate", tickformat = ".1%"),
        paper_bgcolor = "white", plot_bgcolor = "white",
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.3,
                      font = list(size = 9))
      )

    } else {
      # ---- Synthetic fallback ----
      df_v <- vintage_raw
      colnames(df_v) <- c("loan_id","month_on_books","vintage_date","delinquency_flag")
      df_v <- df_v %>%
        mutate(
          month_on_books     = as.integer(month_on_books),
          vintage_year_month = format(as.Date(vintage_date, "%m/%d/%Y"), "%Y-%m"),
          delinquency_flag   = as.integer(delinquency_flag)
        ) %>%
        filter(!is.na(month_on_books), month_on_books >= 0, month_on_books <= 12,
               vintage_year_month %in% sel_vqs)

      if (nrow(df_v) == 0) {
        return(plot_ly() %>%
                 layout(title = list(text = "No vintage data for selected quarters"),
                        paper_bgcolor = "white", plot_bgcolor = "white"))
      }

      vintage_ever <- df_v %>%
        arrange(loan_id, month_on_books) %>%
        group_by(loan_id) %>%
        mutate(ever_2plus = cummax(delinquency_flag)) %>%
        ungroup() %>%
        group_by(vintage_year_month, month_on_books) %>%
        summarise(
          total_loans   = n_distinct(loan_id),
          bad_2plus     = sum(ever_2plus, na.rm = TRUE),
          bad_2plus_pct = bad_2plus / total_loans,
          .groups = "drop"
        )

      colorsd <- c("#dcf3fa","#2e3643","#FB4E0B","#C0C3C7","#B3CBD4","#82868E","#005071")
      plot_ly(data = vintage_ever,
        x = ~month_on_books, y = ~bad_2plus_pct,
        color = ~vintage_year_month, colors = colorsd,
        type = "scatter", mode = "lines+markers"
      ) %>% layout(
        title  = list(text = "Cumulative Performance (Synthetic)",
                      font = list(size = 13)),
        xaxis  = list(title = "Month on Books"),
        yaxis  = list(title = "2+ Delinquency Rate (%)", tickformat = ".1%"),
        paper_bgcolor = "white", plot_bgcolor = "white",
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.3,
                      font = list(size = 9))
      )
    }
  })

  # ---- Policy Overlap server ----

  # Reactive caches
  ro_sim_cache     <- reactiveVal(list())   # named list: policyId -> offered_decision_flag vector
  ro_deep_dive_data <- reactiveVal(NULL)    # list with p1/p2 KPIs + overlap counts
  # Snapshot of the policy selection committed by the last Compare Policies click.
  # Downstream outputs read from this — NOT from input$ro_policy_filter — so
  # checking/unchecking boxes never re-renders the matrix; only the button does.
  ro_committed_selection <- reactiveVal(character(0))

  # Available policies for selected product (includes newly added policies from repository)
  # Uses pr_added_snapshot() and pr_fns_snapshot() so this only re-evaluates when
  # the user is on the Policy Repository tab — not during save from other tabs.
  pr_fns_snapshot <- reactiveVal(NULL)
  observe({
    fns <- policy_fns_rv()
    if (isTRUE(input$sidebar_menu == "repos_1")) {
      pr_fns_snapshot(fns)
    } else if (is.null(isolate(pr_fns_snapshot()))) {
      pr_fns_snapshot(fns)
    }
  })
  observeEvent(input$sidebar_menu, {
    if (isTRUE(input$sidebar_menu == "repos_1")) {
      pr_fns_snapshot(policy_fns_rv())
    }
  }, ignoreInit = TRUE)

  ro_available_policies <- reactive({
    prod <- selected_product_RO()
    req(prod)
    df <- mockPolicies
    snap <- pr_added_snapshot()
    if (!is.null(snap)) df <- dplyr::bind_rows(df, snap)
    rows <- df[df$product == prod, , drop = FALSE]
    fns <- pr_fns_snapshot()
    if (is.null(fns)) fns <- policy_fns_rv()
    out <- rows[sapply(rows$policyId, function(pid) !is.null(fns[[pid]])), , drop = FALSE]
    message("[OVERLAP DEBUG] ro_available_policies() returning ", nrow(out), " policies for product '", prod,
            "': ", paste(out$policyId, collapse = ", "))
    out
  })

  # Overlap product filter — uses shared product_selector_ui("ro")
  selected_product_RO <- reactiveVal("PL")

  observeEvent(input$ro_product, {
    code <- product_name_to_code[input$ro_product]
    if (!is.na(code)) selected_product_RO(code)
  }, ignoreInit = TRUE)

  # Dynamic UI: Policy picker with visible checkboxes (compact dropdown)
  # Defaults to NONE so the user must deliberately pick 2+ and click Compare.
  output$ro_policy_filter_ui <- renderUI({
    avail <- ro_available_policies()
    ch    <- stats::setNames(avail$policyId, avail$policyName)
    tagList(
      shinyWidgets::pickerInput("ro_policy_filter",
        label    = HTML("<b>Policies to Compare</b>"),
        choices  = ch,
        selected = character(0),
        multiple = TRUE,
        width    = "100%",
        options  = list(
          `actions-box`         = TRUE,
          `live-search`         = TRUE,
          `selected-text-format` = "count > 2",
          `count-selected-text`  = "{0} of {1} policies selected",
          `none-selected-text`   = "Select two or more policies\u2026",
          `deselect-all-text`    = "None",
          `select-all-text`      = "All",
          `show-tick`            = TRUE
        )),
      tags$div(style = "font-size:12px; color:#6b7280; margin-top:4px;",
               "Tick two or more, then click Compare Policies.")
    )
  })

  # Dynamic UI: Policy 1 dropdown (baseline – always first)
  output$ro_policy1_ui <- renderUI({
    avail <- ro_available_policies()
    ch    <- stats::setNames(avail$policyId, avail$policyName)
    sel   <- if (length(ch) >= 1) ch[[1]] else NULL
    pickerInput("ro_rule1_select1", label = HTML("<b>Policy 1 (Baseline)</b>"),
      choices = ch, selected = sel,
      options = list(`actions-box` = TRUE),
      multiple = FALSE)
  })

  # Dynamic UI: Policy 2 dropdown
  output$ro_policy2_ui <- renderUI({
    avail <- ro_available_policies()
    ch    <- stats::setNames(avail$policyId, avail$policyName)
    sel   <- if (length(ch) >= 2) ch[[2]] else if (length(ch) >= 1) ch[[1]] else NULL
    pickerInput("ro_rule2_select1", label = HTML("<b>Policy 2</b>"),
      choices = ch, selected = sel,
      options = list(`actions-box` = TRUE),
      multiple = FALSE)
  })

  # Reset cache when product changes (auto-run observe will re-populate)
  observeEvent(selected_product_RO(), {
    ro_sim_cache(list())
    ro_deep_dive_data(NULL)
  }, ignoreInit = TRUE)

  # (deep-dive auto-triggers below when policy selections change)

  # Overlap simulation now ONLY runs when the user clicks Compare Policies.
  # Previously this auto-fired when arriving at the Policy Repository tab,
  # which made the page feel sluggish and obscured the user's intent. The
  # button-gated path makes the compute deliberate and gives us a clean
  # placeholder state to message before the click.

  observeEvent(input$ro_run_overlap, ignoreInit = TRUE, {
    avail    <- ro_available_policies()
    picked   <- input$ro_policy_filter
    picked   <- intersect(picked, avail$policyId)
    if (length(picked) < 2) {
      showNotification("Tick at least two policies before clicking Compare Policies.",
                       type = "warning")
      return()
    }

    existing_cache <- ro_sim_cache()
    new_pids <- setdiff(picked, names(existing_cache))

    if (length(new_pids) > 0) {
      withProgress(message = "Running policy simulations...", value = 0, {
        cache <- existing_cache
        new_avail <- avail[avail$policyId %in% new_pids, , drop = FALSE]
        for (i in seq_len(nrow(new_avail))) {
          pid <- new_avail$policyId[i]
          fn  <- policy_fns_rv()[[pid]]
          incProgress(1 / nrow(new_avail), detail = new_avail$policyName[i])
          message("[OVERLAP DEBUG] Simulating pid='", pid, "' — fn is ",
                  if (is.null(fn)) "NULL (skipping)" else "present")
          if (!is.null(fn)) {
            res <- tryCatch(fn(as.data.frame(policy_input_data)), error = function(e) {
              message("[OVERLAP DEBUG] Policy function error for '", pid, "': ", conditionMessage(e))
              NULL
            })
            if (!is.null(res) && "offered_decision_flag" %in% names(res)) {
              cache[[pid]] <- list(
                decision    = res$offered_decision_flag,
                apr         = if ("offered_final_apr" %in% names(res)) res$offered_final_apr else NULL,
                issued_flag = if ("issued_flag" %in% names(res)) res$issued_flag else NULL
              )
            }
            rm(res); gc(verbose = FALSE)
          }
        }
        ro_sim_cache(cache)
      })
    } else {
      message("[OVERLAP DEBUG] Compare clicked — cache already covers picked policies.")
    }

    # Commit the picked set so downstream outputs render from this snapshot
    # (ignoring subsequent checkbox toggles until the next Compare click).
    ro_committed_selection(picked)

    message("[OVERLAP DEBUG] Compare complete — committed ", length(picked),
            " policies; cache has ", length(ro_sim_cache()), " entries.")
  })

  # NxN agreement matrix (filtered by policy selection)
  output$corr_plot1 <- renderUI({
    # Renders from the committed snapshot only — checkbox toggles after a
    # Compare click are intentionally ignored until the next click.
    committed <- ro_committed_selection()
    cache     <- ro_sim_cache()
    message("[OVERLAP DEBUG] corr_plot1 render — committed=", length(committed),
            " cache=", length(cache))
    if (length(committed) < 2 || length(cache) < 2) {
      return(div(class = "empty-state",
        style = paste0("text-align:center; padding:40px; color:#6b7280;",
                       " background:#f9fafb; border:1px dashed #cbd5e1;",
                       " border-radius:8px; margin:8px;"),
        "Tick at least two policies above and click Compare Policies to run the overlap analysis."))
    }

    avail  <- ro_available_policies()
    labels <- stats::setNames(avail$policyName, avail$policyId)

    ids <- intersect(committed, names(cache))
    if (length(ids) < 2) {
      return(div(class = "empty-state",
        style = paste0("text-align:center; padding:40px; color:#6b7280;",
                       " background:#f9fafb; border:1px dashed #cbd5e1;",
                       " border-radius:8px; margin:8px;"),
        "The committed selection no longer matches cached simulations. Click Compare Policies again."))
    }

    pct_match <- function(x, y) mean(x == y, na.rm = TRUE)
    n <- length(ids)
    agreement_mat <- matrix(NA_real_, n, n)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        agreement_mat[i, j] <- pct_match(cache[[ids[i]]]$decision, cache[[ids[j]]]$decision)
      }
    }
    short_names <- substr(labels[ids], 1, 25)
    dimnames(agreement_mat) <- list(short_names, short_names)
    dt_df <- tibble::rownames_to_column(as.data.frame(round(agreement_mat, 3)), "Policy")

    DT::datatable(dt_df, rownames = FALSE,
      options = list(dom = "t", ordering = FALSE, scrollX = TRUE,
                     columnDefs = list(list(width = "140px", targets = 0)))
    ) %>%
      DT::formatStyle(
        columns = names(dt_df)[-1],
        backgroundColor = DT::styleInterval(c(0.5, 0.75, 0.90),
          c("#fee2e2", "#facc15", "#bbf7d0", "#16a34a")),
        color = "black", fontWeight = "bold"
      ) %>%
      DT::formatPercentage(columns = names(dt_df)[-1], digits = 1)
  })

  # Policy Comparison: explicit "Compare these two" trigger.
  # Replaces the previous auto-observe (which fired on dropdown changes but
  # silently failed if either policy wasn't yet in the simulation cache).
  observeEvent(input$ro_compare_pair, ignoreInit = TRUE, {
    pid1 <- input$ro_rule1_select1
    pid2 <- input$ro_rule2_select1
    if (is.null(pid1) || is.null(pid2) || !nzchar(pid1) || !nzchar(pid2)) {
      showNotification("Pick a policy in both dropdowns before clicking Compare.",
                       type = "warning", duration = 4); return()
    }
    if (pid1 == pid2) {
      showNotification("Pick two different policies.",
                       type = "warning", duration = 4); return()
    }

    # Warm cache for any policy not yet simulated
    cache <- ro_sim_cache()
    missing_ids <- setdiff(c(pid1, pid2), names(cache))
    if (length(missing_ids) > 0) {
      avail <- ro_available_policies()
      withProgress(message = "Running policy simulations...", value = 0, {
        for (pid in missing_ids) {
          fn <- policy_fns_rv()[[pid]]
          incProgress(1 / length(missing_ids),
                      detail = avail$policyName[avail$policyId == pid][1])
          if (!is.null(fn)) {
            res <- tryCatch(fn(as.data.frame(policy_input_data)),
                            error = function(e) NULL)
            if (!is.null(res) && "offered_decision_flag" %in% names(res)) {
              cache[[pid]] <- list(
                decision    = res$offered_decision_flag,
                apr         = if ("offered_final_apr" %in% names(res)) res$offered_final_apr else NULL,
                issued_flag = if ("issued_flag" %in% names(res)) res$issued_flag else NULL
              )
            }
          }
        }
        ro_sim_cache(cache)
      })
    }

    cache <- ro_sim_cache()
    if (!(pid1 %in% names(cache)) || !(pid2 %in% names(cache))) {
      showNotification("Could not simulate one of the picked policies.",
                       type = "error", duration = 6); return()
    }
    entry1 <- cache[[pid1]]; entry2 <- cache[[pid2]]
    dec1 <- entry1$decision; dec2 <- entry2$decision

    # Compute KPIs using credit_data + policy APR where available
    compute_kpis <- function(decision_vec, apr_vec, issued_flag_vec = NULL) {
      n_apps  <- length(decision_vec)
      appr    <- !is.na(decision_vec) & decision_vec == "APPR"
      issued  <- if (!is.null(issued_flag_vec)) issued_flag_vec == 1 else credit_data$issued_flag == 1
      n_offer <- sum(appr, na.rm = TRUE)
      n_conv  <- sum(appr & issued, na.rm = TRUE)
      appr_rate <- if (n_apps > 0) n_offer / n_apps else NA_real_
      conv_rate <- if (n_offer > 0) n_conv / n_offer else NA_real_
      apr_src <- if (!is.null(apr_vec)) apr_vec else credit_data$interest_rate_apr
      avg_apr <- if (n_conv > 0) mean(apr_src[appr & issued], na.rm = TRUE) else NA_real_
      del_rate <- if (n_conv > 0)
        mean(credit_data$performance_delinquency_flag[appr & issued], na.rm = TRUE) else NA_real_
      avg_amt <- if (n_conv > 0)
        mean(credit_data$issued_amount[appr & issued], na.rm = TRUE) else NA_real_
      list(apps = n_apps, offers = n_offer, appr_rate = appr_rate,
           conversions = n_conv, conv_rate = conv_rate,
           avg_apr = avg_apr, avg_amt = avg_amt, del_rate = del_rate)
    }

    deep_dive_result <- tryCatch({
      kpi1 <- compute_kpis(dec1, entry1$apr, entry1$issued_flag)
      kpi2 <- compute_kpis(dec2, entry2$apr, entry2$issued_flag)
      list(kpi1 = kpi1, kpi2 = kpi2)
    }, error = function(e) {
      showNotification(paste("KPI deep-dive error:", e$message),
                       type = "error", duration = 8)
      return(NULL)
    })
    req(deep_dive_result)
    kpi1 <- deep_dive_result$kpi1
    kpi2 <- deep_dive_result$kpi2

    both_appr <- sum(!is.na(dec1) & dec1 == "APPR" & !is.na(dec2) & dec2 == "APPR")
    only_p1   <- sum(!is.na(dec1) & dec1 == "APPR" & (is.na(dec2) | dec2 != "APPR"))
    only_p2   <- sum((is.na(dec1) | dec1 != "APPR") & !is.na(dec2) & dec2 == "APPR")
    both_decl <- sum((is.na(dec1) | dec1 != "APPR") & (is.na(dec2) | dec2 != "APPR"))
    agreement <- mean(dec1 == dec2, na.rm = TRUE)

    avail <- ro_available_policies()
    name1 <- avail$policyName[avail$policyId == pid1]
    name2 <- avail$policyName[avail$policyId == pid2]

    ro_deep_dive_data(list(
      kpi1 = kpi1, kpi2 = kpi2,
      name1 = name1, name2 = name2,
      pid1 = pid1, pid2 = pid2,
      both_appr = both_appr, only_p1 = only_p1,
      only_p2 = only_p2, both_decl = both_decl,
      agreement = agreement
    ))

    shinyjs::showElement("section2")
    shinyjs::hideElement("ro_compare_placeholder")
  })

  # Lever comparison table: show each lever and the change between policies
  output$rule2table1 <- renderDataTable({
    dd <- ro_deep_dive_data()
    req(dd)
    avail <- ro_available_policies()
    row1  <- avail[avail$policyId == dd$pid1, , drop = FALSE]
    row2  <- avail[avail$policyId == dd$pid2, , drop = FALSE]

    # Define the levers and their mockPolicies columns
    levers <- list(
      list(name = "Grading",         col = "grading_logic"),
      list(name = "Pricing",         col = "pricing_logic"),
      list(name = "Term",            col = "term_logic"),
      list(name = "Maximum Loan Amount", col = "max_amount_logic"),
      list(name = "Affordability",   col = "affordability_logic"),
      list(name = "Decisioning",     col = "decision_logic")
    )

    lever_names <- sapply(levers, `[[`, "name")
    p1_vals     <- sapply(levers, function(lv) as.character(row1[[lv$col]]))
    p2_vals     <- sapply(levers, function(lv) as.character(row2[[lv$col]]))
    change_vals <- ifelse(p1_vals == p2_vals, "No change", p2_vals)

    comp_df <- data.frame(
      Section     = lever_names,
      Policy_1    = p1_vals,
      Change      = change_vals,
      stringsAsFactors = FALSE
    )
    names(comp_df) <- c("Section", dd$name1, paste0("Change in ", dd$name2))

    DT::datatable(comp_df, rownames = FALSE,
      options = list(dom = "t", ordering = FALSE,
        columnDefs = list(list(width = "110px", targets = 0)))
    ) %>%
      DT::formatStyle(
        columns = ncol(comp_df),
        backgroundColor = DT::styleEqual("No change", "#d1fae5"),
        color = DT::styleEqual("No change", "#065f46"),
        fontWeight = "bold"
      )
  })

  # KPI tiles helper – 4 key metrics: Approvals, Issued, Avg APR, Approval Rate
  .ro_kpi_tiles <- function(kpis, policy_name) {
    tagList(
      tags$h5(policy_name, class = "text-center text-bold",
        style = "margin-bottom:8px; font-size:13px;"),
      fluidRow(
        column(3, div(class = "custom-value-box",
          h5("Approvals"), h3(prettyNum(kpis$offers, big.mark = ",")))),
        column(3, div(class = "custom-value-box",
          h5("Issued"), h3(prettyNum(kpis$conversions, big.mark = ",")))),
        column(3, div(class = "custom-value-box",
          h5("Avg APR"), h3(if (!is.na(kpis$avg_apr)) roundoff(kpis$avg_apr) else "N/A"))),
        column(3, div(class = "custom-value-box",
          h5("Appr Rate"), h3(if (!is.na(kpis$appr_rate)) roundoff(kpis$appr_rate) else "N/A")))
      )
    )
  }

  output$rule1_metrics <- renderUI({
    dd <- ro_deep_dive_data()
    req(dd)
    .ro_kpi_tiles(dd$kpi1, dd$name1)
  })

  output$rule2_metrics <- renderUI({
    dd <- ro_deep_dive_data()
    req(dd)
    .ro_kpi_tiles(dd$kpi2, dd$name2)
  })

  # Funnel metrics table for Policy Overlap (mirrors Opportunity Finder style)
  output$ro_funnel_table <- DT::renderDT({
    dd <- ro_deep_dive_data()
    req(dd)
    k1 <- dd$kpi1
    k2 <- dd$kpi2

    fp  <- function(x) if (is.na(x)) "\u2014" else sprintf("%.1f%%", x * 100)
    # Counts: comma-separated integer, scientific=FALSE so 100000 -> "100,000"
    # rather than R's default "1e+05".
    fc  <- function(x) {
      if (is.na(x)) return("\u2014")
      formatC(round(as.numeric(x)), format = "d", big.mark = ",")
    }
    # Dollar amounts: $X.XK above 1k, plain $X below.
    fa  <- function(x) {
      if (is.na(x)) return("\u2014")
      x <- as.numeric(x)
      if (abs(x) >= 1000) sprintf("$%.1fK", x / 1000)
      else                sprintf("$%d",    round(x))
    }
    fdpct <- function(b, p) {
      if (is.na(b) || is.na(p) || b == 0) return("\u2014")
      sprintf("%+.1f%%", (p - b) / b * 100)
    }
    fdp <- function(b, p) {
      if (is.na(b) || is.na(p)) return("\u2014")
      sprintf("%+.1f pp", (p - b) * 100)
    }

    rows <- list(
      c("Applications",    fc(k1$apps),      fc(k2$apps),      fdpct(k1$apps,      k2$apps)),
      c("Approved",        fc(k1$offers),    fc(k2$offers),    fdpct(k1$offers,    k2$offers)),
      c("Issued",          fc(k1$conversions), fc(k2$conversions), fdpct(k1$conversions, k2$conversions)),
      c("Approval Rate",   fp(k1$appr_rate), fp(k2$appr_rate), fdp(k1$appr_rate, k2$appr_rate)),
      c("Take Rate",       fp(k1$conv_rate), fp(k2$conv_rate), fdp(k1$conv_rate, k2$conv_rate)),
      c("Bad Rate",        fp(k1$del_rate),  fp(k2$del_rate),  fdp(k1$del_rate,  k2$del_rate)),
      c("Avg APR",         fp(k1$avg_apr),   fp(k2$avg_apr),   fdp(k1$avg_apr,   k2$avg_apr)),
      c("Avg Amount",      fa(k1$avg_amt),   fa(k2$avg_amt),   fdpct(k1$avg_amt, k2$avg_amt))
    )
    tbl <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
    names(tbl) <- c("Metric", dd$name1, dd$name2, "Delta")

    DT::datatable(tbl, rownames = FALSE, selection = "none",
      options = list(
        dom = "t", pageLength = 10,
        initComplete = JS(
          "function(s,j){",
          "$(this.api().table().header()).css(",
          "  {'background-color':'#fb6e36','color':'white'});",
          "}"
        )
      )
    ) %>%
      DT::formatStyle("Delta",
        color = JS(paste0(
          "function(v){",
          "  if(v && v.charAt(0)==='+') return '#16a34a';",
          "  if(v && v.charAt(0)==='-') return '#dc2626';",
          "  return '#374151';",
          "}"
        ))
      )
  })

  # ---- AI Comparison Narrative (UC6) ----
  output$ro_comparison_narrative <- renderUI({
   tryCatch({
    dd <- ro_deep_dive_data()
    req(dd)

    pid1 <- dd$pid1
    pid2 <- dd$pid2
    name1 <- dd$name1
    name2 <- dd$name2

    # Build param_diff from strategy_overrides
    sa <- strategy_overrides[[pid1]]
    sb <- strategy_overrides[[pid2]]
    req(sa, sb)

    params_a <- sa$params %||% list()
    params_b <- sb$params %||% list()
    all_keys <- unique(c(names(params_a), names(params_b)))

    param_diff <- if (length(all_keys) == 0) {
      data.frame(param = character(0), value_a = character(0),
                 value_b = character(0), changed = logical(0),
                 stringsAsFactors = FALSE)
    } else {
      do.call(rbind, lapply(all_keys, function(k) {
        va <- params_a[[k]]
        vb <- params_b[[k]]
        data.frame(
          param = k,
          value_a = if (is.null(va)) "(default)" else as.character(va),
          value_b = if (is.null(vb)) "(default)" else as.character(vb),
          changed = !identical(va, vb),
          stringsAsFactors = FALSE
        )
      }))
    }

    # Build kpi_delta from the deep-dive KPIs
    k1 <- dd$kpi1
    k2 <- dd$kpi2
    metrics <- list(
      list(metric = "approval_rate",  a = k1$appr_rate, b = k2$appr_rate),
      list(metric = "avg_loss_rate",  a = k1$del_rate,  b = k2$del_rate),
      list(metric = "portfolio_irr",  a = k1$avg_apr,   b = k2$avg_apr)
    )
    kpi_delta <- do.call(rbind, lapply(metrics, function(x) {
      va <- x$a; vb <- x$b
      delta <- if (is.null(va) || is.null(vb) || is.na(va) || is.na(vb)) NA_real_ else vb - va
      data.frame(metric = x$metric,
                 a = if (is.null(va)) NA_real_ else va,
                 b = if (is.null(vb)) NA_real_ else vb,
                 delta = delta, stringsAsFactors = FALSE)
    }))

    narrative_text <- narrate_comparison(name1, name2, param_diff, kpi_delta)

    tags$div(
      style = "background:#1a1f2e; border-left:3px solid #fb6e36; border-radius:6px; padding:12px 16px; margin-top:12px;",
      tags$span(style = "color:#fb6e36; font-weight:600;", icon("sparkles"), " Policy Comparison Summary"),
      tags$p(style = "color:#d1d5db; font-size:13px; margin-top:8px; white-space:pre-wrap;", narrative_text)
    )
   }, error = function(e) {
     message("[PolicyRepo] ro_comparison_narrative: ", conditionMessage(e))
     NULL
   })
  })
}
