# ============================================================
# Tab: SMART Risk Scanner
# Discover high-risk micro-segments across every portfolio
# dimension with automated CART analysis and marginal boundary
# optimization.
# ============================================================

smart_recommender_ui <- function() {
  tabItem(tabName = "smart_rec",

    # CSS overrides for Smart Recommender
    tags$style(HTML("
      @keyframes pulse { 0%,100% { opacity:0.6; transform:scale(0.95); } 50% { opacity:1; transform:scale(1.05); } }
      /* Adjust Cutoff slider text: white on dark background */
      #sr_cutoff_slider .irs--shiny .irs-min,
      #sr_cutoff_slider .irs--shiny .irs-max,
      #sr_cutoff_slider .irs--shiny .irs-from,
      #sr_cutoff_slider .irs--shiny .irs-to,
      #sr_cutoff_slider .irs--shiny .irs-single,
      #sr_cutoff_slider .irs-grid-text {
        color: #ffffff !important;
      }
      #sr_cutoff_slider + .irs .irs-min,
      #sr_cutoff_slider + .irs .irs-max,
      #sr_cutoff_slider + .irs .irs-from,
      #sr_cutoff_slider + .irs .irs-to,
      #sr_cutoff_slider + .irs .irs-single,
      #sr_cutoff_slider + .irs .irs-grid-text {
        color: #ffffff !important;
      }
    ")),

    header_panel(
      "Risk Agent",
      "Discover high-risk micro-segments across every portfolio dimension with automated analysis and marginal boundary optimization.",
      icon_name = "bullseye",
      data_scope = "issued_perf"
    ),

    tags$div(style = "background:#0f2a47; color:#9cc5ff; padding:8px 14px; border-radius:6px; margin-bottom:12px; font-size:12px;",
             tags$i(class = "fa fa-database", style = "margin-right:6px;"),
             "Scanner always runs on ", tags$b("Issued loans + Performance"),
             " \u2014 the only population with realised delinquency / IRR outcomes to learn from."),

    # Product selector + active policy banner + AI image + scan button
    fluidRow(
      column(12,
        product_selector_ui("sr"),
        active_policy_card_ui("sr"),
        # Scanner visual — CSS+icon badge replaces the bitmap AI-chip image so
        # it stays crisp at any zoom and matches the brand orange.
        div(style = "text-align:center; margin-top:24px; margin-bottom:6px;",
          div(style = paste0(
                "display:inline-flex; align-items:center; justify-content:center;",
                " width:120px; height:120px; border-radius:50%;",
                " background:radial-gradient(circle at 30% 30%, #ff8a5a, #fb6e36 65%, #d65117 100%);",
                " box-shadow:0 8px 24px rgba(251,110,54,0.35), inset 0 -4px 10px rgba(0,0,0,0.18);"),
              icon("bullseye", style = "color:white; font-size:50px;")
          )
        ),
        div(style = "text-align:center; margin-top:16px; margin-bottom:10px;",
          actionButton("sr_simulate_now", "Run Risk Scan",
            icon = icon("bolt"), class = "btn-lg",
            style = "background-color:#fb6e36; color:white; border:none; font-weight:bold; font-size:16px; padding:14px 48px; border-radius:6px;")
        )
      )
    ),

    # Analysis pipeline (visible during scan)
    pipeline_container("sr_scan_pipeline"),

    # Loading spinner fallback (visible while scanning if pipeline not shown)
    conditionalPanel(
      condition = "output.sr_scanning",
      div(style = "text-align:center; padding:60px 20px;",
          icon("spinner", class = "fa-spin fa-3x", style = "color:#fb6e36;"),
          p(style = "color:#9ca3af; margin-top:15px; font-size:15px;",
            "Scanning portfolio slices..."))),

    # Portfolio KPIs (hidden until scan ready)
    conditionalPanel(
      condition = "output.sr_scan_ready",
      uiOutput("sr_portfolio_kpis")),

    # Source type filter
    conditionalPanel(
      condition = "output.sr_scan_ready",
      div(style = "margin-top:10px; margin-bottom:5px;",
        shinyWidgets::radioGroupButtons(
          inputId  = "sr_source_filter",
          label    = NULL,
          choices  = c("All", "Delinquency", "IRR"),
          selected = "Delinquency",
          size     = "sm",
          status   = "default"
        )
      )
    ),

    # Recommendation cards
    uiOutput("sr_recommendation_cards"),

    # AI Insights panel (UC3)
    uiOutput("sr_ai_insights"),

    # AI action loading overlay (hidden by default)
    shinyjs::hidden(div(id = "sr_action_overlay",
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
        icon("bullseye", style = "color:white; font-size:36px;")
      ),
      tags$p("Analyzing segment...", style = "color:#fb6e36; font-size:16px; margin-top:16px; font-weight:600;")
    )),

    # Marginal drilldown
    uiOutput("sr_marginal_drilldown")
  )
}


# simplify_rule() — moved to R/engine/segments.R

# format_rule_display() is now defined in 02_global_utils.R

# extract_segments() — moved to R/engine/segments.R

# compute_marginal_bands() — moved to R/engine/segments.R

# build_boundary_population() — moved to R/engine/segments.R

smart_recommender_server <- function(input, output, session,
                                     credit_data, policy_input_data,
                                     loan_irr_cache, perf_tape,
                                     added_policy, policy_fns_rv) {

  # ---- Active policy banner under product selector ----
  wire_active_policy_card(input, output, session, "sr")

  # ---- Reactive state ----
  sr_rv <- reactiveValues(
    scan_results      = NULL,
    selected_seg      = NULL,
    portfolio_metrics = NULL,
    filtered_data     = NULL,
    # A/B simulation slots
    sim_A = NULL, sim_B = NULL,
    adverse_flip_ids = character(0),
    bps = 0
  )

  # Pagination state for recommendation cards (5 per page)
  sr_page <- reactiveVal(1L)
  sr_page_size <- 5L

  # ---- Smart Rec product selector — uses shared product_selector_ui("sr") ----
  sr_selected_product <- reactiveVal("Personal Loan")

  observeEvent(input$sr_product, {
    sr_selected_product(input$sr_product)
  }, ignoreInit = TRUE)

  output$sr_active_policy_label <- renderText({
    get_active_policy_id(sr_selected_product())
  })

  # ---- Filtered data reactive (product + policy regime) ----
  # Returns a logical row-mask so we can apply the same filter consistently
  # to other dataframes (e.g. policy_input_data) that share row order with credit_data.
  sr_row_mask <- reactive({
    mask <- credit_data$product_type == sr_selected_product()
    # Filter to active policy for the selected product
    if ("policy_id" %in% names(credit_data)) {
      active_pid <- get_active_policy_id(sr_selected_product())
      mask <- mask & (credit_data$policy_id == active_pid)
    }
    mask
  })

  sr_filtered <- reactive({
    credit_data[sr_row_mask(), , drop = FALSE]
  })

  # ---- Scanner-local cached tape reactive ----
  # Centralises perf_tape() calls so all Scanner outputs share one reference
  # per render cycle, avoiding repeated ~100K-row reads on each render.
  sr_tape <- reactive({
    tryCatch(perf_tape(), error = function(e) {
      message("[Scanner] sr_tape: perf_tape() failed: ", e$message)
      NULL
    })
  })

  # ---- Loan-id -> row-index hash for tape$rows (built lazily once per tape) ----
  # Replaces expensive %in% scans over the full tape$rows$loan_id vector for
  # compute_real_irr. The hash is built once per tape invalidation.
  sr_tape_index <- reactive({
    tape <- sr_tape()
    if (is.null(tape) || is.null(tape$rows) || !nrow(tape$rows)) return(NULL)
    tryCatch({
      lid <- as.character(tape$rows$loan_id)
      split(seq_along(lid), lid)
    }, error = function(e) {
      message("[Scanner] sr_tape_index build failed: ", e$message)
      NULL
    })
  })

  # ── Reactive flags ──
  output$sr_scan_ready <- reactive({ !is.null(sr_rv$scan_results) })
  outputOptions(output, "sr_scan_ready", suspendWhenHidden = FALSE)

  sr_scanning <- reactiveVal(FALSE)
  output$sr_scanning <- reactive({ sr_scanning() })
  outputOptions(output, "sr_scanning", suspendWhenHidden = FALSE)


  # ── Scan engine — triggers on Simulate Now button ──
  observeEvent(input$sr_simulate_now, {
    tryCatch({
    sr_rv$scan_results <- NULL
    sr_scanning(TRUE)

    df <- sr_filtered()
    req(nrow(df) > 100)
    df_issued <- df[df$issued_flag == 1, ]
    req(nrow(df_issued) > 50)

    # Analysis variables
    all_vars <- unname(analysis_vars)
    avail_vars <- intersect(all_vars, names(df_issued))
    avail_vars <- avail_vars[sapply(avail_vars, function(v) is.numeric(df_issued[[v]]))]
    req(length(avail_vars) >= 3)

    # ---- Show analysis pipeline ----
    pipeline_show(session, "sr_scan_pipeline", list(
      list(label = "Preparing portfolio data and variable selection"),
      list(label = "Running proprietary risk models across portfolio cross-sections"),
      list(label = "Computing projected lifetime bad rates (donor curves where needed)"),
      list(label = "Computing segment-level IRR from cashflow models"),
      list(label = "Ranking segments by risk-adjusted action score")
    ))

    pipeline_step_done(session, "sr_scan_pipeline", 0L,
      paste0(format(nrow(df_issued), big.mark = ","), " issued loans, ",
             length(avail_vars), " variables"))

    # Run scan engine
    scan_result <- scan_portfolio(
      df_issued       = df_issued,
      avail_vars      = avail_vars,
      perf_tape       = tryCatch(sr_tape(), error = function(e) NULL),
      irr_cache       = tryCatch(loan_irr_cache(), error = function(e) NULL),
      cross_slices    = TRUE,
      objective       = "blended",
      anchor_fico_max = tryCatch(consultant_config$anchor_fico_floor,
        error = function(e) 760),
      progress_fn     = function(i, n, label) {
        # Update pipeline step 1 with scan progress
        if (i == 1) pipeline_step_done(session, "sr_scan_pipeline", 1L,
          paste0("Scanning ", n, " cross-sections"))
      }
    )

    top_segments   <- scan_result$segments
    portfolio_bad  <- scan_result$portfolio_bad
    portfolio_proj <- scan_result$portfolio_proj
    portfolio_irr  <- scan_result$portfolio_irr
    n_slices       <- scan_result$n_slices_scanned

    pipeline_step_done(session, "sr_scan_pipeline", 2L,
      if (!is.na(portfolio_proj)) paste0(round(portfolio_proj * 100, 1), "% portfolio projected")
      else "computed")

    pipeline_step_done(session, "sr_scan_pipeline", 3L,
      if (!is.na(portfolio_irr)) paste0(round(portfolio_irr * 100, 1), "% portfolio IRR")
      else "computed")

    pipeline_step_done(session, "sr_scan_pipeline", 4L,
      paste0(length(top_segments), " segments ranked"))

    pipeline_complete(session, "sr_scan_pipeline")

    sr_rv$scan_results <- top_segments
    sr_rv$selected_seg <- NULL
    sr_page(1L)
    sr_rv$portfolio_metrics <- list(
      bad_rate = portfolio_bad,
      projected_bad_rate = portfolio_proj,
      irr      = portfolio_irr,
      n_issued = nrow(df_issued),
      n_slices = n_slices
    )
    sr_scanning(FALSE)
    }, error = function(e) {
      message("[SR ERROR] Scan failed: ", e$message)
      sr_scanning(FALSE)
    })
  })


  # ── Portfolio summary KPIs ──
  output$sr_portfolio_kpis <- renderUI({
    req(sr_rv$portfolio_metrics)
    m <- sr_rv$portfolio_metrics
    has_irr <- !is.na(m$irr)
    w <- if (has_irr) 3 else 4  # wider columns when no IRR card

    kpi_list <- list(
      column(w, kpi_box(
        fmt_k(m$n_issued), "Issued Loans", "in filtered scope")),
      column(w, kpi_box(
        scales::percent(m$bad_rate, accuracy = 0.1), "Portfolio 2+ Rate",
        if (!is.null(m$projected_bad_rate) && abs(m$projected_bad_rate - m$bad_rate) > 0.001)
          paste0("projected lifetime: ", scales::percent(m$projected_bad_rate, accuracy = 0.1))
        else "baseline")),
      if (has_irr) column(w, kpi_box(
        scales::percent(m$irr, accuracy = 0.1),
        "Portfolio Avg IRR", "baseline")),
      column(w, kpi_box(
        length(sr_rv$scan_results), "Segments Found",
        paste("from", m$n_slices %||% 1, "portfolio slices")))
    )
    do.call(fluidRow, Filter(Negate(is.null), kpi_list))
  })

  # ── Reset page on source filter change ──
  observeEvent(input$sr_source_filter, {
    sr_page(1L)
  }, ignoreInit = TRUE)

  # ── Filtered segments reactive (applies source type filter) ──
  sr_filtered_segs <- reactive({
    segs <- sr_rv$scan_results
    if (is.null(segs)) return(NULL)
    src <- input$sr_source_filter %||% "All"
    if (src == "All") return(segs)
    Filter(function(s) tolower(s$source) == tolower(src), segs)
  })

  # ── Pagination observers ──
  observeEvent(input$sr_page_prev, {
    if (sr_page() > 1L) sr_page(sr_page() - 1L)
  })
  observeEvent(input$sr_page_next, {
    segs <- sr_filtered_segs()
    max_page <- ceiling(length(segs) / sr_page_size)
    if (sr_page() < max_page) sr_page(sr_page() + 1L)
  })

  # ── Recommendation cards ──
  output$sr_recommendation_cards <- renderUI({
    req(sr_filtered_segs())
    segs <- sr_filtered_segs()
    if (length(segs) == 0) {
      return(div(class = "card", style = "padding:20px; text-align:center;",
                 icon("check-circle", style = "color:#10b981; font-size:24px;"),
                 p("No actionable segments found. Portfolio looks healthy.")))
    }

    m <- sr_rv$portfolio_metrics
    n_total <- length(segs)
    page    <- sr_page()
    max_page <- ceiling(n_total / sr_page_size)
    idx_start <- (page - 1L) * sr_page_size + 1L
    idx_end   <- min(page * sr_page_size, n_total)
    page_indices <- idx_start:idx_end

    cards <- lapply(page_indices, function(i) {
      s <- segs[[i]]
      severity <- if (s$lift >= 2.5) "border-left: 4px solid #ef4444;"
                  else "border-left: 4px solid #f59e0b;"
      source_label <- if (s$source == "delinquency") "Delinquency" else "IRR"
      filter_label <- s$source_filter %||% "All Data"
      pct_of_book <- scales::percent(s$n / m$n_issued, accuracy = 0.1)

      tree_badge <- span(style = "display:inline-block; background:#374151; color:#d1d5db;
                           font-size:11px; padding:2px 8px; border-radius:10px;
                           margin-right:6px; font-weight:600;", source_label)
      filter_badge <- if (filter_label != "All Data") {
        span(style = "display:inline-block; background:#1e40af; color:#93c5fd;
                      font-size:11px; padding:2px 8px; border-radius:10px;
                      font-weight:600;", filter_label)
      } else NULL

      # Whole card is clickable; JS toggles the .sr-card-selected class and
      # pushes the index to input$sr_selected_card so the top action bar can
      # dispatch.
      onclick_js <- sprintf(
        "document.querySelectorAll('.sr-rec-card').forEach(function(c){c.classList.remove('sr-card-selected');});this.classList.add('sr-card-selected');Shiny.setInputValue('sr_selected_card', %d, {priority:'event'});",
        i
      )

      div(class = "card sr-rec-card",
          style = paste0("padding:15px; margin-bottom:10px; cursor:pointer; ", severity),
          `data-card-id` = i,
          onclick = onclick_js,
        fluidRow(
          column(1, div(style = "font-size:28px; font-weight:bold; color:#fb6e36;
                         text-align:center; padding-top:10px;",
                        paste0("#", i))),
          column(11,
            div(style = "font-size:15px; font-weight:bold; color:#111827; line-height:1.4;",
                paste0("2+ Rate: ", scales::percent(s$bad_rate, accuracy = 0.1),
                       if (!is.null(s$projected_bad_rate) &&
                           abs(s$projected_bad_rate - s$bad_rate) > 0.01)
                         paste0(" observed | ",
                                scales::percent(s$projected_bad_rate, accuracy = 0.1),
                                " projected")
                       else "",
                       " (", s$lift, "x portfolio) | Loans: ",
                       scales::comma(s$n), " (", pct_of_book, " of book)")),
            div(style = "font-size:13px; color:#6b7280; margin-top:4px; font-weight:600;",
                paste0("APR: ", scales::percent(s$avg_apr, accuracy = 0.1),
                       "  |  IRR: ",
                       if (!is.na(s$mean_irr)) scales::percent(s$mean_irr, accuracy = 0.1) else "N/A")),
            div(style = "margin-top:6px;", tree_badge, filter_badge),
            div(style = "font-size:13px; color:#9ca3af; margin-top:4px; font-weight:normal;",
                s$display_rule)
          )
        )
      )
    })

    # Pagination controls
    nav_bar <- if (max_page > 1) {
      div(style = "display:flex; justify-content:center; align-items:center; gap:12px;
                    margin-top:10px; margin-bottom:5px;",
        actionButton("sr_page_prev", icon("chevron-left"),
                     class = "btn-sm",
                     style = paste0("background:#374151; color:white; border:none;",
                                    if (page == 1) " opacity:0.4; pointer-events:none;" else "")),
        span(style = "color:#9ca3af; font-size:13px; font-weight:600;",
             paste0("Page ", page, " of ", max_page,
                    "  (", idx_start, "-", idx_end, " of ", n_total, ")")),
        actionButton("sr_page_next", icon("chevron-right"),
                     class = "btn-sm",
                     style = paste0("background:#374151; color:white; border:none;",
                                    if (page == max_page) " opacity:0.4; pointer-events:none;" else ""))
      )
    } else NULL

    # Sticky action bar — disabled until a card is selected. The CSS in
    # smart_recommender_ui() handles the .sr-card-selected highlight and
    # the disabled state of the bar buttons (input$sr_selected_card == NULL).
    action_bar <- div(
      id = "sr_action_bar",
      style = paste0(
        "position:sticky; top:0; z-index:20; background:#f9fafb; ",
        "padding:10px 16px; border:1px solid #e5e7eb; border-radius:8px; ",
        "margin-bottom:12px; display:flex; align-items:center; gap:8px; ",
        "box-shadow:0 1px 3px rgba(0,0,0,0.04);"),
      tags$span(id = "sr_selection_label",
        style = "font-size:13px; color:#6b7280; flex:1;",
        "Select a recommendation card to enable actions"),
      actionButton("sr_act_deeper", "Deep dive",
        icon = icon("scissors"), class = "btn-sm sr-act-btn",
        title = "Open this segment's variables in Opportunity Agent",
        style = "background:#8b5cf6; color:white; border:none;"),
      actionButton("sr_act_explore", "Explore",
        icon = icon("magnifying-glass-chart"), class = "btn-sm sr-act-btn",
        style = "background:#3b82f6; color:white; border:none;"),
      actionButton("sr_act_decline", "Decline",
        icon = icon("ban"), class = "btn-sm btn-danger sr-act-btn"),
      actionButton("sr_act_reprice", "Reprice",
        icon = icon("dollar-sign"), class = "btn-sm btn-warning sr-act-btn"),
      actionButton("sr_act_talking", "Talking points",
        icon = icon("clipboard"), class = "btn-sm sr-act-btn",
        style = "background:#374151; color:white; border:none;")
    )

    tagList(
      tags$style(HTML(
        ".sr-rec-card { transition: box-shadow 0.15s ease, background 0.15s ease; }
         .sr-rec-card:hover { box-shadow: 0 2px 8px rgba(0,0,0,0.08); }
         .sr-rec-card.sr-card-selected { background:#fff7ed; box-shadow:0 0 0 2px #fb6e36; }
         #sr_action_bar.sr-bar-disabled .sr-act-btn { opacity:0.45; pointer-events:none; }
        ")),
      div(style = "margin-top:15px; margin-bottom:8px;",
          h3(style = "color:#fb6e36; font-weight:bold; font-size:20px;",
             icon("lightbulb"), " Recommendations")),
      action_bar,
      do.call(tagList, cards),
      nav_bar
    )
  })

  # Toggle the disabled class on the action bar based on selection.
  observe({
    has_selection <- !is.null(input$sr_selected_card)
    shinyjs::runjs(sprintf(
      "(function(){var b=document.getElementById('sr_action_bar');if(!b)return;b.classList.%s('sr-bar-disabled');var lbl=document.getElementById('sr_selection_label');if(lbl)lbl.textContent=%s;})();",
      if (has_selection) "remove" else "add",
      if (has_selection) sprintf("'Segment #%d selected'", as.integer(input$sr_selected_card))
      else "'Select a recommendation card to enable actions'"
    ))
  })

  # Reset selection whenever the card list changes (new scan / page change).
  observeEvent(sr_filtered_segs(), {
    shinyjs::runjs("Shiny.setInputValue('sr_selected_card', null, {priority:'event'});document.querySelectorAll('.sr-rec-card').forEach(function(c){c.classList.remove('sr-card-selected');});")
  }, ignoreInit = TRUE)

  # ── Action helpers — invoked by the sticky action-bar buttons. Take a
  # `seg` (the selected segment) and dispatch to the same flow the per-card
  # buttons used to run.
  .sr_get_selected_seg <- function() {
    idx <- input$sr_selected_card
    if (is.null(idx)) return(NULL)
    segs <- sr_filtered_segs()
    idx <- as.integer(idx)
    if (is.na(idx) || idx < 1 || idx > length(segs)) return(NULL)
    list(idx = idx, seg = segs[[idx]])
  }

  .sr_action_explore <- function(seg) {
    sr_rv$selected_seg <- seg
    shinyjs::runjs("setTimeout(function(){
      var el = document.getElementById('sr_marginal_drilldown');
      if(el) el.scrollIntoView({behavior:'smooth', block:'start'});
    }, 600);")
  }

  .sr_action_deeper <- function(seg, idx) {
    rule_text <- seg$rule %||% seg$display_rule %||% ""
    vars <- extract_rule_vars(rule_text)
    if (length(vars) == 0) {
      carry <- unique(c(seg$primary_var, seg$tree_vars))
      vars  <- carry[!is.na(carry) & nzchar(carry)]
    }
    av <- if (exists("analysis_vars", envir = globalenv()))
            get("analysis_vars", envir = globalenv()) else character(0)
    vars <- intersect(vars, unname(av))
    if (length(vars) == 0) {
      showNotification("No rule variables found to hand off.",
                       type = "warning", duration = 3)
      return(invisible(NULL))
    }
    shinyWidgets::updatePickerInput(session, "opp_vars", selected = vars)
    # seg$loan_ids is the authoritative leaf membership (rule text can match
    # more loans than the leaf when the CART path is simplified for display).
    session$userData$deeper_cuts_rule <- rule_text
    session$userData$deeper_cuts_loan_ids <-
      if (!is.null(seg$loan_ids)) as.character(seg$loan_ids) else NULL
    showNotification(
      paste0("Carried segment #", idx,
             " (", length(vars), " variables) into Opportunity Agent."),
      type = "message", duration = 3)
    updateTabItems(session, "sidebar_menu", "opp_find")
  }

  observeEvent(input$sr_act_explore, {
    sel <- .sr_get_selected_seg(); req(sel)
    .sr_action_explore(sel$seg)
  }, ignoreInit = TRUE)

  observeEvent(input$sr_act_deeper, {
    sel <- .sr_get_selected_seg(); req(sel)
    .sr_action_deeper(sel$seg, sel$idx)
  }, ignoreInit = TRUE)

  # ── AI Insights panel (UC3) ──
  output$sr_ai_insights <- renderUI({
    req(sr_rv$scan_results, sr_rv$portfolio_metrics)
    m <- sr_rv$portfolio_metrics
    segs <- sr_rv$scan_results
    top_rule <- if (length(segs) > 0) (segs[[1]]$display_rule %||% segs[[1]]$rule %||% "n/a") else "n/a"
    kpis <- list(
      bad_rate      = m$bad_rate,
      irr           = m$irr,
      n_issued      = m$n_issued,
      segment_count = length(segs),
      top_segment_rule = top_rule
    )
    insights_text <- tryCatch(
      generate_insights("scan", kpis),
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

  # ── Talking Points action handler (UC7) ──
  observeEvent(input$sr_act_talking, {
    sel <- .sr_get_selected_seg(); req(sel)
    s <- sel$seg
    m <- sr_rv$portfolio_metrics
    pct_book <- if (!is.null(m) && !is.null(m$n_issued) && m$n_issued > 0) s$n / m$n_issued else NA_real_
    rec_action <- if (!is.null(s$lift) && s$lift >= 3) "decline"
                  else if (!is.null(s$lift) && s$lift >= 2) "reprice"
                  else "monitor"
    seg_data <- list(
      n           = s$n,
      bad_rate    = s$bad_rate,
      lift        = s$lift,
      mean_irr    = s$mean_irr,
      avg_apr     = s$avg_apr,
      pct_of_book = pct_book,
      rule        = s$display_rule %||% s$rule %||% "n/a",
      recommended_action = rec_action,
      source      = s$source %||% "delinquency",
      portfolio_bad_rate = m$bad_rate
    )
    tp_text <- tryCatch(
      generate_talking_points("segment", seg_data),
      error = function(e) paste0("1. Error generating talking points: ", conditionMessage(e))
    )
    showModal(modalDialog(
      title = "Talking Points",
      tags$pre(id = "sr_tp_text",
        style = "background:#111827; color:#d1d5db; padding:16px; border-radius:6px; white-space:pre-wrap; font-size:13px;",
        tp_text),
      footer = tagList(
        actionButton("sr_tp_copy", "Copy to Clipboard", icon = icon("clipboard")),
        modalButton("Close")
      )
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$sr_tp_copy, {
    shinyjs::runjs("navigator.clipboard.writeText(document.getElementById('sr_tp_text').innerText);")
    showNotification("Copied!", type = "message", duration = 2)
  })

  # ── Marginal drilldown panel ──
  output$sr_marginal_drilldown <- renderUI({
    seg <- sr_rv$selected_seg
    req(seg)
    req(is.list(seg))

    if (is.na(seg$cutoff_val) || is.null(seg$primary_var)) {
      return(div(class = "card", style = "padding:20px;",
                 p("Cannot compute boundary analysis for multi-condition segments.")))
    }

    div(style = "margin-top:15px;",
      div(class = "card", style = "padding:15px;",
        fluidRow(
          column(8,
            h4(style = "color:white; margin-top:0;",
               icon("magnifying-glass-chart"),
               paste0(" Boundary Analysis: ", seg$primary_var,
                       if (seg$direction == "below") " < " else " >= ",
                       if (grepl("income|amount|payment", seg$primary_var, ignore.case = TRUE))
                         paste0("$", formatC(round(seg$cutoff_val), format = "f", digits = 0, big.mark = ","))
                       else round(seg$cutoff_val, 2)))
          ),
          column(4, style = "text-align:right;",
            actionButton("sr_close_drilldown", "Close",
                         icon = icon("times"), class = "btn-sm btn-secondary")
          )
        ),

        fluidRow(
          column(6,
            div(style = "padding-top:10px;",
              DT::dataTableOutput("sr_band_table"))
          ),
          column(6,
            uiOutput("sr_chart_insight"),
            uiOutput("sr_table_insight")
          )
        ),

        fluidRow(column(6, div(style = "margin-top:15px;",
          # Action buttons — span the same width as the band table column on the
          # left (column 6) so they end where the Avg <primary_var> column ends.
          div(style = "display:flex; align-items:stretch; gap:10px; width:100%;",
            actionButton("sr_apply_decline", "Decline Segment",
                         icon = icon("ban"), class = "btn-danger",
                         style = "flex:1;"),
            actionButton("sr_apply_reprice", "Reprice Segment",
                         icon = icon("dollar-sign"), class = "btn-warning",
                         style = "flex:1;"),
            actionButton("sr_goto_opp", "Deep dive",
                         icon = icon("scissors"),
                         style = "flex:1; background:#0f766e; color:white; border:none;")
          ),
          # A/B slot selector + Clear links sit directly under the action buttons.
          div(style = "margin-top:12px; display:flex; align-items:center; gap:16px; flex-wrap:wrap;",
            radioGroupButtons("sr_sim_slot", "Save As",
              choices = c("Primary Action" = "A", "Alternative Action" = "B"),
              selected = "A", size = "sm", status = "warning"),
            div(style = "display:flex; gap:16px; align-items:center; padding-top:18px;",
              actionLink("sr_clear_A", label = tagList(icon("trash"), " Clear Primary"),
                style = "color:#dc2626; font-size:12px;"),
              actionLink("sr_clear_B", label = tagList(icon("trash"), " Clear Alternative"),
                style = "color:#dc2626; font-size:12px;")
            )
          )
        ))),

        # ── Results panel (rendered dynamically) ──
        uiOutput("sr_action_results")
      )
    )
  })

  # Close drilldown
  observeEvent(input$sr_close_drilldown, {
    sr_rv$selected_seg <- NULL
  })

  # ── Reprice modal popup ──
  sr_show_reprice_modal <- function() {
    showModal(modalDialog(
      title = tagList(icon("dollar-sign"), "Reprice Segment"),
      size = "m", easyClose = TRUE,
      fluidRow(
        column(6,
          selectInput("sr_pricing_bps", "Basis Points (BPS)", width = "100%",
            choices = c("50" = "50", "100" = "100", "150" = "150",
                        "200" = "200", "300" = "300", "Custom" = "Custom"),
            selected = "200")
        ),
        column(6,
          shinyjs::hidden(div(id = "sr_custom_bps_div",
            numericInput("sr_pricing_custom", "Custom BPS", value = 200, min = -500, max = 1000, step = 25)
          ))
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("sr_confirm_reprice", "Apply Reprice", icon = icon("check"), class = "btn-success")
      )
    ))
  }

  # ── 2+ rate by band ──
  output$sr_band_bars <- plotly::renderPlotly({
    req(sr_rv$selected_seg)
    seg <- sr_rv$selected_seg
    req(seg$primary_var, seg$cutoff_val)

    df <- sr_filtered()
    df <- df[df$issued_flag == 1, ]

    irr_cache <- tryCatch(loan_irr_cache(), error = function(e) {
      message("[Scanner] loan_irr_cache() failed: ", e$message)
      NULL
    })
    if (!is.null(irr_cache) && "loan_irr" %in% names(irr_cache)) {
      irr_lookup <- setNames(irr_cache$loan_irr, irr_cache$loan_id)
      df$loan_irr <- irr_lookup[as.character(df$loan_id)]
    } else {
      df$loan_irr <- NA_real_
    }

    # Filter to loans matching secondary conditions (vary only primary variable)
    df <- build_boundary_population(df, seg)

    mb <- compute_marginal_bands(df, seg$primary_var, seg$cutoff_val, seg$direction)
    req(mb)

    colors <- ifelse(grepl("in-segment", mb$bands$band), "#ef4444",
              ifelse(grepl("Marginal out", mb$bands$band), "#f59e0b", "#10b981"))

    plotly::plot_ly(mb$bands, x = ~band, y = ~bad_rate, type = "bar",
                    marker = list(color = colors),
                    text = ~paste0(scales::percent(bad_rate, accuracy = 0.1),
                                   "\n", fmt_k(count), " loans"),
                    textposition = "outside") %>%
      plotly::layout(
        title = list(text = "2+ Rate by Band", font = list(size = 13)),
        xaxis = list(title = ""),
        yaxis = list(title = "2+ Rate", tickformat = ".1%"),
        paper_bgcolor = "#111827", plot_bgcolor = "#111827",
        font = list(color = "#9ca3af")
      )
  })

  # ── Band detail table ──
  output$sr_band_table <- DT::renderDataTable({
    req(sr_rv$selected_seg)
    seg <- sr_rv$selected_seg
    req(seg$primary_var, seg$cutoff_val)

    df <- sr_filtered()
    df <- df[df$issued_flag == 1, ]

    irr_cache <- tryCatch(loan_irr_cache(), error = function(e) {
      message("[Scanner] loan_irr_cache() failed: ", e$message)
      NULL
    })
    if (!is.null(irr_cache) && "loan_irr" %in% names(irr_cache)) {
      irr_lookup <- setNames(irr_cache$loan_irr, irr_cache$loan_id)
      df$loan_irr <- irr_lookup[as.character(df$loan_id)]
    } else {
      df$loan_irr <- NA_real_
    }

    # Filter to loans matching secondary conditions
    df <- build_boundary_population(df, seg)

    mb <- compute_marginal_bands(df, seg$primary_var, seg$cutoff_val, seg$direction)
    req(mb)

    tbl <- mb$bands

    # Keep only the band rows without segment total
    tbl <- tbl[tbl$band %in% c("Deep in-segment", "Marginal in-segment",
                                "Marginal out-segment", "Safe zone"), ]

    # Flag which bands are the CART-recommended action zone. The "in-segment"
    # bands represent the loans the tool recommends acting on (decline/reprice),
    # so highlight those rows in a light color to draw the eye to the
    # recommendation.
    is_recommended <- tbl$band %in% c("Deep in-segment", "Marginal in-segment")

    tbl$bad_rate <- scales::percent(tbl$bad_rate, accuracy = 0.1)
    tbl$avg_irr  <- ifelse(is.na(tbl$avg_irr), "N/A",
                           scales::percent(tbl$avg_irr, accuracy = 0.1))
    tbl$avg_var  <- round(tbl$avg_var, 2)
    tbl$is_recommended <- is_recommended
    names(tbl) <- c("Band", "Loans", "2+ Rate", "Avg IRR",
                     paste("Avg", seg$primary_var), "is_recommended")

    # Hide the is_recommended helper column from display but keep it in the
    # data for row-styling via formatStyle.
    recommended_col_idx <- which(names(tbl) == "is_recommended") - 1L  # 0-based for DT

    DT::datatable(
      tbl,
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE,
        columnDefs = list(list(visible = FALSE, targets = recommended_col_idx))
      ),
      rownames = FALSE,
      class = "compact stripe"
    ) %>%
      DT::formatStyle(
        "is_recommended",
        target = "row",
        backgroundColor = DT::styleEqual(c(TRUE, FALSE),
                                         c("#fff5e6", "transparent")),
        color = DT::styleEqual(c(TRUE, FALSE),
                               c("#1f2937", ""))
      )
  })

  # ── Chart insight ──
  output$sr_chart_insight <- renderUI({
    req(sr_rv$selected_seg)
    seg <- sr_rv$selected_seg
    req(seg)
    m <- sr_rv$portfolio_metrics
    req(m)

    # Build dynamic insight based on segment metrics
    var_label <- gsub("_", " ", seg$primary_var)
    cutoff_fmt <- if (grepl("income|amount|payment", seg$primary_var, ignore.case = TRUE))
      paste0("$", formatC(round(seg$cutoff_val), format = "f", digits = 0, big.mark = ","))
    else round(seg$cutoff_val, 2)
    dir_text <- if (seg$direction == "below") "below" else "above"

    # Determine severity and action
    if (seg$lift >= 3) {
      action_text <- paste0("Strong decline candidate. The in-segment 2+ rate is ",
        seg$lift, "x the portfolio average — consider declining or tightening the cutoff.")
      icon_name <- "triangle-exclamation"
      border_col <- "#ef4444"
    } else if (seg$lift >= 2) {
      action_text <- paste0("Reprice or tighten. At ", seg$lift,
        "x portfolio 2+ rate, repricing by +100-200 bps may restore IRR without losing volume.")
      icon_name <- "circle-exclamation"
      border_col <- "#f59e0b"
    } else {
      action_text <- paste0("Monitor closely. Lift of ", seg$lift,
        "x is moderate — use the slider below to test tighter cutoffs before acting.")
      icon_name <- "circle-info"
      border_col <- "#3b82f6"
    }

    div(style = paste0("margin-top:10px; padding:10px 12px; border-left:3px solid ",
                        border_col, "; background:#1f2937; border-radius:4px;"),
      div(style = "display:flex; align-items:flex-start; gap:8px;",
        icon(icon_name, style = paste0("color:", border_col, "; margin-top:3px;")),
        div(
          div(style = "color:#e5e7eb; font-size:12px; font-weight:600; margin-bottom:4px;",
            paste0("Loans with ", var_label, " ", dir_text, " ", cutoff_fmt,
                   " show ", scales::percent(seg$bad_rate, accuracy = 0.1),
                   " 2+ rate",
                   if (!is.null(seg$projected_bad_rate) &&
                       abs(seg$projected_bad_rate - seg$bad_rate) > 0.01)
                     paste0(" (", scales::percent(seg$projected_bad_rate, accuracy = 0.1),
                            " projected lifetime)")
                   else "",
                   " vs. ", scales::percent(m$bad_rate, accuracy = 0.1),
                   " portfolio baseline.")),
          div(style = paste0("color:", border_col, "; font-size:12px; font-weight:bold;"),
            action_text)
        )
      )
    )
  })

  # ── Table insight (IRR segments only — delinquency trees lack marginal bands) ──
  output$sr_table_insight <- renderUI({
    req(sr_rv$selected_seg)
    seg <- sr_rv$selected_seg
    req(seg$primary_var, seg$cutoff_val)
    if (identical(seg$source, "delinquency")) return(NULL)

    df <- sr_filtered()
    df <- df[df$issued_flag == 1, ]

    irr_cache <- tryCatch(loan_irr_cache(), error = function(e) {
      message("[Scanner] loan_irr_cache() failed: ", e$message)
      NULL
    })
    if (!is.null(irr_cache) && "loan_irr" %in% names(irr_cache)) {
      irr_lookup <- setNames(irr_cache$loan_irr, irr_cache$loan_id)
      df$loan_irr <- irr_lookup[as.character(df$loan_id)]
    } else {
      df$loan_irr <- NA_real_
    }

    # Filter to loans matching secondary conditions
    df <- build_boundary_population(df, seg)

    mb <- compute_marginal_bands(df, seg$primary_var, seg$cutoff_val, seg$direction)
    req(mb)

    bands <- mb$bands
    # Find the marginal bands
    marginal_in  <- bands[grepl("Marginal in", bands$band), ]
    marginal_out <- bands[grepl("Marginal out", bands$band), ]
    deep_in      <- bands[grepl("Deep in", bands$band), ]
    safe         <- bands[grepl("Safe", bands$band), ]

    # Build insight about the gradient
    insight_parts <- c()

    if (nrow(deep_in) > 0 && nrow(safe) > 0) {
      bad_drop <- deep_in$bad_rate[1] - safe$bad_rate[1]
      insight_parts <- c(insight_parts,
        paste0("2+ rate drops from ",
          scales::percent(deep_in$bad_rate[1], accuracy = 0.1), " (deep in-segment) to ",
          scales::percent(safe$bad_rate[1], accuracy = 0.1), " (safe zone) — a ",
          scales::percent(bad_drop, accuracy = 0.1), " absolute spread."))
    }

    if (nrow(marginal_in) > 0 && nrow(marginal_out) > 0) {
      marginal_gap <- marginal_in$bad_rate[1] - marginal_out$bad_rate[1]
      if (marginal_gap > 0.005) {
        insight_parts <- c(insight_parts,
          paste0("The marginal zone shows a clear ",
            scales::percent(marginal_gap, accuracy = 0.1),
            " jump at the boundary — the cutoff is well-placed."))
      } else {
        insight_parts <- c(insight_parts,
          "The marginal bands have similar 2+ rates — consider shifting the cutoff to capture more risk reduction.")
      }
    }

    if (nrow(marginal_in) > 0) {
      insight_parts <- c(insight_parts,
        paste0("The marginal in-segment band has ",
          scales::comma(marginal_in$count[1]),
          " loans — these are candidates for repricing rather than outright decline."))
    }

    div(style = "margin-top:10px; padding:10px 12px; border-left:3px solid #8b5cf6; background:#1f2937; border-radius:4px;",
      div(style = "display:flex; align-items:flex-start; gap:8px;",
        icon("table", style = "color:#8b5cf6; margin-top:3px;"),
        div(
          div(style = "color:#e5e7eb; font-size:12px; font-weight:600; margin-bottom:4px;",
            "Band Breakdown"),
          lapply(insight_parts, function(txt) {
            div(style = "color:#d1d5db; font-size:12px; margin-bottom:3px;",
              paste0("\u2022 ", txt))
          })
        )
      )
    )
  })

  # ── Cutoff slider impact ──
  output$sr_slider_impact <- renderUI({
    req(sr_rv$selected_seg, input$sr_cutoff_slider)
    seg <- sr_rv$selected_seg
    req(seg$primary_var)

    df <- sr_filtered()
    df <- df[df$issued_flag == 1, ]

    # Filter to loans matching secondary conditions
    df <- build_boundary_population(df, seg)

    new_cutoff      <- input$sr_cutoff_slider

    # Use actual CART membership for original, cutoff-based for new
    pv <- df[[seg$primary_var]]
    pv_ok <- !is.na(pv)
    originally_in <- if ("in_segment" %in% names(df)) df$in_segment else {
      if (seg$direction == "below") pv_ok & pv < seg$cutoff_val
      else pv_ok & pv >= seg$cutoff_val
    }
    if (seg$direction == "below") {
      now_in <- pv_ok & pv < new_cutoff
    } else {
      now_in <- pv_ok & pv >= new_cutoff
    }

    # Guard against NA propagation in logical vectors
    now_in[is.na(now_in)] <- FALSE
    originally_in[is.na(originally_in)] <- FALSE

    delta_loans   <- sum(now_in, na.rm = TRUE) - sum(originally_in, na.rm = TRUE)
    new_bad_rate  <- mean(df$performance_delinquency_flag[now_in], na.rm = TRUE)
    orig_bad_rate <- mean(df$performance_delinquency_flag[originally_in], na.rm = TRUE)

    # Portfolio-level impact if segment is declined
    remaining <- df[!now_in, ]
    new_portfolio_bad <- mean(remaining$performance_delinquency_flag, na.rm = TRUE)
    baseline_bad <- sr_rv$portfolio_metrics$bad_rate

    # NA-safe comparisons
    delta_loans <- ifelse(is.na(delta_loans), 0L, delta_loans)
    delta_sign <- if (delta_loans >= 0) "+" else ""
    bad_delta  <- ifelse(is.na(new_portfolio_bad) || is.na(baseline_bad), 0,
                         new_portfolio_bad - baseline_bad)

    div(style = "padding-top:10px;",
      fluidRow(
        column(3, kpi_box(
          paste0(delta_sign, fmt_k(abs(delta_loans))),
          "Loans Shifted",
          if (delta_loans > 0) "more in segment" else "fewer in segment")),
        column(3, kpi_box(
          scales::percent(ifelse(is.nan(new_bad_rate), 0, new_bad_rate), accuracy = 0.1),
          "Segment 2+ Rate",
          paste("was:", scales::percent(ifelse(is.nan(orig_bad_rate), 0, orig_bad_rate), accuracy = 0.1)))),
        column(3, kpi_box(
          fmt_k(sum(now_in, na.rm = TRUE)),
          "Segment Size",
          paste("was:", fmt_k(sum(originally_in, na.rm = TRUE))))),
        column(3, kpi_box(
          paste0(if (bad_delta <= 0) "" else "+",
                 scales::percent(bad_delta, accuracy = 0.1)),
          "Portfolio 2+ Rate",
          "if segment declined"))
      )
    )
  })

  # ── Show/hide custom BPS input ──
  observeEvent(input$sr_pricing_bps, {
    if (identical(input$sr_pricing_bps, "Custom")) {
      shinyjs::show("sr_custom_bps_div")
    } else {
      shinyjs::hide("sr_custom_bps_div")
    }
  }, ignoreInit = TRUE)

  # ── Show reprice modal on drilldown Reprice click ──
  observeEvent(input$sr_apply_reprice, {
    sr_show_reprice_modal()
  })

  # ── Core action engine (shared by Decline and Reprice) ──
  sr_apply_action <- function(action) {
    .sa_t0 <- Sys.time()
    on.exit(if (getOption("csm_debug", FALSE)) cat(sprintf("[%s] [Scanner] sr_apply_action(%s): %.2fs\n",
                        format(Sys.time(), "%H:%M:%OS3"), action,
                        as.numeric(difftime(Sys.time(), .sa_t0, units = "secs"))),
                file = stderr()), add = TRUE)
    seg <- sr_rv$selected_seg
    req(seg)

    # ---- Build engine baseline (full L10-L90 cascade) ----
    cd_filtered  <- sr_filtered()
    pid_filtered <- policy_input_data[sr_row_mask(), , drop = FALSE]
    default_params <- build_default_params(policy_manifest)
    # Scanner is hardcoded to the PL-CURRENT production policy — apply its
    # parameter overrides on top of generic defaults so baseline metrics
    # match the production policy, not the raw factory defaults.
    if (exists("strategy_overrides") &&
        !is.null(strategy_overrides[["PL-CURRENT"]])) {
      pl_current_overrides <- strategy_overrides[["PL-CURRENT"]]$params
      if (!is.null(pl_current_overrides) && length(pl_current_overrides) > 0) {
        default_params <- modifyList(default_params, pl_current_overrides)
      }
    }
    baseline_df <- apply_policy_with_params(pid_filtered, default_params)

    # Carry historical/performance columns from credit_data into baseline
    carry_cols <- c("issued_flag", "issued_amount", "performance_delinquency_flag",
                    "booking_vintage", "fico_band", "borrower_repeat_type",
                    "mob_bins", "date", "months_on_books", "product_type")
    for (col in carry_cols) {
      if (col %in% names(cd_filtered) && !(col %in% names(baseline_df)))
        baseline_df[[col]] <- cd_filtered[[col]]
    }
    baseline_df$issued_flag <- cd_filtered$issued_flag
    baseline_df$interest_rate_apr <- ifelse(
      baseline_df$offered_decision_flag == "APPR",
      baseline_df$offered_final_apr,
      cd_filtered$interest_rate_apr)

    df_full <- baseline_df

    # ---- Identify in-segment loans using stored loan_ids ----
    # Use seg$loan_ids (from CART slice assignment) for exact membership.
    # Do NOT re-apply the tree to cd_filtered — the tree was trained on a
    # slice (e.g. Partner: Bank_Channel) and would match loans from other
    # slices that happen to have similar profiles, inflating the count.
    in_segment <- cd_filtered$loan_id %in% seg$loan_ids

    # Verify segment count matches card
    n_issued_in_seg <- sum(in_segment & cd_filtered$issued_flag == 1, na.rm = TRUE)
    message("[Scanner] Segment card: ", seg$n, " issued | Action targets: ",
            n_issued_in_seg, " issued (full pop in_segment: ", sum(in_segment), ")")

    in_segment_issued <- in_segment & (df_full$issued_flag == 1)

    # ---- Build proposed dataset ----
    df_proposed <- df_full
    adverse_flip_ids <- character(0)
    bps_val <- 0
    # Track step-by-step intermediates for the analysis popup.
    analysis_steps <- list(
      elast_pct   = NA_real_,
      drop_rate   = NA_real_,
      n_drop      = 0L,
      adv_pct     = NA_real_,
      adverse_adj = NA_real_,
      n_flip      = 0L,
      n_rescue    = 0L)

    if (action == "Decline") {
      to_decline <- in_segment & (
        (!is.na(df_full$offered_decision_flag) &
         df_full$offered_decision_flag == "APPR") |
        (df_full$issued_flag == 1)
      )
      df_proposed$offered_decision_flag[to_decline] <- "DECL"
      df_proposed$issued_flag[to_decline] <- 0L
    } else {
      # Reprice
      bps_val <- if (identical(input$sr_pricing_bps, "Custom"))
                   suppressWarnings(as.numeric(input$sr_pricing_custom))
                 else
                   suppressWarnings(as.numeric(input$sr_pricing_bps))
      if (is.na(bps_val) || length(bps_val) == 0) bps_val <- 0

      if (bps_val != 0) {
        to_price <- in_segment &
          !is.na(df_full$offered_decision_flag) &
          df_full$offered_decision_flag == "APPR"
        df_proposed$adjusted_apr_pre_term_cap[to_price] <-
          df_proposed$adjusted_apr_pre_term_cap[to_price] + bps_val / 10000

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

        df_proposed$interest_rate_apr <- ifelse(
          df_proposed$offered_decision_flag == "APPR",
          df_proposed$offered_final_apr,
          df_proposed$interest_rate_apr)

        newly_declined <- df_proposed$offered_decision_flag != "APPR" &
                          df_full$offered_decision_flag == "APPR"
        df_proposed$issued_flag[newly_declined] <- 0L

        # Price elasticity — only drops loans when price RISES.
        # Negative bps (price cut) should INCREASE take-up, not decrease it;
        # using abs(bps_val) would incorrectly drop high-FICO loans on a rate cut.
        # Pass seg$primary_var so the waterfall's Layer 1 can match this
        # segment against direct V4 test/control evidence when one exists.
        seg_grades <- df_full$risk_grade[in_segment & df_full$issued_flag == 1]
        elast_pct  <- get_segment_assumption(seg_grades, "elasticity",
                                             primary_var = seg$primary_var) / 100
        analysis_steps$elast_pct <- elast_pct
        in_seg_issued <- in_segment_issued & df_full$issued_flag == 1
        n_seg_issued  <- sum(in_seg_issued, na.rm = TRUE)

        if (bps_val > 0) {
          drop_rate <- min((bps_val / 100) * elast_pct, 0.50)
          n_drop    <- round(n_seg_issued * drop_rate)
          analysis_steps$drop_rate <- drop_rate
          analysis_steps$n_drop    <- as.integer(n_drop)
          if (n_drop > 0 && n_seg_issued > 0) {
            seg_issued_idx <- which(in_seg_issued)
            fico_order <- order(df_full$fico_score[seg_issued_idx], decreasing = TRUE)
            drop_idx   <- seg_issued_idx[fico_order[seq_len(n_drop)]]
            df_proposed$issued_flag[drop_idx] <- 0L
          }
        }
        # Negative BPS: elasticity produces a modest uplift to take-up of
        # currently-approved loans in the segment that historically did not
        # accept the higher-APR offer. These are boundary cases that Layer 90
        # already re-approved via affordability relaxation; the elasticity
        # lift tags them as "now accepted" without needing to synthesize new
        # loans. This is deliberately conservative: a rate cut in Scanner is
        # still a "reprice" action on existing segment loans, not a swap-in.
        # For a true swap-in of declined populations, use Opportunity Agent.

        # Adverse selection (price up) / Positive selection (price down)
        adv_pct <- get_segment_assumption(seg_grades, "adverse_selection") / 100
        analysis_steps$adv_pct <- adv_pct
        if (adv_pct > 0 && bps_val > 0) {
          # Price UP: good borrowers leave, bad borrowers stay → flip some
          # low-FICO good loans to delinquent to represent the mix shift.
          adverse_adj <- (bps_val / 100) * adv_pct
          still_issued <- in_segment & df_proposed$issued_flag == 1
          good_in_seg  <- which(still_issued &
                                  df_proposed$performance_delinquency_flag == 0)
          n_flip <- round(length(good_in_seg) * adverse_adj)
          analysis_steps$adverse_adj <- adverse_adj
          analysis_steps$n_flip      <- as.integer(n_flip)
          if (n_flip > 0 && length(good_in_seg) > 0) {
            set.seed(123)
            fico_ord <- order(df_proposed$fico_score[good_in_seg])
            flip_idx <- good_in_seg[fico_ord[seq_len(min(n_flip, length(good_in_seg)))]]
            df_proposed$performance_delinquency_flag[flip_idx] <- 1L
            adverse_flip_ids <- as.character(df_proposed$loan_id[flip_idx])
          }
        } else if (adv_pct > 0 && bps_val < 0) {
          # Price DOWN: positive selection — the best borrowers, who had been
          # priced out or were marginal takers, become more willing to accept.
          # Model this symmetrically by flipping some low-FICO bad loans to
          # good, representing the reduction in adverse mix when pricing
          # improves. The magnitude is capped at the same rate as the
          # adverse-selection model, but in the opposite direction.
          positive_adj <- (abs(bps_val) / 100) * adv_pct
          still_issued <- in_segment & df_proposed$issued_flag == 1
          bad_in_seg   <- which(still_issued &
                                  df_proposed$performance_delinquency_flag == 1)
          n_rescue <- round(length(bad_in_seg) * positive_adj)
          analysis_steps$adverse_adj <- positive_adj
          analysis_steps$n_rescue    <- as.integer(n_rescue)
          if (n_rescue > 0 && length(bad_in_seg) > 0) {
            set.seed(123)
            fico_ord <- order(df_proposed$fico_score[bad_in_seg], decreasing = TRUE)
            rescue_idx <- bad_in_seg[fico_ord[seq_len(min(n_rescue, length(bad_in_seg)))]]
            df_proposed$performance_delinquency_flag[rescue_idx] <- 0L
          }
        }
      }
    }

    # ---- Compute funnel metrics ----
    bl <- compute_funnel(df_full)
    pr <- compute_funnel(df_proposed)

    bl_seg_apr <- mean(
      df_full$interest_rate_apr[in_segment & df_full$issued_flag == 1], na.rm = TRUE)
    pr_seg_apr <- mean(
      df_proposed$interest_rate_apr[in_segment & df_proposed$issued_flag == 1], na.rm = TRUE)

    seg_size      <- sum(in_segment, na.rm = TRUE)
    seg_appr      <- sum(in_segment & (
      (!is.na(df_full$offered_decision_flag) & df_full$offered_decision_flag == "APPR") |
      (df_full$issued_flag == 1)), na.rm = TRUE)
    bl_seg_issued <- sum(in_segment & df_full$issued_flag == 1, na.rm = TRUE)
    pr_seg_issued <- sum(in_segment & df_proposed$issued_flag == 1, na.rm = TRUE)

    # Store global reactive state
    sr_rv$bps <- bps_val
    sr_rv$adverse_flip_ids <- adverse_flip_ids

    # ---- Build sample preview data ----
    sample_preview <- NULL
    sample_full <- NULL
    sample_all  <- NULL
    n_affected  <- 0L

    if (action == "Decline") {
      changed <- which(
        in_segment & df_full$issued_flag == 1 & df_proposed$issued_flag == 0)
      if (length(changed) > 0) {
        sample_df <- data.frame(
          loan_id           = df_full$loan_id[changed],
          fico_score        = df_full$fico_score[changed],
          risk_grade        = df_full$risk_grade[changed],
          original_decision = df_full$offered_decision_flag[changed],
          new_decision      = "DECL",
          original_issued   = df_full$issued_amount[changed],
          new_issued        = 0,
          stringsAsFactors  = FALSE)
        sample_all  <- sample_df
        set.seed(42)
        sample_full <- head(sample_df[sample(nrow(sample_df)), ], 50)
        sample_preview <- head(sample_full, 5)
        n_affected <- length(changed)
      }
    } else {
      changed <- which(
        in_segment_issued &
        !is.na(df_full$interest_rate_apr) &
        !is.na(df_proposed$interest_rate_apr) &
        abs(df_proposed$interest_rate_apr - df_full$interest_rate_apr) > 1e-6)
      if (length(changed) > 0) {
        sample_df <- data.frame(
          loan_id      = df_full$loan_id[changed],
          fico_score   = df_full$fico_score[changed],
          risk_grade   = df_full$risk_grade[changed],
          original_apr = round(df_full$interest_rate_apr[changed] * 100, 2),
          new_apr      = round(df_proposed$interest_rate_apr[changed] * 100, 2),
          apr_change_bps = round(
            (df_proposed$interest_rate_apr[changed] - df_full$interest_rate_apr[changed]) * 10000),
          stringsAsFactors = FALSE)
        sample_all  <- sample_df
        set.seed(42)
        sample_full <- head(sample_df[sample(nrow(sample_df)), ], 50)
        sample_preview <- head(sample_full, 5)
        n_affected <- length(changed)
      }
    }

    # ---- Store in A/B simulation slot ----
    slot <- input$sr_sim_slot %||% "A"
    slot_label <- if (slot == "A") "Primary" else "Alternative"
    sim_data <- list(
      df_full = df_full, df_proposed = df_proposed,
      in_segment = in_segment, action = action, bps = bps_val,
      elast = tryCatch(
        get_segment_assumption(df_full$risk_grade[in_segment & df_full$issued_flag == 1],
                               "elasticity",
                               primary_var = seg$primary_var),
        error = function(e) {
          message("[Scanner] elasticity lookup failed, using default 5: ", e$message)
          5
        }),
      bl = bl, pr = pr,
      bl_seg_apr = bl_seg_apr, pr_seg_apr = pr_seg_apr,
      seg_size = seg_size, seg_appr = seg_appr,
      bl_seg_issued = bl_seg_issued, pr_seg_issued = pr_seg_issued,
      label = slot_label,
      segment_condition = seg$display_rule %||% seg$rule %||% "",
      saved_tree = seg$tree, saved_tree_vars = seg$tree_vars,
      saved_node = seg$leaf_id,
      adverse_flip_ids = adverse_flip_ids,
      sample_preview = sample_preview,
      sample_full = sample_full,
      sample_all = sample_all,
      n_affected = n_affected,
      analysis_steps  = analysis_steps,
      n_seg_issued    = bl_seg_issued
    )
    sr_rv[[paste0("sim_", slot)]] <- sim_data
    sr_rv$last_sim <- sim_data

    verb <- if (action == "Decline") "declined" else "repriced"
    showNotification(
      paste0(slot_label, ": ", scales::comma(n_affected), " loans ", verb),
      type = "message", duration = 4)
  }

  # ── Helper: render the step-by-step Action Analysis modal ──
  .sr_step_row <- function(n, title, detail, active) {
    pct <- scales::percent
    bg   <- if (active) "#1e3a5f" else "#1a1f2e"
    bord <- if (active) "3px solid #fb6e36" else "3px solid #374151"
    pill <- if (active) tags$span(style = "background:#fb6e36; color:white; font-size:10px; font-weight:700; padding:2px 8px; border-radius:10px; margin-left:8px;", "APPLIED")
             else tags$span(style = "background:#374151; color:#9ca3af; font-size:10px; font-weight:600; padding:2px 8px; border-radius:10px; margin-left:8px;", "n/a")
    div(style = sprintf("background:%s; border-left:%s; padding:10px 14px; margin-bottom:8px; border-radius:4px;", bg, bord),
        div(style = "display:flex; align-items:center; margin-bottom:4px;",
            tags$span(style = "color:#fb6e36; font-weight:700; margin-right:8px;", paste0("Step ", n)),
            tags$span(style = sprintf("color:%s; font-weight:600;", if (active) "white" else "#9ca3af"), title),
            pill),
        div(style = sprintf("color:%s; font-size:13px;", if (active) "#d1d5db" else "#6b7280"), detail))
  }

  sr_show_analysis_modal <- function(sim) {
    if (is.null(sim) || is.null(sim$analysis_steps)) return(invisible())
    s <- sim$analysis_steps
    bps <- sim$bps %||% 0
    action <- sim$action

    # Portfolio-level deltas for header
    fmt_pp <- function(x) sprintf("%+.2fpp", x * 100)
    bl_bad <- sim$bl$bad_rate %||% NA_real_
    pr_bad <- sim$pr$bad_rate %||% NA_real_
    # IRR is not carried on sim$bl/pr (compute_funnel returns funnel counts only).
    # Resolve from the scanner's shared compute cache, which already holds
    # baseline + proposed XIRR for this sim slot.
    cache_entry <- tryCatch(sr_cache_for(sim), error = function(e) NULL)
    bl_irr <- cache_entry$bl_xirr %||% NA_real_
    pr_irr <- cache_entry$pr_xirr %||% NA_real_

    # Audit-log style: says what ran, not how. Rendered via analysis_engine_static
    # so the recap matches the live "ANALYSIS ENGINE" pipeline visual language.
    is_pricing <- action == "Pricing" && bps != 0
    step_pricing_status <- if (is_pricing) "done" else "skipped"

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
      title = tagList(icon("chart-line"), " Action Analysis \u2014 How the numbers were computed"),
      size = "l", easyClose = TRUE,
      analysis_engine_static(steps, summary = summary),
      footer = modalButton("Close")
    ))
  }

  # ── Decline action ──
  observeEvent(input$sr_apply_decline, {
    req(sr_rv$selected_seg)
    shinyjs::show("sr_action_overlay")
    sr_apply_action("Decline")
    sr_show_analysis_modal(sr_rv$last_sim)
    # Overlay hidden by sr_action_results renderUI when output is ready
  })

  # ── Reprice confirm (from modal) ──
  observeEvent(input$sr_confirm_reprice, {
    req(sr_rv$selected_seg)
    removeModal()
    shinyjs::show("sr_action_overlay")
    sr_apply_action("Pricing")
    sr_show_analysis_modal(sr_rv$last_sim)
    # Overlay hidden by sr_action_results renderUI when output is ready
    # Scroll moved to sr_action_results renderUI so it fires after content is ready
  })

  # ── Jump to Opportunity Agent with the selected segment's variables
  #    pre-loaded into the Risk Segmentation picker. Lets the user take a
  #    scanner recommendation and keep cutting it without re-choosing vars.
  observeEvent(input$sr_goto_opp, {
    seg <- sr_rv$selected_seg
    # Carry only variables actually USED in the segment's rule, not every
    # tree candidate. seg$tree_vars is the full ~50-var input set the
    # Scanner trained on — passing that would select all 50 in Opp Explorer.
    rule_text <- seg$rule %||% seg$display_rule %||% ""
    carry <- extract_rule_vars(rule_text)
    if (length(carry) == 0 && !is.null(seg$primary_var))
      carry <- seg$primary_var
    carry <- carry[!is.na(carry) & nzchar(carry)]
    if (length(carry) > 0) {
      av <- if (exists("analysis_vars", envir = globalenv()))
              get("analysis_vars", envir = globalenv()) else character(0)
      carry <- intersect(carry, unname(av))
      if (length(carry) > 0) {
        shinyWidgets::updatePickerInput(session, "opp_vars", selected = carry)
        showNotification(
          paste0("Carried ", length(carry), " variable(s) into Opportunity Agent."),
          type = "message", duration = 3)
      }
    }
    # Stash BOTH the rule text (for display) and the exact loan_ids (for the
    # drill-down subset). Rule text alone matches more loans than the actual
    # CART leaf; without loan_ids, Opp Explorer's Run Segmentation total
    # won't tie to Scanner's segment size. Same contract as the "Deeper cuts"
    # handoff (sr_deeper_*).
    if (nzchar(rule_text)) session$userData$deeper_cuts_rule <- rule_text
    session$userData$deeper_cuts_loan_ids <-
      if (!is.null(seg$loan_ids)) as.character(seg$loan_ids) else NULL
    updateTabItems(session, "sidebar_menu", "opp_find")
  })

  # ── Clear simulation slots ──
  observeEvent(input$sr_clear_A, {
    sr_rv$sim_A <- NULL
    showNotification("Primary action cleared.", type = "message", duration = 2)
  })
  observeEvent(input$sr_clear_B, {
    sr_rv$sim_B <- NULL
    showNotification("Alternative action cleared.", type = "message", duration = 2)
  })

  # ── Decline action — fires immediately and scrolls to results ──
  observeEvent(input$sr_act_decline, {
    sel <- .sr_get_selected_seg(); req(sel)
    sr_rv$selected_seg <- sel$seg
    shinyjs::show("sr_action_overlay")
    sr_apply_action("Decline")
    # Overlay hidden + scroll handled by sr_action_results renderUI when ready.
  }, ignoreInit = TRUE)

  # ── Reprice action — opens reprice modal ──
  observeEvent(input$sr_act_reprice, {
    sel <- .sr_get_selected_seg(); req(sel)
    sr_rv$selected_seg <- sel$seg
    sr_show_reprice_modal()
  }, ignoreInit = TRUE)

  # ==================================================================
  # RESULTS PANEL — matches Opportunity Finder layout:
  # Sample Preview → Scenario Summary → KPIs → SQL Buttons →
  # Analysis Filters → Funnel+IRR side-by-side → Vintage → Save
  # ==================================================================

  sr_slot_data <- function(slot) {
    sr_rv[[paste0("sim_", slot)]]
  }

  # ── Simulation compute cache ──
  # All expensive post-action computations (projected bad rate, portfolio
  # XIRR) are computed once per sim state change and reused across
  # renderers. Previously the KPI row, funnel table, IRR plot and vintage
  # chart each called compute_projected_bad_rate / XIRR on the same loan
  # universe, producing 4-8 redundant calls per Decline click (~5-10s).
  # See CLAUDE.md perf notes for rationale. The cache is a single
  # reactive that renderers share; it invalidates whenever sim_A or sim_B
  # changes. Combined with the session-level sr_baseline_memo below,
  # the second and subsequent clicks return in <0.2s because the
  # baseline XIRR and per-loan projection vector are reused unchanged.
  sr_compute_cache <- reactive({
    simA <- sr_rv$sim_A
    simB <- sr_rv$sim_B
    if (is.null(simA) && is.null(simB)) return(NULL)
    .sr_build_cache(simA, simB)
  })

  # Session-level baseline memo: the Scanner is hardcoded to PL-CURRENT,
  # so df_f is identical across every Decline / Reprice click in a session.
  # Caching the baseline XIRR + per-loan projected bad rate eliminates
  # ~1.5s of duplicated XIRR work and ~0.6s of projection work on each
  # subsequent click after the first.
  sr_baseline_memo <- local({
    cache <- NULL
    list(
      get = function(key) {
        if (is.null(cache) || !identical(cache$key, key)) return(NULL)
        cache
      },
      set = function(key, bl_xirr, r_bl) {
        cache <<- list(key = key, bl_xirr = bl_xirr, r_bl = r_bl)
      }
    )
  })

  # Helper that does the actual compute. Caller supplies the sim slots
  # so this can be invoked from either a reactive or a plain call.
  .sr_build_cache <- function(simA, simB) {
    if (is.null(simA) && is.null(simB)) return(NULL)

    t0 <- Sys.time()
    if (getOption("csm_debug", FALSE)) cat(sprintf("[%s] [Scanner] .sr_build_cache START\n",
                format(t0, "%H:%M:%OS3")),
        file = stderr())
    tape <- tryCatch(
      isolate(sr_tape()),
      error = function(e) {
        message("[Scanner] sr_compute_cache: sr_tape() failed: ", e$message)
        NULL
      })

    compute_for_sim <- function(sim) {
      if (is.null(sim)) return(NULL)
      df_f <- sim$df_full
      df_p <- sim$df_proposed
      bl_ids <- as.character(df_f$loan_id[df_f$issued_flag == 1])
      pr_ids <- as.character(df_p$loan_id[df_p$issued_flag == 1])

      # Session baseline memo key: digest of the sorted baseline issued
      # ids. If the Scanner has been run multiple times on PL-CURRENT,
      # this key is stable across clicks and the memo hits cheaply.
      memo_key <- tryCatch(
        digest::digest(sort(bl_ids)),
        error = function(e) paste0("n=", length(bl_ids)))
      memo <- sr_baseline_memo$get(memo_key)

      bl_proj <- NA_real_
      pr_proj <- NA_real_
      r_bl <- NULL
      bl_xirr <- NA_real_

      if (!is.null(memo)) {
        bl_xirr <- memo$bl_xirr
        r_bl <- memo$r_bl
        if (!is.null(r_bl) && nrow(r_bl) > 0) {
          bl_proj <- mean(r_bl$projected_lifetime_prob, na.rm = TRUE)
        }
      } else if (!is.null(tape)) {
        # Baseline XIRR — compute once, memoize for the session.
        bl_xirr <- tryCatch(
          compute_adjusted_xirr_v2(df_f, tape, df_f),
          error = function(e) {
            message("[Scanner] sr_compute_cache baseline XIRR failed: ", e$message)
            NA_real_
          })
        # Baseline per-loan projected bad rate — compute once, memoize.
        tryCatch({
          if (length(bl_ids) > 0) {
            r_bl <- compute_projected_bad_rate(bl_ids, tape, df_f)
            if (!is.null(r_bl) && nrow(r_bl) > 0)
              bl_proj <- mean(r_bl$projected_lifetime_prob, na.rm = TRUE)
          }
        }, error = function(e) {
          message("[Scanner] sr_compute_cache projection failed: ", e$message)
        })
        sr_baseline_memo$set(memo_key, bl_xirr, r_bl)
      }

      # Proposed projected bad rate: derive from the cached baseline
      # per-loan vector by intersecting with the proposed issued ids.
      if (!is.null(r_bl) && nrow(r_bl) > 0 && length(pr_ids) > 0) {
        pr_in_bl <- pr_ids[pr_ids %in% r_bl$loan_id]
        if (length(pr_in_bl) == length(pr_ids)) {
          sub <- r_bl[r_bl$loan_id %in% pr_ids, , drop = FALSE]
          pr_proj <- mean(sub$projected_lifetime_prob, na.rm = TRUE)
        } else if (!is.null(tape)) {
          r_pr <- tryCatch(
            compute_projected_bad_rate(pr_ids, tape, df_p),
            error = function(e) NULL)
          if (!is.null(r_pr) && nrow(r_pr) > 0)
            pr_proj <- mean(r_pr$projected_lifetime_prob, na.rm = TRUE)
        }
      }

      # Proposed XIRR — cannot be memoized across clicks because it
      # depends on the specific declined/repriced segment.
      pr_xirr <- NA_real_
      if (!is.null(tape)) {
        pr_xirr <- tryCatch(
          compute_adjusted_xirr_v2(df_p, tape, df_f),
          error = function(e) {
            message("[Scanner] sr_compute_cache proposed XIRR failed: ", e$message)
            NA_real_
          })
      }

      list(
        bl_ids = bl_ids, pr_ids = pr_ids,
        bl_proj_bad_rate = bl_proj, pr_proj_bad_rate = pr_proj,
        bl_xirr = bl_xirr, pr_xirr = pr_xirr
      )
    }

    cache <- list(
      tape_available = !is.null(tape),
      A = compute_for_sim(simA),
      B = compute_for_sim(simB),
      sim_A_ref = simA,
      sim_B_ref = simB
    )
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    if (getOption("csm_debug", FALSE)) cat(sprintf("[%s] [Scanner] sr_compute_cache: took %.2fs\n",
                format(Sys.time(), "%H:%M:%OS3"), elapsed),
        file = stderr())
    cache
  }

  # Helper: look up cached entry for a given sim slot by pointer identity.
  sr_cache_for <- function(sim) {
    cache <- sr_compute_cache()
    if (is.null(cache) || is.null(sim)) return(NULL)
    if (!is.null(sr_rv$sim_A) && identical(sim, sr_rv$sim_A)) return(cache$A)
    if (!is.null(sr_rv$sim_B) && identical(sim, sr_rv$sim_B)) return(cache$B)
    NULL
  }

  # ── KPI row builder ──
  sr_build_kpi_row <- function(sim, label_prefix = "") {
    df_f <- sim$df_full
    df_p <- sim$df_proposed
    seg  <- sim$in_segment
    action <- sim$action
    bl <- compute_funnel(df_f)
    pr <- compute_funnel(df_p)
    seg_issued <- sum(seg & df_f$issued_flag == 1, na.rm = TRUE)
    # Use max(issued, approved) as denominator floor to prevent >100% take rate.
    # The policy re-run may decline loans that were originally approved (known
    # parameterised engine vs canonical engine discrepancy), causing issued > approved.
    seg_appr_bl <- max(sum(seg & df_f$offered_decision_flag == "APPR", na.rm = TRUE), seg_issued)
    seg_appr_pr <- max(sum(seg & df_p$offered_decision_flag == "APPR", na.rm = TRUE),
                       sum(seg & df_p$issued_flag == 1, na.rm = TRUE))
    bsi <- seg_issued
    psi <- sum(seg & df_p$issued_flag == 1, na.rm = TRUE)
    bl_conv <- if (seg_appr_bl > 0) bsi / seg_appr_bl else NA_real_
    pr_conv <- if (seg_appr_pr > 0) psi / seg_appr_pr else NA_real_
    ba  <- mean(df_f$interest_rate_apr[seg & df_f$issued_flag == 1], na.rm = TRUE)
    pa  <- mean(df_p$interest_rate_apr[seg & df_p$issued_flag == 1], na.rm = TRUE)
    seg_bl_br_raw <- mean(df_f$performance_delinquency_flag[seg & df_f$issued_flag == 1], na.rm = TRUE)
    seg_pr_br_raw <- mean(df_p$performance_delinquency_flag[seg & df_p$issued_flag == 1], na.rm = TRUE)
    seg_bl_br <- ifelse(is.na(seg_bl_br_raw), 0, seg_bl_br_raw * 100)
    seg_pr_br <- ifelse(is.na(seg_pr_br_raw), 0, seg_pr_br_raw * 100)
    bl_br <- ifelse(is.na(bl$bad_rate), 0, bl$bad_rate * 100)
    pr_br <- ifelse(is.na(pr$bad_rate), 0, pr$bad_rate * 100)

    kpi1 <- kpi_box(fmt_k(seg_issued), "Issued (Segment)",
      sub = paste0(fmt_k(seg_appr_bl), " approved"))

    if (action == "Decline") {
      kpi2 <- kpi_box(sprintf("%.1f%% \u2192 %.1f%%", bl$appr_rate * 100, pr$appr_rate * 100),
        "Approval Rate")
      delta_appr <- (pr$appr_rate - bl$appr_rate) * 100
      kpi3 <- kpi_box(sprintf("%+.1f pp", delta_appr), "Approval Rate Delta",
        sub = if (delta_appr < 0) "Tighter risk filter" else "Expanded approvals")
    } else {
      kpi2 <- kpi_box(sprintf("%.1f%% \u2192 %.1f%%",
        ifelse(is.na(bl_conv), 0, bl_conv * 100), ifelse(is.na(pr_conv), 0, pr_conv * 100)),
        "Segment Take Rate",
        sub = "issued / approved in segment")
      delta_conv <- (pr_conv - bl_conv) * 100
      kpi3 <- kpi_box(sprintf("%+.1f pp", ifelse(is.na(delta_conv), 0, delta_conv)),
        "Segment Take Rate Delta", sub = sprintf("%s \u2192 %s issued", fmt_k(bsi), fmt_k(psi)))
    }
    kpi4 <- kpi_box(sprintf("%.1f%% \u2192 %.1f%%", seg_bl_br, seg_pr_br),
      "Segment 2+ Rate")
    kpi5 <- kpi_box(sprintf("%.1f%% \u2192 %.1f%%",
      ifelse(is.na(ba), 0, ba * 100), ifelse(is.na(pa), 0, pa * 100)),
      "Avg APR (Segment)")
    # Compute projected portfolio bad rate for supplementary display.
    # Read from sr_compute_cache to avoid redundant compute_projected_bad_rate
    # calls (this KPI row and sr_funnel_table both need the same numbers).
    proj_sub_label <- tryCatch({
      entry <- sr_cache_for(sim)
      if (!is.null(entry) && !is.na(entry$bl_proj_bad_rate)) {
        bl_p <- entry$bl_proj_bad_rate
        pr_p <- entry$pr_proj_bad_rate
        if (abs(bl_p - bl$bad_rate) > 0.001)
          paste0(if (pr_br < bl_br) "Improved" else if (pr_br > bl_br) "Worsened" else "Unchanged",
                 " | projected: ", sprintf("%.1f%%", bl_p * 100),
                 " \u2192 ", sprintf("%.1f%%", pr_p * 100))
        else if (pr_br < bl_br) "Improved" else if (pr_br > bl_br) "Worsened" else "Unchanged"
      } else {
        if (pr_br < bl_br) "Improved" else if (pr_br > bl_br) "Worsened" else "Unchanged"
      }
    }, error = function(e) {
      message("[Scanner] sr_build_kpi_row projection label failed: ", e$message)
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

  # ── Full action results panel (matches Opp Finder layout) ──
  output$sr_action_results <- renderUI({
    .ar_t0 <- Sys.time()
    if (getOption("csm_debug", FALSE)) cat(sprintf("[%s] [Scanner] sr_action_results START\n",
                format(.ar_t0, "%H:%M:%OS3")),
        file = stderr())
    on.exit({
      shinyjs::hide("sr_action_overlay")
      if (getOption("csm_debug", FALSE)) cat(sprintf("[%s] [Scanner] sr_action_results END: %.2fs\n",
                        format(Sys.time(), "%H:%M:%OS3"),
                        as.numeric(difftime(Sys.time(), .ar_t0, units = "secs"))),
                file = stderr())
    }, add = TRUE)
    simA <- sr_rv$sim_A
    simB <- sr_rv$sim_B
    if (is.null(simA) && is.null(simB)) return(NULL)

    # KPI rows
    kpi_ui <- if (!is.null(simA) && !is.null(simB)) {
      tagList(
        sr_build_kpi_row(simA, simA$label %||% "Primary Action"),
        sr_build_kpi_row(simB, simB$label %||% "Alternative Action")
      )
    } else if (!is.null(simA)) {
      sr_build_kpi_row(simA)
    } else {
      sr_build_kpi_row(simB)
    }

    # ── Action Reasoning (UC4) ──
    # Build reasoning for most recent action
    action_reasoning_ui <- NULL
    latest_sim <- if (!is.null(simA)) simA else simB
    if (!is.null(latest_sim)) {
      seg <- sr_rv$selected_seg
      action_type <- if (identical(latest_sim$action, "Pricing")) "reprice" else "decline"
      reasoning_text <- tryCatch({
        seg_for_ai <- list(
          display_rule = seg$display_rule %||% seg$rule,
          bad_rate     = seg$bad_rate,
          mean_irr     = seg$mean_irr,
          n            = seg$n,
          lift         = seg$lift,
          avg_apr      = latest_sim$bl_seg_apr %||% seg$avg_apr
        )
        port_ctx <- list(
          portfolio_bad = sr_rv$portfolio_metrics$bad_rate,
          portfolio_irr = sr_rv$portfolio_metrics$irr
        )
        explain_action(action_type, seg_for_ai, marginal_data = NULL,
                       portfolio_context = port_ctx)
      }, error = function(e) {
        message("[Scanner] explain_action failed: ", e$message)
        NULL
      })
      if (!is.null(reasoning_text) && nzchar(reasoning_text)) {
        action_reasoning_ui <- tags$div(
          style = "background:#1a1f2e; border-left:3px solid #fb6e36; border-radius:6px; padding:10px 14px; margin-bottom:12px;",
          tags$span(style = "color:#fb6e36; font-weight:600;",
            icon("sparkles"), " Why this action?"),
          tags$p(style = "color:#d1d5db; font-size:13px; margin-top:6px;",
            reasoning_text)
        )
      }
    }

    # Scroll to results now that content is actually ready
    shinyjs::runjs("setTimeout(function(){
      var el = document.getElementById('sr_action_results');
      if(el) el.scrollIntoView({behavior:'smooth', block:'start'});
    }, 300);")

    div(style = "margin-top:20px;",
      # 0. Action reasoning (UC4)
      action_reasoning_ui,

      # 1. Sample preview
      uiOutput("sr_sample_cards"),

      # 2. Funnel + IRR side-by-side
      fluidRow(
        column(6, div(class = "card", style = "padding:15px; margin-bottom:15px;",
          h4("Funnel Metrics", style = "font-weight:bold; margin-bottom:8px;"),
          uiOutput("sr_funnel_subtitle"),
          uiOutput("sr_funnel_table")
        )),
        column(6, div(class = "card", style = "padding:15px; margin-bottom:15px;",
          uiOutput("sr_irr_title"),
          selectInput("sr_irr_level", NULL,
            choices = c("Overall", "FICO Band" = "fico_band",
                        "Partner Channel" = "partner_channel",
                        "Grade" = "risk_grade",
                        "Repeat Type" = "borrower_repeat_type"),
            selected = "partner_channel", width = "200px"),
          plotly::plotlyOutput("sr_irr_plot", height = "380px")
        ))
      ),

      # 7. Vintage curves (full-width for 1 scenario, side-by-side for 2)
      div(class = "card", style = "padding:15px; margin-bottom:15px;",
        h4("Vintage Performance Curves", style = "font-weight:bold;"),
        if (!is.null(simA) && !is.null(simB)) {
          fluidRow(
            column(6, plotly::plotlyOutput("sr_vintage_a", height = "420px")),
            column(6, plotly::plotlyOutput("sr_vintage_b", height = "420px"))
          )
        } else {
          fluidRow(
            column(12, plotly::plotlyOutput(
              if (!is.null(simA)) "sr_vintage_a" else "sr_vintage_b",
              height = "420px"))
          )
        }
      ),

      # 8. Save to repository
      div(class = "card", style = "padding:15px;",
        h4("Save Policy to Repository", style = "font-weight:bold;"),
        p("Save a simulated policy action to the repository for comparison.",
          style = "color:#9ca3af; margin-bottom:12px;"),
        fluidRow(
          column(12,
            textInput("sr_save_name", "Policy Name",
              placeholder = "e.g., High-Risk Segment Decline")),
          column(12,
            div(style = "display:flex; gap:10px; padding-top:10px;",
              div(style = "flex:1;",
                actionButton("sr_sql_baseline", "Baseline SQL",
                  icon = icon("database"), class = "btn-sm", width = "100%",
                  style = "background-color:#334155; color:white; border:none;")),
              div(style = "flex:1;",
                actionButton("sr_save_btn_A", "Save Primary",
                  icon = icon("save"), class = "btn-warning btn-sm", width = "100%")),
              div(style = "flex:1;",
                downloadButton("sr_download_policy_A", "SQL Primary",
                  class = "btn-sm",
                  style = "background-color:#10b981; color:white; width:100%;")),
              div(style = "flex:1;",
                actionButton("sr_save_btn_B", "Save Alternative",
                  icon = icon("save"), class = "btn-info btn-sm", width = "100%")),
              div(style = "flex:1;",
                downloadButton("sr_download_policy_B", "SQL Alternative",
                  class = "btn-sm",
                  style = "background-color:#0ea5e9; color:white; width:100%;"))
            ))
        )
      )
    )
  })

  # ── Funnel comparison table (uses sr_perf_data for analysis filters) ──
  output$sr_funnel_table <- renderUI({
    .ft_t0 <- Sys.time()
    on.exit(if (getOption("csm_debug", FALSE)) cat(sprintf("[%s] [Scanner] sr_funnel_table render: %.2fs\n",
                        format(Sys.time(), "%H:%M:%OS3"),
                        as.numeric(difftime(Sys.time(), .ft_t0, units = "secs"))),
                file = stderr()), add = TRUE)
    tryCatch({
    simA <- sr_rv$sim_A
    simB <- sr_rv$sim_B
    if (is.null(simA) && is.null(simB)) return(NULL)

    d <- sr_perf_data()
    fp  <- function(x) if (is.na(x)) "\u2014" else sprintf("%.1f%%", x * 100)
    fc  <- function(x) if (is.na(x)) "\u2014" else fmt_k(x)

    tape <- sr_tape()
    # Detect whether the analysis filters have subset the data. When no
    # filters are applied we can read cached IRR / projection values from
    # sr_compute_cache (keyed on the raw sim_* frames), eliminating duplicate
    # XIRR + projection work. With filters applied we fall back to direct
    # computation against the filtered subset.
    filters_active <- (!is.null(simA) && !identical(nrow(d$df_full), nrow(simA$df_full))) ||
                      (!is.null(simB) && !identical(nrow(d$df_full), nrow(simB$df_full)))

    compute_irr_pair <- function(bl_df, pr_df, sim_ref = NULL) {
      if (!is.null(tape)) {
        # Try cache first (unfiltered path). When filters are active the
        # cache doesn't apply, so we fall through to the direct compute.
        if (!filters_active && !is.null(sim_ref)) {
          entry <- sr_cache_for(sim_ref)
          if (!is.null(entry) && !is.na(entry$bl_xirr)) {
            return(list(bl = entry$bl_xirr, pr = entry$pr_xirr,
                        label = "Portfolio IRR"))
          }
        }
        bl_xirr <- tryCatch(
          compute_adjusted_xirr_v2(bl_df, tape, bl_df),
          error = function(e) {
            message("[Scanner] compute_irr_pair baseline XIRR failed: ", e$message)
            NA_real_
          }
        )
        pr_xirr <- tryCatch(
          compute_adjusted_xirr_v2(pr_df, tape, bl_df),
          error = function(e) {
            message("[Scanner] compute_irr_pair proposed XIRR failed: ", e$message)
            NA_real_
          }
        )
        list(bl = bl_xirr, pr = pr_xirr, label = "Portfolio IRR")
      } else {
        bl_f <- tryCatch(compute_funnel(bl_df), error = function(e) {
          message("[Scanner] compute_irr_pair baseline funnel failed: ", e$message)
          list(avg_apr = NA_real_)
        })
        pr_f <- tryCatch(compute_funnel(pr_df), error = function(e) {
          message("[Scanner] compute_irr_pair proposed funnel failed: ", e$message)
          list(avg_apr = NA_real_)
        })
        list(bl = bl_f$avg_apr, pr = pr_f$avg_apr, label = "Avg IRR (APR)")
      }
    }

    # Compute projected bad rates for funnel table (if tape available).
    # Prefer cached values when filters are not active; fall back to a
    # direct compute on the filtered subset otherwise.
    compute_funnel_proj <- function(df_src, sim_ref = NULL, side = c("bl", "pr")) {
      side <- match.arg(side)
      if (is.null(tape)) return(NA_real_)
      if (!filters_active && !is.null(sim_ref)) {
        entry <- sr_cache_for(sim_ref)
        if (!is.null(entry)) {
          val <- if (side == "bl") entry$bl_proj_bad_rate else entry$pr_proj_bad_rate
          if (!is.na(val)) return(val)
        }
      }
      tryCatch({
        ids <- as.character(df_src$loan_id[df_src$issued_flag == 1])
        if (length(ids) == 0) return(NA_real_)
        proj <- compute_projected_bad_rate(ids, tape, df_src)
        if (!is.null(proj) && nrow(proj) > 0)
          mean(proj$projected_lifetime_prob, na.rm = TRUE)
        else NA_real_
      }, error = function(e) {
        message("[Scanner] compute_funnel_proj failed: ", e$message)
        NA_real_
      })
    }

    dual <- !is.null(simA) && !is.null(simB)

    if (dual) {
      bl_A <- compute_funnel(d$df_full)
      pr_A <- compute_funnel(simA$df_proposed)
      pr_B <- compute_funnel(simB$df_proposed)
      irr_A <- compute_irr_pair(d$df_full, simA$df_proposed, sim_ref = simA)
      irr_B <- compute_irr_pair(d$df_full, simB$df_proposed, sim_ref = simB)

      rows <- list(
        c("Applications",  fc(bl_A$apps),      fc(pr_A$apps),      fc(pr_B$apps)),
        c("Approved",      fc(bl_A$approved),  fc(pr_A$approved),  fc(pr_B$approved)),
        c("Issued",        fc(bl_A$issued),    fc(pr_A$issued),    fc(pr_B$issued)),
        c("Approval Rate", fp(bl_A$appr_rate), fp(pr_A$appr_rate), fp(pr_B$appr_rate)),
        c("Take Rate",     fp(bl_A$conv_rate), fp(pr_A$conv_rate), fp(pr_B$conv_rate)),
        c("2+ Rate",      fp(bl_A$bad_rate),  fp(pr_A$bad_rate),  fp(pr_B$bad_rate))
      )
      # Add projected bad rate row if tape is available
      bl_proj <- compute_funnel_proj(d$df_full, sim_ref = simA, side = "bl")
      prA_proj <- compute_funnel_proj(simA$df_proposed, sim_ref = simA, side = "pr")
      prB_proj <- compute_funnel_proj(simB$df_proposed, sim_ref = simB, side = "pr")
      if (!is.na(bl_proj))
        rows <- c(rows, list(c("2+ Rate (Projected)", fp(bl_proj), fp(prA_proj), fp(prB_proj))))
      rows <- c(rows, list(
        c("Avg APR",       fp(bl_A$avg_apr),   fp(pr_A$avg_apr),   fp(pr_B$avg_apr)),
        c(irr_A$label,     fp(irr_A$bl),       fp(irr_A$pr),       fp(irr_B$pr))
      ))
      tbl <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
      names(tbl) <- c("Metric", "Baseline", "Primary", "Alternative")
    } else {
      bl <- compute_funnel(d$df_full)
      pr <- compute_funnel(d$df_proposed)
      sim <- if (!is.null(simA)) simA else simB
      irr <- compute_irr_pair(d$df_full, d$df_proposed, sim_ref = sim)
      slot_label <- sim$label %||% "Scenario"
      rows <- list(
        c("Applications",    fc(bl$apps),      fc(pr$apps)),
        c("Approved",        fc(bl$approved),  fc(pr$approved)),
        c("Issued",          fc(bl$issued),    fc(pr$issued)),
        c("Approval Rate",   fp(bl$appr_rate), fp(pr$appr_rate)),
        c("Take Rate",       fp(bl$conv_rate), fp(pr$conv_rate)),
        c("2+ Rate",        fp(bl$bad_rate),  fp(pr$bad_rate))
      )
      bl_proj <- compute_funnel_proj(d$df_full, sim_ref = sim, side = "bl")
      pr_proj <- compute_funnel_proj(d$df_proposed, sim_ref = sim, side = "pr")
      if (!is.na(bl_proj))
        rows <- c(rows, list(c("2+ Rate (Projected)", fp(bl_proj), fp(pr_proj))))
      rows <- c(rows, list(
        c("Avg APR",         fp(bl$avg_apr),   fp(pr$avg_apr)),
        c(irr$label,         fp(irr$bl),       fp(irr$pr))
      ))
      tbl <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
      names(tbl) <- c("Metric", "Baseline", slot_label)
    }

    header_cells <- lapply(names(tbl), function(col) {
      tags$th(
        style = "background:#fb6e36; color:white; padding:10px 12px; font-size:12px; text-align:left;",
        col)
    })
    body_rows <- lapply(seq_len(nrow(tbl)), function(i) {
      tags$tr(lapply(seq_along(tbl), function(j) {
        tags$td(
          style = paste0("padding:10px 12px; font-size:12px; border-top:1px solid #e5e7eb;",
                         if (j == 1) " font-weight:600; color:#0f172a;" else " color:#334155;"),
          tbl[i, j])
      }))
    })

    tags$div(style = "overflow-x:auto;",
      tags$table(
        style = "width:100%; border-collapse:collapse; table-layout:fixed;",
        tags$thead(tags$tr(header_cells)),
        tags$tbody(body_rows)
      )
    )
    }, error = function(e) {
      message("[sr_funnel_table] Error: ", conditionMessage(e))
      tags$div(style = "padding:12px; color:#9ca3af; font-size:12px;",
               "Funnel table temporarily unavailable.")
    })
  })

  # ── Scenario summary ──
  output$sr_scenario_summary <- renderUI({
    rows <- list()
    for (slot in c("A", "B")) {
      sim <- sr_slot_data(slot)
      if (is.null(sim)) next
      slot_label <- if (slot == "A") "Primary" else "Alternative"
      action_label <- if (identical(sim$action, "Pricing")) {
        paste0("Price", if (!is.null(sim$bps) && sim$bps > 0) paste0(" +", sim$bps, " bps") else "")
      } else "Decline"
      cond <- sim$segment_condition %||% ""
      if (!nzchar(cond)) cond <- "Segment not available"
      rows[[length(rows) + 1]] <- div(
        style = "display:flex; gap:16px; align-items:flex-start; padding:10px 14px; border-top:1px solid #e5e7eb;",
        tags$div(style = "min-width:88px; font-weight:700; color:#0f172a;", slot_label),
        tags$div(style = "min-width:120px; color:#334155;", action_label),
        tags$div(style = "min-width:110px; color:#334155;",
          paste0(format(sim$n_affected %||% 0, big.mark = ","), " affected")),
        tags$div(style = "flex:1; color:#64748b; font-size:12px;",
          paste(strwrap(cond, width = 70), collapse = " "))
      )
    }
    if (length(rows) == 0) return(NULL)
    div(class = "card", style = "margin-bottom:15px;",
      h4("Scenario Summary", style = "font-weight:bold; margin-bottom:8px;"),
      div(style = "background:#f8fafc; border-radius:8px; overflow:hidden;", rows)
    )
  })

  # ── SQL buttons ──
  output$sr_sql_buttons <- renderUI({
    simA <- sr_rv$sim_A
    simB <- sr_rv$sim_B
    if (is.null(simA) && is.null(simB)) return(NULL)
    btn_dark <- "background-color:#334155; color:white; border:none; padding:6px 14px; border-radius:4px; font-size:13px;"
    btn_a    <- "background-color:#f97316; color:white; border:none; padding:6px 14px; border-radius:4px; font-size:13px;"
    btn_b    <- "background-color:#17a2b8; color:white; border:none; padding:6px 14px; border-radius:4px; font-size:13px;"
    btns <- tagList(
      actionButton("sr_sql_baseline", "Baseline Policy (SQL)",
        icon = icon("database"), style = btn_dark),
      actionButton("sr_sql_scenario_a", paste0(simA$label %||% "Primary", " (SQL)"),
        icon = icon("code"), style = btn_a)
    )
    if (!is.null(simB)) {
      btns <- tagList(btns,
        actionButton("sr_sql_scenario_b", paste0(simB$label %||% "Alternative", " (SQL)"),
          icon = icon("code"), style = btn_b))
    }
    fluidRow(column(12, div(
      style = "padding:4px 15px 8px; display:flex; gap:10px; margin-bottom:15px;", btns)))
  })

  # ── SQL preview modals ──
  sr_show_sql_modal <- function(sql_code, title) {
    showModal(modalDialog(
      title = tagList(icon("database"), title),
      size = "l", easyClose = TRUE,
      tags$pre(style = "max-height:500px; overflow-y:auto; font-size:12px; background:#1e293b; color:#e2e8f0; padding:15px; border-radius:6px;",
        sql_code),
      footer = tagList(
        downloadButton("sr_sql_download", "Download SQL", class = "btn-sm",
          style = "background-color:#10b981; color:white;"),
        modalButton("Close"))
    ))
  }

  observeEvent(input$sr_sql_baseline, {
    sr_show_sql_modal("-- Baseline Policy: No changes applied\n-- Run default policy engine (L10-L90)",
      "Baseline Policy SQL")
  })
  observeEvent(input$sr_sql_scenario_a, {
    sim <- sr_rv$sim_A
    req(sim)
    cond <- sim$segment_condition %||% "segment"
    action_sql <- if (identical(sim$action, "Decline")) {
      paste0("-- Decline segment where: ", cond,
             "\nUPDATE decisions SET offered_decision_flag = 'DECL', issued_flag = 0\nWHERE ", cond, ";")
    } else {
      paste0("-- Reprice segment where: ", cond,
             "\n-- BPS adjustment: +", sim$bps %||% 0,
             "\nUPDATE pricing SET apr = apr + ", (sim$bps %||% 0) / 10000,
             "\nWHERE ", cond, ";")
    }
    sr_show_sql_modal(action_sql, paste0(sim$label %||% "Primary", " Action SQL"))
  })
  observeEvent(input$sr_sql_scenario_b, {
    sim <- sr_rv$sim_B
    req(sim)
    cond <- sim$segment_condition %||% "segment"
    action_sql <- if (identical(sim$action, "Decline")) {
      paste0("-- Decline segment where: ", cond,
             "\nUPDATE decisions SET offered_decision_flag = 'DECL', issued_flag = 0\nWHERE ", cond, ";")
    } else {
      paste0("-- Reprice segment where: ", cond,
             "\n-- BPS adjustment: +", sim$bps %||% 0,
             "\nUPDATE pricing SET apr = apr + ", (sim$bps %||% 0) / 10000,
             "\nWHERE ", cond, ";")
    }
    sr_show_sql_modal(action_sql, paste0(sim$label %||% "Alternative", " Action SQL"))
  })

  # ── Populate analysis filter choices when sim data changes ──
  observe({
    sim <- sr_rv$sim_A %||% sr_rv$sim_B
    req(sim)
    df <- sim$df_full
    fico_choices <- c("all", sort(unique(as.character(df$fico_band[!is.na(df$fico_band)]))))
    partner_choices <- c("all", sort(unique(as.character(df$partner_channel[!is.na(df$partner_channel)]))))
    repeat_choices <- c("all", sort(unique(as.character(df$borrower_repeat_type[!is.na(df$borrower_repeat_type)]))))
    mob_choices <- c("all", sort(unique(as.character(df$mob_bins[!is.na(df$mob_bins)]))))
    vintage_choices <- c("all", sort(unique(format(df$date[!is.na(df$date)], "%Y-%m"))))
    shinyWidgets::updatePickerInput(session, "sr_perf_fico", choices = fico_choices, selected = "all")
    shinyWidgets::updatePickerInput(session, "sr_perf_partner", choices = partner_choices, selected = "all")
    shinyWidgets::updatePickerInput(session, "sr_perf_repeat", choices = repeat_choices, selected = "all")
    shinyWidgets::updatePickerInput(session, "sr_perf_mob", choices = mob_choices, selected = "all")
    shinyWidgets::updatePickerInput(session, "sr_perf_vintage", choices = vintage_choices, selected = "all")
  })

  # ── Analysis-section filter reactive (reads sim data, applies 5 filters) ──
  sr_perf_data <- reactive({
    sim <- sr_rv$sim_A %||% sr_rv$sim_B
    req(sim)
    df_f <- sim$df_full
    df_p <- sim$df_proposed
    seg  <- sim$in_segment

    if (!("all" %in% input$sr_perf_fico) && length(input$sr_perf_fico) > 0) {
      keep <- df_f$fico_band %in% input$sr_perf_fico
      df_f <- df_f[keep, ]; df_p <- df_p[keep, ]; seg <- seg[keep]
    }
    if (!("all" %in% input$sr_perf_partner) && length(input$sr_perf_partner) > 0) {
      keep <- df_f$partner_channel %in% input$sr_perf_partner
      df_f <- df_f[keep, ]; df_p <- df_p[keep, ]; seg <- seg[keep]
    }
    if (!("all" %in% input$sr_perf_repeat) && length(input$sr_perf_repeat) > 0) {
      keep <- df_f$borrower_repeat_type %in% input$sr_perf_repeat
      df_f <- df_f[keep, ]; df_p <- df_p[keep, ]; seg <- seg[keep]
    }
    if (!("all" %in% input$sr_perf_mob) && length(input$sr_perf_mob) > 0) {
      keep <- df_f$mob_bins %in% input$sr_perf_mob
      df_f <- df_f[keep, ]; df_p <- df_p[keep, ]; seg <- seg[keep]
    }
    if (!("all" %in% input$sr_perf_vintage) && length(input$sr_perf_vintage) > 0) {
      v    <- format(df_f$date, "%Y-%m")
      keep <- !is.na(v) & v %in% input$sr_perf_vintage
      df_f <- df_f[keep, ]; df_p <- df_p[keep, ]; seg <- seg[keep]
    }
    list(df_full = df_f, df_proposed = df_p, in_segment = seg)
  })

  # ── Funnel subtitle (filter badge) ──
  output$sr_funnel_subtitle <- renderUI({
    filters <- c()
    if (!("all" %in% input$sr_perf_fico) && length(input$sr_perf_fico) > 0)
      filters <- c(filters, paste("FICO:", paste(input$sr_perf_fico, collapse = ", ")))
    if (!("all" %in% input$sr_perf_partner) && length(input$sr_perf_partner) > 0)
      filters <- c(filters, paste("Partner:", paste(input$sr_perf_partner, collapse = ", ")))
    if (!("all" %in% input$sr_perf_repeat) && length(input$sr_perf_repeat) > 0)
      filters <- c(filters, paste("Repeat:", paste(input$sr_perf_repeat, collapse = ", ")))
    if (!("all" %in% input$sr_perf_mob) && length(input$sr_perf_mob) > 0)
      filters <- c(filters, paste("MOB:", paste(input$sr_perf_mob, collapse = ", ")))
    if (!("all" %in% input$sr_perf_vintage) && length(input$sr_perf_vintage) > 0)
      filters <- c(filters, paste("Vintage:", paste(input$sr_perf_vintage, collapse = ", ")))
    if (length(filters) == 0) return(NULL)
    tags$div(style = "margin-bottom:8px;",
      tags$span(style = "background:#e5e7eb; color:#374151; padding:3px 8px; border-radius:10px; font-size:11px;",
        paste(filters, collapse = " | ")))
  })

  # ── Save to repository helper + observers ──
  # Helper: persist added policies and their closures to disk.
  # Runs on every save (autosave) AND on session end as a belt-and-suspenders
  # backstop. Previous behaviour deferred all disk writes to session$onEnded,
  # which lost saves if the session crashed, was force-closed, or timed out.
  sr_do_persist <- function(tag = "persist") {
    tryCatch({
      pol_df <- isolate(added_policy())
      if (is.null(pol_df) || nrow(pol_df) == 0) return(invisible())
      all_fns <- isolate(policy_fns_rv())
      saved_fns <- list()
      for (pid in pol_df$policyId) {
        if (!is.null(all_fns[[pid]])) {
          fn <- all_fns[[pid]]
          fn_env <- environment(fn)
          if (!is.null(fn_env) && !identical(fn_env, globalenv())) {
            clean_env <- new.env(parent = globalenv())
            for (nm in ls(fn_env, all.names = TRUE)) {
              assign(nm, get(nm, envir = fn_env), envir = clean_env)
            }
            environment(fn) <- clean_env
          }
          saved_fns[[pid]] <- fn
        }
      }
      tmp_path <- paste0(saved_policies_path, ".tmp")
      saveRDS(list(policies = pol_df, fns = saved_fns), tmp_path)
      fsize <- file.info(tmp_path)$size
      if (!is.na(fsize) && fsize > 10e6) {
        file.remove(tmp_path)
        message("[", tag, "] Skipped — file too large (", round(fsize / 1e6, 1), " MB)")
      } else {
        file.rename(tmp_path, saved_policies_path)
        message("[", tag, "] Saved ", nrow(pol_df), " policies to disk")
      }
    }, error = function(e) {
      message("[", tag, "] Failed: ", e$message)
    })
  }

  sr_persist_policies <- function() {
    sr_do_persist(tag = "persist/autosave")
  }

  # Belt-and-suspenders: also save on session end in case of any missed saves.
  session$onEnded(function() sr_do_persist(tag = "persist/end"))

  sr_save_scenario <- function(slot) {
    sim <- sr_rv[[paste0("sim_", slot)]]
    if (is.null(sim)) {
      showNotification(paste0("Scenario ", slot, " has no simulation. Run an action first."),
        type = "warning", duration = 3)
      return()
    }

    name <- trimws(input$sr_save_name)
    if (nchar(name) == 0) {
      showNotification("Please provide a policy name.", type = "warning", duration = 3)
      return()
    }

    tryCatch({
      # Capture parameters from sim slot
      saved_condition <- sim$segment_condition %||% "TRUE"
      saved_action    <- sim$action
      saved_bps       <- sim$bps %||% 0
      saved_elast     <- sim$elast %||% 5
      saved_tree      <- sim$saved_tree
      saved_tree_vars <- sim$saved_tree_vars
      saved_node      <- sim$saved_node

      # Strip bulky training data from rpart tree to avoid serializing large objects
      # Keep: frame, splits, csplit, where, xlevels (needed by assign_leaf_nodes)
      if (!is.null(saved_tree)) {
        saved_tree$call      <- NULL
        saved_tree$y         <- NULL
        saved_tree$functions <- NULL
        saved_tree$ordered   <- NULL
      }

      # Build policy function in a clean environment (avoids capturing server scope)
      policy_env <- new.env(parent = globalenv())
      policy_env$saved_condition  <- saved_condition
      policy_env$saved_action     <- saved_action
      policy_env$saved_bps        <- saved_bps
      policy_env$saved_elast      <- saved_elast
      policy_env$saved_tree       <- saved_tree
      policy_env$saved_tree_vars  <- saved_tree_vars
      policy_env$saved_node       <- saved_node

      policy_fn <- local(function(data) {
        result <- data

        # Identify segment using saved CART tree
        seg_mask <- if (!is.null(saved_tree) && !is.null(saved_node)) {
          pred_nodes <- assign_leaf_nodes(saved_tree, data, saved_tree_vars)
          !is.na(pred_nodes) & pred_nodes == saved_node
        } else {
          m <- tryCatch(
            with(data, eval(parse(text = saved_condition))),
            error = function(e) {
              message("[Scanner] saved_policy eval failed, returning FALSE mask: ",
                      e$message)
              rep(FALSE, nrow(data))
            }
          )
          m[is.na(m)] <- FALSE
          m
        }
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
          result <- replay_layers_50_80_90(result)
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

      slot_label <- if (slot == "A") "Primary" else "Alternative"
      new_policy <- data.frame(
        id                  = new_id,
        policyId            = pid,
        policyName          = paste0(name, " (", slot_label, ")"),
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

      # Update reactives immediately (instant, main thread)
      existing <- added_policy()
      if (!is.null(existing)) {
        added_policy(bind_rows(existing, new_policy))
      } else {
        added_policy(new_policy)
      }

      fns <- policy_fns_rv()
      fns[[pid]] <- policy_fn
      policy_fns_rv(fns)

      # Persist to disk in background (non-blocking)
      sr_persist_policies()

      showNotification(
        HTML(paste0("<b>", slot_label, "</b> saved as <b>'", htmltools::htmlEscape(name),
                    "'</b> (", pid, ") to Policy Repository")),
        type = "message", duration = 4)
      updateTextInput(session, "sr_save_name", value = "")

    }, error = function(e) {
      message("[sr_save] Error saving policy: ", e$message)
      showNotification(
        paste0("Failed to save policy: ", e$message),
        type = "error", duration = 6)
    })
  }

  observeEvent(input$sr_save_btn_A, { sr_save_scenario("A") })
  observeEvent(input$sr_save_btn_B, { sr_save_scenario("B") })

  # ── SQL download handlers ──
  sr_build_sql_download <- function(slot) {
    sim <- sr_rv[[paste0("sim_", slot)]]
    if (is.null(sim)) stop(paste0("Scenario ", slot, " has not been run yet."))
    bp  <- build_default_params(policy_manifest)
    # Scanner is hardcoded to PL-CURRENT — merge its overrides so the
    # emitted SQL reflects the production policy thresholds.
    if (exists("strategy_overrides") &&
        !is.null(strategy_overrides[["PL-CURRENT"]])) {
      pl_current_overrides <- strategy_overrides[["PL-CURRENT"]]$params
      if (!is.null(pl_current_overrides) && length(pl_current_overrides) > 0) {
        bp <- modifyList(bp, pl_current_overrides)
      }
    }
    act <- sim$action %||% "Decline"
    lbl <- sim$label %||% if (slot == "A") "Primary Action" else "Alternative Action"
    name <- trimws(input$sr_save_name %||% "")
    if (nzchar(name)) lbl <- name
    generate_policy_sql(bp,
      label        = paste0(lbl, " — ", act,
                      if (act == "Pricing" && (sim$bps %||% 0) > 0)
                        paste0(" +", sim$bps, "bps") else ""),
      segment_cond = sim$segment_condition,
      action       = act,
      bps          = sim$bps %||% 0)
  }

  output$sr_download_policy_A <- downloadHandler(
    filename = function() {
      lbl <- sr_rv$sim_A$label %||% "primary"
      paste0("scanner_", tolower(gsub("\\s+", "_", lbl)), "_policy_", format(Sys.Date(), "%Y%m%d"), ".sql")
    },
    content = function(file) writeLines(sr_build_sql_download("A"), file)
  )
  output$sr_download_policy_B <- downloadHandler(
    filename = function() {
      lbl <- sr_rv$sim_B$label %||% "alternative"
      paste0("scanner_", tolower(gsub("\\s+", "_", lbl)), "_policy_", format(Sys.Date(), "%Y%m%d"), ".sql")
    },
    content = function(file) writeLines(sr_build_sql_download("B"), file)
  )

  # ── Sample preview cards ──
  output$sr_sample_cards <- renderUI({
    cards <- list()
    for (slot in c("A", "B")) {
      sim <- sr_slot_data(slot)
      if (is.null(sim) || is.null(sim$sample_preview)) next
      label <- sim$label %||% slot
      verb <- ifelse(identical(sim$action, "Decline"), "declined", "repriced")
      title_text <- paste0(label, " — ", sim$n_affected %||% 0, " loans ", verb)

      cards[[length(cards) + 1]] <- column(
        if (!is.null(sr_slot_data("A")$sample_preview) &&
            !is.null(sr_slot_data("B")$sample_preview)) 6 else 12,
        div(class = "card", style = "padding:10px 15px;",
          fluidRow(
            column(8,
              h5(icon("table"), title_text,
                style = "font-weight:bold; margin:0; display:inline-block;")
            ),
            column(4, style = "text-align:right;",
              actionLink(paste0("sr_sample_expand_", slot),
                label = tagList(icon("expand"), " View affected loans"),
                style = "color:#fb6e36; font-size:12px;")
            )
          )
        )
      )
    }
    if (length(cards) == 0) return(NULL)
    div(style = "margin-bottom:15px;", do.call(fluidRow, cards))
  })

  # ── Sample modal + download ──
  sr_show_sample_modal <- function(slot) {
    sim <- sr_slot_data(slot)
    req(sim$sample_full)
    verb <- ifelse(identical(sim$action, "Decline"), "declined", "repriced")
    display_df <- if (!is.null(sim$sample_all)) sim$sample_all else sim$sample_full
    output[[paste0("sr_modal_table_", slot)]] <- DT::renderDataTable({
      DT::datatable(display_df, selection = "none", rownames = FALSE,
        options = list(pageLength = 25, dom = "tip", scrollX = TRUE,
                       columnDefs = list(list(className = "dt-center", targets = "_all"))))
    })
    sr_rv[[paste0("modal_download_", slot)]] <- display_df
    showModal(modalDialog(
      title = h4(icon("table"), paste0(
        sim$label %||% slot, " — ", sim$n_affected %||% 0, " total (", verb, ")")),
      size = "l", easyClose = TRUE,
      footer = tagList(
        downloadButton(paste0("sr_download_affected_", slot), "Download CSV",
          class = "btn-sm",
          style = "background-color:#10b981; color:white; border:none; margin-right:8px;"),
        modalButton("Close")
      ),
      DT::DTOutput(paste0("sr_modal_table_", slot))
    ))
  }

  observeEvent(input$sr_sample_expand_A, { sr_show_sample_modal("A") })
  observeEvent(input$sr_sample_expand_B, { sr_show_sample_modal("B") })

  output$sr_download_affected_A <- downloadHandler(
    filename = function() {
      sim <- sr_slot_data("A")
      verb <- if (identical(sim$action, "Decline")) "declined" else "repriced"
      paste0("scanner_primary_", verb, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      df <- sr_rv$modal_download_A
      if (!is.null(df)) write.csv(df, file, row.names = FALSE)
    }
  )
  output$sr_download_affected_B <- downloadHandler(
    filename = function() {
      sim <- sr_slot_data("B")
      verb <- if (identical(sim$action, "Decline")) "declined" else "repriced"
      paste0("scanner_alternative_", verb, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      df <- sr_rv$modal_download_B
      if (!is.null(df)) write.csv(df, file, row.names = FALSE)
    }
  )

  # ── IRR title ──
  output$sr_irr_title <- renderUI({
    tape <- sr_tape()
    if (!is.null(tape)) {
      h4("Portfolio IRR (Cashflow-based)", style = "font-weight:bold; color:white;")
    } else {
      h4("Average IRR (APR proxy)", style = "font-weight:bold; color:white;")
    }
  })

  # ── IRR bar chart ──
  output$sr_irr_plot <- plotly::renderPlotly({
    .ip_t0 <- Sys.time()
    on.exit(if (getOption("csm_debug", FALSE)) cat(sprintf("[%s] [Scanner] sr_irr_plot render: %.2fs\n",
                        format(Sys.time(), "%H:%M:%OS3"),
                        as.numeric(difftime(Sys.time(), .ip_t0, units = "secs"))),
                file = stderr()), add = TRUE)
    tryCatch({
    simA <- sr_rv$sim_A
    simB <- sr_rv$sim_B
    if (is.null(simA) && is.null(simB)) {
      return(plotly::plot_ly() %>%
               plotly::layout(title = list(text = "Run an action to see IRR", font = list(size = 13)),
                              paper_bgcolor = "white", plot_bgcolor = "white"))
    }

    level <- input$sr_irr_level %||% "partner_channel"
    tape  <- sr_tape()
    tape_index <- sr_tape_index()
    dual  <- !is.null(simA) && !is.null(simB)

    level_label <- switch(level,
      "Overall"              = "Overall",
      "fico_band"            = "FICO Band",
      "partner_channel"      = "Partner Channel",
      "risk_grade"           = "Grade",
      "borrower_repeat_type" = "Repeat Type",
      level)

    wrap_axis_label <- function(x, width = 14) {
      vapply(as.character(x), function(val) {
        if (is.na(val) || !nzchar(val)) return(val)
        paste(strwrap(val, width = width), collapse = "<br>")
      }, character(1))
    }

    if (!is.null(tape)) {
      # ---- Real XIRR from tape cashflows ----
      sim_ref <- if (!is.null(simA)) simA else simB
      bl_ids   <- as.character(sim_ref$df_full$loan_id[sim_ref$df_full$issued_flag == 1])

      # For very large tapes (>200K rows), sample the loan universe down to 50K
      # IDs once per render. XIRR plot is a visualization, not a regulatory
      # number — sampling keeps render times bounded on the 100K dev dataset.
      .sample_large_ids <- function(ids) {
        if (!is.null(tape$rows) && nrow(tape$rows) > 200000L &&
            length(ids) > 50000L) {
          set.seed(42)
          ids <- sample(ids, 50000L)
        }
        ids
      }

      # Scanner IRR delegates to canonical engine functions so we never
      # maintain duplicate XIRR logic in the tab. Baseline calls reduce to
      # compute_portfolio_xirr_v2(); scenario calls use
      # compute_adjusted_xirr_v2() which handles APR-delta re-pricing of
      # both observed and projected cashflows. Grouping (by FICO band /
      # partner / etc.) is implemented by repeatedly calling the canonical
      # scalar function on each group's loan-id subset.
      .sub_tape <- function(ids_subset) {
        ids_subset <- as.character(ids_subset)
        rows_sub <- if (!is.null(tape_index)) {
          matched <- tape_index[ids_subset]
          row_idx <- unlist(matched, use.names = FALSE)
          if (length(row_idx) == 0) tape$rows[0, , drop = FALSE]
          else tape$rows[row_idx, , drop = FALSE]
        } else {
          tape$rows[tape$rows$loan_id %in% ids_subset, , drop = FALSE]
        }
        proj_sub <- if (!is.null(tape$projected)) {
          tape$projected[tape$projected$loan_id %in% ids_subset, , drop = FALSE]
        } else NULL
        list(
          rows        = rows_sub,
          projected   = proj_sub,
          loan_view   = tape$loan_view,
          vintages    = tape$vintages,
          cohort_loss = tape$cohort_loss
        )
      }

      .scalar_xirr <- function(ids_subset, scenario_label, sim_data) {
        if (length(ids_subset) == 0) return(NA_real_)
        sub_tape <- .sub_tape(ids_subset)
        if (is.null(sub_tape$rows) || nrow(sub_tape$rows) == 0) return(NA_real_)
        if (identical(scenario_label, "Baseline") || is.null(sim_data)) {
          tryCatch(compute_portfolio_xirr_v2(sub_tape),
                   error = function(e) {
                     message("[Scanner] compute_real_irr baseline XIRR failed: ", e$message)
                     NA_real_
                   })
        } else {
          df_bl  <- sim_data$df_full
          df_sim <- sim_data$df_proposed
          df_sim_sub <- df_sim[as.character(df_sim$loan_id) %in% ids_subset &
                               df_sim$issued_flag == 1, , drop = FALSE]
          if (nrow(df_sim_sub) == 0) return(NA_real_)
          tryCatch(compute_adjusted_xirr_v2(df_sim_sub, sub_tape, df_bl),
                   error = function(e) {
                     message("[Scanner] compute_real_irr adjusted XIRR failed: ", e$message)
                     NA_real_
                   })
        }
      }

      compute_real_irr <- function(ids, scenario_label, sim_data = NULL) {
        ids <- .sample_large_ids(unique(as.character(ids)))
        if (length(ids) == 0) {
          return(data.frame(Group = character(), irr_val = numeric(),
                            n_loans = integer(), Scenario = character(),
                            stringsAsFactors = FALSE))
        }

        # For group-level breakdowns, pull per-loan group assignment from
        # the scenario's simulated df_proposed (or the reference sim for
        # baseline). The tape has these columns too, but sourcing from the
        # sim frame keeps group assignment consistent with the KPI cards.
        seg_src <- if (!is.null(sim_data)) sim_data else sim_ref
        df_src  <- seg_src$df_proposed
        if (is.null(df_src)) df_src <- seg_src$df_full

        if (level == "Overall") {
          irr_val <- .scalar_xirr(ids, scenario_label, sim_data)
          return(data.frame(Group = "Overall", irr_val = irr_val,
                            n_loans = length(ids),
                            Scenario = scenario_label,
                            stringsAsFactors = FALSE))
        }

        if (!level %in% names(df_src)) {
          return(data.frame(Group = character(), irr_val = numeric(),
                            n_loans = integer(), Scenario = character(),
                            stringsAsFactors = FALSE))
        }

        df_grp <- df_src[as.character(df_src$loan_id) %in% ids, , drop = FALSE]
        df_grp <- df_grp[!is.na(df_grp[[level]]), , drop = FALSE]
        if (nrow(df_grp) == 0) {
          return(data.frame(Group = character(), irr_val = numeric(),
                            n_loans = integer(), Scenario = character(),
                            stringsAsFactors = FALSE))
        }

        grp_vals <- unique(df_grp[[level]])
        out <- lapply(grp_vals, function(g) {
          grp_ids <- as.character(df_grp$loan_id[df_grp[[level]] == g])
          if (length(grp_ids) == 0) return(NULL)
          data.frame(
            Group    = g,
            irr_val  = .scalar_xirr(grp_ids, scenario_label, sim_data),
            n_loans  = length(grp_ids),
            Scenario = scenario_label,
            stringsAsFactors = FALSE
          )
        })
        do.call(rbind, Filter(Negate(is.null), out))
      }

      irr_bl <- compute_real_irr(bl_ids, "Baseline")
      if (dual) {
        pr_A_ids <- as.character(simA$df_proposed$loan_id[simA$df_proposed$issued_flag == 1])
        pr_B_ids <- as.character(simB$df_proposed$loan_id[simB$df_proposed$issued_flag == 1])
        label_a <- simA$label %||% "Primary"
        label_b <- simB$label %||% "Alternative"
        irr_A <- compute_real_irr(pr_A_ids, label_a, sim_data = simA)
        irr_B <- compute_real_irr(pr_B_ids, label_b, sim_data = simB)
        both  <- dplyr::bind_rows(irr_bl, irr_A, irr_B) %>% dplyr::filter(!is.na(irr_val))
        bar_colors <- setNames(c("#6c757d", "#fb6e36", "#17a2b8"),
                               c("Baseline", label_a, label_b))
      } else {
        pr_ids <- as.character(sim_ref$df_proposed$loan_id[sim_ref$df_proposed$issued_flag == 1])
        irr_pr <- compute_real_irr(pr_ids, "Simulated", sim_data = sim_ref)
        both   <- dplyr::bind_rows(irr_bl, irr_pr) %>% dplyr::filter(!is.na(irr_val))
        bar_colors <- c("Baseline" = "#6c757d", "Simulated" = "#fb6e36")
      }

      if (nrow(both) == 0) {
        return(plotly::plot_ly() %>%
                 plotly::layout(title = list(text = "IRR could not be computed"),
                                paper_bgcolor = "white", plot_bgcolor = "white"))
      }

      both$GroupDisplay <- if (identical(level, "partner_channel")) {
        wrap_axis_label(both$Group, width = 12)
      } else both$Group

      plotly::plot_ly(both, x = ~GroupDisplay, y = ~irr_val, color = ~Scenario,
              colors = bar_colors, type = "bar",
              hovertext = ~paste0("<b>", Group, "</b><br>IRR: ",
                                  scales::percent(irr_val, accuracy = 0.1),
                                  "<br>N = ", fmt_k(n_loans)),
              hoverinfo = "text+name") %>%
        plotly::layout(
          barmode = "group",
          title   = list(text = paste("Portfolio IRR by", level_label), font = list(size = 13)),
          xaxis   = list(title = level_label, tickangle = -25, automargin = TRUE),
          yaxis   = list(title = "IRR (annualised)", tickformat = ".1%"),
          paper_bgcolor = "white", plot_bgcolor = "white",
          legend  = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.32)
        )

    } else {
      # ---- Fallback: average APR when no tape ----
      compute_irr_fallback <- function(df_src, scenario_label) {
        issued <- df_src %>% dplyr::filter(issued_flag == 1, !is.na(interest_rate_apr))
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
            dplyr::filter(!is.na(.data[[level]])) %>%
            dplyr::group_by(Group = .data[[level]]) %>%
            dplyr::summarise(irr_val = mean(interest_rate_apr, na.rm = TRUE),
                             n_loans = dplyr::n(), .groups = "drop") %>%
            dplyr::mutate(Scenario = scenario_label)
        }
      }

      sim_ref <- if (!is.null(simA)) simA else simB
      irr_bl <- compute_irr_fallback(sim_ref$df_full, "Baseline")
      if (dual) {
        label_a <- simA$label %||% "Primary"
        label_b <- simB$label %||% "Alternative"
        irr_A <- compute_irr_fallback(simA$df_proposed, label_a)
        irr_B <- compute_irr_fallback(simB$df_proposed, label_b)
        both  <- dplyr::bind_rows(irr_bl, irr_A, irr_B)
        bar_colors <- setNames(c("#6c757d", "#fb6e36", "#17a2b8"),
                               c("Baseline", label_a, label_b))
      } else {
        irr_pr <- compute_irr_fallback(sim_ref$df_proposed, "Simulated")
        both   <- dplyr::bind_rows(irr_bl, irr_pr)
        bar_colors <- c("Baseline" = "#6c757d", "Simulated" = "#fb6e36")
      }

      if (nrow(both) == 0) {
        return(plotly::plot_ly() %>%
                 plotly::layout(title = list(text = "No IRR data available"),
                                paper_bgcolor = "white", plot_bgcolor = "white"))
      }

      both$GroupDisplay <- if (identical(level, "partner_channel")) {
        wrap_axis_label(both$Group, width = 12)
      } else both$Group

      plotly::plot_ly(both, x = ~GroupDisplay, y = ~irr_val, color = ~Scenario,
              colors = bar_colors, type = "bar",
              hovertext = ~paste0("<b>", Group, "</b><br>Avg APR: ",
                                  scales::percent(irr_val, accuracy = 0.1),
                                  "<br>N = ", fmt_k(n_loans)),
              hoverinfo = "text+name") %>%
        plotly::layout(
          barmode = "group",
          title   = list(text = paste("Avg APR by", level_label, "(no tape)"),
                         font = list(size = 13)),
          xaxis   = list(title = level_label, tickangle = -25, automargin = TRUE),
          yaxis   = list(title = "Avg APR", tickformat = ".1%"),
          paper_bgcolor = "white", plot_bgcolor = "white",
          legend  = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.32)
        )
    }
    }, error = function(e) {
      message("[sr_irr_plot] Error: ", conditionMessage(e))
      plotly::plot_ly() %>%
        plotly::layout(title = list(text = "IRR chart temporarily unavailable",
                                    font = list(size = 13)),
                       paper_bgcolor = "white", plot_bgcolor = "white")
    })
  })

  # ── Vintage curve helpers (local to scanner) ──
  .sr_build_vintage <- function(tape, ids, scenario_label, adverse_ids = character(0)) {
    sel_vqs <- tape$vintages
    if (is.null(sel_vqs) || length(sel_vqs) == 0) return(data.frame())
    # Use last 4 vintage quarters
    sel_vqs <- tail(sel_vqs, min(4, length(sel_vqs)))

    rows_sub <- tape$rows[tape$rows$loan_id %in% ids &
                          tape$rows$vintage_quarter %in% sel_vqs, ]
    if (nrow(rows_sub) == 0) return(data.frame())

    min_dq_mob <- 4L
    cohort_size <- rows_sub %>%
      dplyr::filter(month_on_books == 0) %>%
      dplyr::group_by(vintage_quarter) %>%
      dplyr::summarise(n_loans = dplyr::n_distinct(loan_id), .groups = "drop")
    first_dq <- rows_sub %>%
      dplyr::filter(co_amount > 0) %>%
      dplyr::group_by(loan_id, vintage_quarter) %>%
      dplyr::summarise(first_dq_mob = min(month_on_books), .groups = "drop")
    flagged_no_co <- rows_sub %>%
      dplyr::filter(performance_delinquency_flag == 1) %>%
      dplyr::distinct(loan_id, vintage_quarter) %>%
      dplyr::anti_join(first_dq, by = "loan_id") %>%
      dplyr::mutate(first_dq_mob = min_dq_mob)
    first_dq <- dplyr::bind_rows(first_dq, flagged_no_co) %>%
      dplyr::mutate(first_dq_mob = pmax(first_dq_mob, min_dq_mob))

    if (length(adverse_ids) > 0) {
      syn_mob <- if (nrow(first_dq) > 0) median(first_dq$first_dq_mob) else min_dq_mob
      adv_in_tape <- rows_sub %>%
        dplyr::filter(loan_id %in% adverse_ids) %>%
        dplyr::distinct(loan_id, vintage_quarter) %>%
        dplyr::anti_join(first_dq, by = "loan_id")
      if (nrow(adv_in_tape) > 0) {
        adv_dq <- adv_in_tape %>% dplyr::mutate(first_dq_mob = as.integer(round(syn_mob)))
        first_dq <- dplyr::bind_rows(first_dq, adv_dq)
      }
    }
    max_mob_v <- rows_sub %>%
      dplyr::group_by(vintage_quarter) %>%
      dplyr::summarise(max_mob = max(month_on_books), .groups = "drop")
    grid <- max_mob_v %>%
      rowwise() %>%
      do({ data.frame(vintage_quarter = .$vintage_quarter,
                      month_on_books = 0:.$max_mob, stringsAsFactors = FALSE) }) %>%
      ungroup() %>%
      dplyr::left_join(cohort_size, by = "vintage_quarter")
    dq_counts <- first_dq %>%
      dplyr::group_by(vintage_quarter) %>%
      dplyr::arrange(first_dq_mob) %>%
      dplyr::mutate(cum_bad = dplyr::row_number()) %>%
      dplyr::group_by(vintage_quarter, first_dq_mob) %>%
      dplyr::summarise(cum_bad = max(cum_bad), .groups = "drop")
    grid %>%
      dplyr::left_join(dq_counts %>% dplyr::rename(month_on_books = first_dq_mob),
                by = c("vintage_quarter", "month_on_books")) %>%
      dplyr::arrange(vintage_quarter, month_on_books) %>%
      dplyr::group_by(vintage_quarter) %>%
      tidyr::fill(cum_bad, .direction = "down") %>%
      dplyr::mutate(cum_bad = ifelse(is.na(cum_bad), 0, cum_bad),
             bad_pct = cum_bad / n_loans,
             Scenario = scenario_label) %>%
      dplyr::ungroup()
  }

  .sr_plot_vintage <- function(bl_data, sc_data, chart_title) {
    both <- dplyr::bind_rows(bl_data, sc_data)
    if (nrow(both) == 0) {
      return(plotly::plot_ly() %>%
               plotly::layout(title = list(text = "No vintage data available",
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

    p <- plotly::plot_ly()
    shown_legend <- character(0)
    for (vq in vintages_in_data) {
      for (sc in unique(both$Scenario)) {
        sub <- both %>% dplyr::filter(vintage_quarter == vq, Scenario == sc)
        if (nrow(sub) == 0) next
        show_legend <- !(vq %in% shown_legend)
        if (show_legend) shown_legend <- c(shown_legend, vq)
        p <- p %>% plotly::add_trace(
          data = sub, x = ~month_on_books, y = ~bad_pct,
          type = "scatter", mode = "lines+markers",
          name = vq, showlegend = show_legend,
          line = list(color = pal[[vq]],
                      dash = if (sc == "Baseline") "solid" else "dash",
                      width = 2, shape = "linear"),
          marker = list(color = pal[[vq]], size = 3),
          legendgroup = vq,
          hovertemplate = paste0(
            "<b>", vq, " (", sc, ")</b><br>",
            "MOB: %{x}<br>Delinquency: %{y:.1%}<br>",
            "Loans: ", fmt_k(sub$n_loans[1]), "<extra></extra>")
        )
      }
    }
    p %>% plotly::layout(
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

  # ── Vintage chart A ──
  output$sr_vintage_a <- plotly::renderPlotly({
    .va_t0 <- Sys.time()
    on.exit(if (getOption("csm_debug", FALSE)) cat(sprintf("[%s] [Scanner] sr_vintage_a render: %.2fs\n",
                        format(Sys.time(), "%H:%M:%OS3"),
                        as.numeric(difftime(Sys.time(), .va_t0, units = "secs"))),
                file = stderr()), add = TRUE)
    tryCatch({
    tape <- sr_tape()
    if (is.null(tape)) {
      return(plotly::plot_ly() %>%
               plotly::layout(title = list(text = "No performance tape loaded",
                                           font = list(size = 13)),
                              paper_bgcolor = "white", plot_bgcolor = "white"))
    }
    simA <- sr_rv$sim_A
    simB <- sr_rv$sim_B
    sim_ref <- if (!is.null(simA)) simA else simB
    req(sim_ref)

    bl_ids <- as.character(sim_ref$df_full$loan_id[sim_ref$df_full$issued_flag == 1])
    pr_ids <- as.character(sim_ref$df_proposed$loan_id[sim_ref$df_proposed$issued_flag == 1])
    adv_ids <- sim_ref$adverse_flip_ids %||% character(0)
    sc_label <- sim_ref$label %||% "Simulated"

    bl_v <- .sr_build_vintage(tape, bl_ids, "Baseline")
    pr_v <- .sr_build_vintage(tape, pr_ids, sc_label, adverse_ids = adv_ids)
    .sr_plot_vintage(bl_v, pr_v, sc_label)
    }, error = function(e) {
      message("[sr_vintage_a] Error: ", conditionMessage(e))
      plotly::plot_ly() %>%
        plotly::layout(title = list(text = "Vintage chart temporarily unavailable",
                                    font = list(size = 13)),
                       paper_bgcolor = "white", plot_bgcolor = "white")
    })
  })

  # ── Vintage chart B ──
  output$sr_vintage_b <- plotly::renderPlotly({
    tryCatch({
    tape <- sr_tape()
    simA <- sr_rv$sim_A
    simB <- sr_rv$sim_B

    if (is.null(tape) || is.null(simA) || is.null(simB)) {
      msg <- if (is.null(tape)) "No performance tape loaded"
             else "Run a second scenario to compare"
      return(plotly::plot_ly() %>%
               plotly::layout(title = list(text = msg, font = list(size = 13, color = "#9ca3af")),
                              paper_bgcolor = "white", plot_bgcolor = "white",
                              xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)))
    }

    bl_ids <- as.character(simA$df_full$loan_id[simA$df_full$issued_flag == 1])
    pr_ids <- as.character(simB$df_proposed$loan_id[simB$df_proposed$issued_flag == 1])
    adv_ids <- simB$adverse_flip_ids %||% character(0)
    label_b <- simB$label %||% "Alternative"

    bl_v <- .sr_build_vintage(tape, bl_ids, "Baseline")
    pr_v <- .sr_build_vintage(tape, pr_ids, label_b, adverse_ids = adv_ids)
    .sr_plot_vintage(bl_v, pr_v, label_b)
    }, error = function(e) {
      message("[sr_vintage_b] Error: ", conditionMessage(e))
      plotly::plot_ly() %>%
        plotly::layout(title = list(text = "Vintage chart temporarily unavailable",
                                    font = list(size = 13)),
                       paper_bgcolor = "white", plot_bgcolor = "white")
    })
  })

  # ── Render priority overrides ──
  # KPIs / action results render first; heavy plots are deprioritised so the
  # user sees numbers immediately after clicking Decline. Without this the
  # Shiny scheduler may spend seconds on plots before updating KPIs.
  tryCatch({
    outputOptions(output, "sr_action_results", priority = 10)
    outputOptions(output, "sr_irr_plot", priority = -1)
    outputOptions(output, "sr_vintage_a", priority = -1)
    outputOptions(output, "sr_vintage_b", priority = -1)
  }, error = function(e) {
    message("[smart_recommender_server] outputOptions priority failed: ",
            conditionMessage(e))
  })

  # ---- NL Q&A current-view registration ----
  # The Scanner's selection model is button-based (sr_explore_i buttons set
  # sr_rv$selected_seg) rather than DT row selection.  When the user has
  # drilled into a segment, return only that segment's loans; otherwise fall
  # through to the full loan_data so the NL bar still answers sensibly.
  sr_nl_current_view <- reactive({
    seg <- sr_rv$selected_seg
    if (is.null(seg)) return(loan_data)
    ids <- seg$loan_ids
    if (is.null(ids) || length(ids) == 0) return(loan_data)
    loan_data[loan_data$loan_id %in% ids, , drop = FALSE]
  })

  nl_register_current_view(session, "smart_rec", sr_nl_current_view)
}
