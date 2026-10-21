# ============================================================
# Tab: Data Upload
# Supports CSV, Excel, S3, and SQL/EDH data sources
# Three data types: Application, Performance, Issued
# ============================================================

# Reusable source selector + upload panel for each data type
du_source_panel <- function(prefix, label, description, demo_btn = FALSE) {
  ns <- function(id) paste0(prefix, "_", id)
  div(class = "card", style = "padding:15px; margin-bottom:12px;",
    # Header row
    div(style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:10px;",
      div(
        h4(label, style = "font-weight:bold; margin:0;"),
        p(description, style = "color:#9ca3af; font-size:12px; margin:2px 0 0 0;")
      ),
      uiOutput(ns("status_badge"))
    ),
    # Source pills
    div(style = "display:flex; gap:6px; margin-bottom:12px; flex-wrap:wrap;",
      actionButton(ns("src_csv"),   "CSV",       icon = icon("file-csv"),   class = "btn-sm",
        style = "background-color:#fb6e36; color:white; border:none; border-radius:20px; padding:5px 14px; font-size:12px;"),
      actionButton(ns("src_excel"), "Excel",     icon = icon("file-excel"), class = "btn-sm",
        style = "background-color:transparent; color:#fb6e36; border:1px solid #fb6e36; border-radius:20px; padding:5px 14px; font-size:12px;"),
      actionButton(ns("src_s3"),    "S3 Bucket", icon = icon("cloud"),      class = "btn-sm",
        style = "background-color:transparent; color:#fb6e36; border:1px solid #fb6e36; border-radius:20px; padding:5px 14px; font-size:12px;"),
      actionButton(ns("src_sql"),   "SQL / EDH", icon = icon("database"),   class = "btn-sm",
        style = "background-color:transparent; color:#fb6e36; border:1px solid #fb6e36; border-radius:20px; padding:5px 14px; font-size:12px;")
    ),
    # CSV panel
    div(id = ns("panel_csv"),
      fileInput(ns("file_csv"), NULL, accept = ".csv", width = "100%"),
      if (demo_btn) fluidRow(
        column(6, actionButton(ns("demo"), "Load Demo", class = "custom-button btn-sm", width = "100%", icon = icon("database"))),
        column(6, actionButton(ns("clear"), "Clear", class = "btn btn-default btn-sm", width = "100%", icon = icon("times")))
      ) else fluidRow(
        column(12, actionButton(ns("clear"), "Clear", class = "btn btn-default btn-sm", width = "100%", icon = icon("times")))
      )
    ),
    # Excel panel
    hidden(div(id = ns("panel_excel"),
      fileInput(ns("file_excel"), NULL, accept = c(".xlsx", ".xls"), width = "100%"),
      fluidRow(
        column(6, selectInput(ns("excel_sheet"), "Sheet", choices = c("(upload first)"), width = "100%")),
        column(3, actionButton(ns("excel_load"), "Load", class = "custom-button btn-sm", width = "100%", icon = icon("file-import"))),
        column(3, actionButton(ns("clear_xl"), "Clear", class = "btn btn-default btn-sm", width = "100%", icon = icon("times")))
      )
    )),
    # S3 panel
    hidden(div(id = ns("panel_s3"),
      fluidRow(
        column(6, textInput(ns("s3_bucket"), NULL, placeholder = "s3://edh-prod-data-lake/...", width = "100%")),
        column(4, textInput(ns("s3_key"), NULL, placeholder = "path/to/file.csv", width = "100%")),
        column(2, actionButton(ns("s3_load"), "Load", class = "custom-button btn-sm", width = "100%", icon = icon("cloud-arrow-down"),
                               style = "margin-top:0px;"))
      )
    )),
    # SQL / EDH panel
    hidden(div(id = ns("panel_sql"),
      fluidRow(
        column(4, selectInput(ns("sql_source"), NULL,
          choices = c("EDH", "Snowflake", "Redshift", "PostgreSQL", "Custom JDBC"), width = "100%")),
        column(8, textInput(ns("sql_host"), NULL, placeholder = "host / connection string", width = "100%"))
      ),
      tags$textarea(id = ns("sql_query"),
        style = "width:100%; height:60px; font-family:monospace; font-size:11px; padding:6px; border:1px solid #d1d5db; border-radius:6px; resize:vertical;",
        placeholder = "SELECT * FROM edh.risk.perf_tape WHERE product='PL'"),
      div(style = "margin-top:6px;",
        actionButton(ns("sql_exec"), "Execute & Load", class = "custom-button btn-sm", icon = icon("play")))
    )),
    # KPI row
    uiOutput(ns("kpis"))
  )
}

data_upload_ui <- function() {
  tabItem(tabName = "data_upload",
    header_panel(
      "Data Upload",
      "Upload application, performance, or issued loan data from CSV, Excel, S3, or SQL/EDH sources.",
      icon_name = "cloud-arrow-up"
    ),

    # Product selector
    fluidRow(column(12, product_selector_ui("du"))),

    # Active policy banner (reflects current active per product)
    active_policy_card_ui("du"),

    # Three data panels stacked
    fluidRow(
      column(9,
        du_source_panel("du_app",  "Application Data",
          "Loan applications with FICO, DTI, income, decision flags. Powers policy simulation and what-if analysis."),
        du_source_panel("du_perf", "Performance Data",
          "Loan-level cashflow tape with MOB, delinquency, and payment history. Powers vintage curves and IRR.",
          demo_btn = TRUE),
        du_source_panel("du_iss",  "Issued Data",
          "Funded loan records with amounts, APR, terms, and booking dates. Powers funnel and portfolio analytics.")
      ),
      column(3,
        div(class = "card", style = "padding:12px; position:sticky; top:60px;",
          h5("How It Works", style = "font-weight:bold; margin-top:0;"),
          p(style = "font-size:12px; color:#6b7280; margin-bottom:8px;",
            "Upload data to replace synthetic defaults with your real data across all tabs."),
          tags$div(style = "font-size:11px; color:#6b7280;",
            tags$div(style = "margin-bottom:6px;",
              icon("circle-check", style = "color:#10b981;"), " ",
              tags$b("Application"), " — Policy engine, SMART scanner"),
            tags$div(style = "margin-bottom:6px;",
              icon("circle-check", style = "color:#10b981;"), " ",
              tags$b("Performance"), " — Vintage curves, IRR, delinquency"),
            tags$div(style = "margin-bottom:6px;",
              icon("circle-check", style = "color:#10b981;"), " ",
              tags$b("Issued"), " — Funnel metrics, portfolio KPIs")
          ),
          hr(style = "margin:10px 0;"),
          h6("File Limits", style = "font-weight:600; margin-bottom:6px;"),
          p(style = "font-size:11px; color:#6b7280; margin:0;",
            "Max 50 MB per file, 500K rows")
        )
      )
    )
  )
}

# ---- Server: wire up each panel ----
data_upload_server <- function(input, output, session, perf_tape) {

  # ---- Active policy banner under product selector ----
  wire_active_policy_card(input, output, session, "du")

  # Generic source-tab switcher for a given prefix
  wire_source_switch <- function(prefix) {
    ns <- function(id) paste0(prefix, "_", id)
    switch_fn <- function(src) {
      for (p in c("csv", "excel", "s3", "sql")) {
        if (p == src) shinyjs::show(ns(paste0("panel_", p)))
        else          shinyjs::hide(ns(paste0("panel_", p)))
      }
      btns <- paste0(ns("src_"), c("csv", "excel", "s3", "sql"))
      srcs <- c("csv", "excel", "s3", "sql")
      for (i in seq_along(btns)) {
        if (srcs[i] == src) {
          shinyjs::runjs(sprintf("$('#%s').css({'background-color':'#fb6e36','color':'white','border':'none'})", btns[i]))
        } else {
          shinyjs::runjs(sprintf("$('#%s').css({'background-color':'transparent','color':'#fb6e36','border':'1px solid #fb6e36'})", btns[i]))
        }
      }
    }
    observeEvent(input[[ns("src_csv")]],   switch_fn("csv"))
    observeEvent(input[[ns("src_excel")]], switch_fn("excel"))
    observeEvent(input[[ns("src_s3")]],    switch_fn("s3"))
    observeEvent(input[[ns("src_sql")]],   switch_fn("sql"))
  }

  wire_source_switch("du_app")
  wire_source_switch("du_perf")
  wire_source_switch("du_iss")

  # ---- Performance tape: CSV upload ----
  observeEvent(input$du_perf_file_csv, {
    req(input$du_perf_file_csv$datapath)
    if (file.size(input$du_perf_file_csv$datapath) > 50 * 1024^2) {
      showNotification("File too large (max 50MB)", type = "error", duration = 8); return()
    }
    tryCatch({
      df_raw <- read.csv(input$du_perf_file_csv$datapath, stringsAsFactors = FALSE, check.names = FALSE)
      if (nrow(df_raw) > 500000) { showNotification("Too many rows (max 500K)", type = "error"); return() }
      if (!"loan_id" %in% names(df_raw)) { showNotification("Missing column: loan_id", type = "error"); return() }
      if (!"dcor" %in% names(df_raw)) df_raw$dcor <- 0
      if (!"dpor" %in% names(df_raw)) df_raw$dpor <- 0
      tape <- build_perf_tape_v2(df_raw)
      perf_tape(tape)
      showNotification(sprintf("Performance CSV loaded: %s loans.", format(n_distinct(tape$rows$loan_id), big.mark = ",")),
                       type = "message", duration = 4)
    }, error = function(e) showNotification(paste("Error:", e$message), type = "error", duration = 8))
  })

  # ---- Performance tape: Excel upload ----
  observeEvent(input$du_perf_file_excel, {
    req(input$du_perf_file_excel$datapath)
    tryCatch({
      sheets <- readxl::excel_sheets(input$du_perf_file_excel$datapath)
      updateSelectInput(session, "du_perf_excel_sheet", choices = sheets, selected = sheets[1])
    }, error = function(e) showNotification(paste("Excel error:", e$message), type = "error"))
  })
  observeEvent(input$du_perf_excel_load, {
    req(input$du_perf_file_excel$datapath, input$du_perf_excel_sheet)
    tryCatch({
      df_raw <- as.data.frame(readxl::read_excel(input$du_perf_file_excel$datapath, sheet = input$du_perf_excel_sheet))
      if (!"loan_id" %in% names(df_raw)) { showNotification("Missing: loan_id", type = "error"); return() }
      if (!"dcor" %in% names(df_raw)) df_raw$dcor <- 0
      if (!"dpor" %in% names(df_raw)) df_raw$dpor <- 0
      tape <- build_perf_tape_v2(df_raw)
      perf_tape(tape)
      showNotification(sprintf("Performance Excel loaded: %s loans.", format(n_distinct(tape$rows$loan_id), big.mark = ",")),
                       type = "message", duration = 4)
    }, error = function(e) showNotification(paste("Error:", e$message), type = "error", duration = 8))
  })

  # ---- Performance tape: Demo ----
  observeEvent(input$du_perf_demo, {
    demo_path <- "inst/data/irr_performance_tape_FIXED_v2.csv"
    if (!file.exists(demo_path)) { showNotification("Demo file not found.", type = "error"); return() }
    tryCatch({
      df_raw <- read.csv(demo_path, stringsAsFactors = FALSE, check.names = FALSE)
      tape <- build_perf_tape_v2(df_raw)
      perf_tape(tape)
      showNotification(sprintf("Demo tape loaded: %s loans.", format(n_distinct(tape$rows$loan_id), big.mark = ",")),
                       type = "message", duration = 4)
    }, error = function(e) showNotification(paste("Error:", e$message), type = "error", duration = 8))
  })

  # ---- Performance tape: Clear ----
  observeEvent(input$du_perf_clear,    { perf_tape(NULL); showNotification("Performance tape cleared.", type = "message", duration = 3) })
  observeEvent(input$du_perf_clear_xl, { perf_tape(NULL); showNotification("Performance tape cleared.", type = "message", duration = 3) })

  # ---- Performance tape: KPIs ----
  output$du_perf_kpis <- renderUI({
    tape <- perf_tape()
    if (is.null(tape)) return(NULL)
    n_loans <- n_distinct(tape$rows$loan_id)
    n_rows  <- nrow(tape$rows)
    dt_range <- paste(format(min(tape$rows$payment_date, na.rm = TRUE), "%b %Y"), "\u2013",
                      format(max(tape$rows$payment_date, na.rm = TRUE), "%b %Y"))
    fluidRow(style = "margin-top:8px;",
      column(4, kpi_box(fmt_k(n_loans), "Loans")),
      column(4, kpi_box(fmt_k(n_rows),  "Rows")),
      column(4, kpi_box(dt_range, "Range"))
    )
  })

  # ---- Performance tape: Status badge ----
  output$du_perf_status_badge <- renderUI({
    tape <- perf_tape()
    if (!is.null(tape)) {
      tags$span(style = "background:#e6f4ea; color:#137333; padding:3px 10px; border-radius:12px; font-size:11px; font-weight:600;",
                icon("circle-check"), format(n_distinct(tape$rows$loan_id), big.mark = ","), "loans loaded")
    } else {
      tags$span(style = "background:#f1f3f4; color:#5f6368; padding:3px 10px; border-radius:12px; font-size:11px;",
                "No data")
    }
  })

  # ---- Application data: Status badge (placeholder) ----
  output$du_app_status_badge <- renderUI({
    tags$span(style = "background:#e8f0fe; color:#1967d2; padding:3px 10px; border-radius:12px; font-size:11px; font-weight:600;",
              icon("circle-check"), "50K apps (built-in)")
  })
  output$du_app_kpis <- renderUI(NULL)

  # ---- Issued data: Status badge (placeholder) ----
  output$du_iss_status_badge <- renderUI({
    tags$span(style = "background:#e8f0fe; color:#1967d2; padding:3px 10px; border-radius:12px; font-size:11px; font-weight:600;",
              icon("circle-check"), "13.8K loans (built-in)")
  })
  output$du_iss_kpis <- renderUI(NULL)

  # ---- S3 / SQL placeholders for all panels ----
  for (pfx in c("du_app", "du_perf", "du_iss")) {
    local({
      p <- pfx
      observeEvent(input[[paste0(p, "_s3_load")]], {
        showNotification(HTML("<b>S3</b> — Requires aws.s3 package and valid AWS credentials."), type = "message", duration = 5)
      })
      observeEvent(input[[paste0(p, "_sql_exec")]], {
        showNotification(HTML("<b>SQL</b> — Requires DBI + database driver. Configure in production."), type = "message", duration = 5)
      })
    })
  }

  # ---- App / Issued CSV uploads (placeholder — show notification) ----
  observeEvent(input$du_app_file_csv, {
    showNotification("Application data upload coming soon. Currently using built-in dataset.", type = "warning", duration = 5)
  })
  observeEvent(input$du_iss_file_csv, {
    showNotification("Issued data upload coming soon. Currently using built-in dataset.", type = "warning", duration = 5)
  })
  observeEvent(input$du_app_clear, { showNotification("Application data reset to built-in.", type = "message", duration = 3) })
  observeEvent(input$du_iss_clear, { showNotification("Issued data reset to built-in.", type = "message", duration = 3) })
}
