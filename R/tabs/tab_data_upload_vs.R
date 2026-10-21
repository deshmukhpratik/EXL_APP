# ============================================================
# Tab: Data Upload VS — connectors + validated drop-in replace + cache
# ============================================================
# Three panels:
#   1. Data sources grid (connectors: Snowflake / Databricks / BigQuery / S3 /
#      Azure Synapse / PostgreSQL / SFTP / Local CSV) — Configure modals are
#      demo-only.
#   2. Validated upload: type-aware checklist + preview + commit gate.
#   3. Current performance-tape metadata + cache panel.
# White-card theme to match the sibling Data Upload tab.

# Catalogue of demo connectors
.dvs_data_sources <- list(
  list(id = "local_csv",     name = "Local CSV",        category = "File",
       description = "Uploaded performance tape and loan data (current).",
       status = "connected",   glyph = "\u2261"),
  list(id = "snowflake",     name = "Snowflake",        category = "Warehouse",
       description = "Connect via warehouse / role / database.",
       status = "available",   glyph = "\u2744"),
  list(id = "databricks",    name = "Databricks",       category = "Lakehouse",
       description = "Delta Lake via JDBC + service principal.",
       status = "available",   glyph = "\u25c8"),
  list(id = "bigquery",      name = "Google BigQuery",  category = "Warehouse",
       description = "Service account JSON + dataset path.",
       status = "available",   glyph = "\u25c7"),
  list(id = "s3",            name = "AWS S3 / Parquet", category = "Object store",
       description = "Bucket + prefix + IAM role.",
       status = "available",   glyph = "\u2601"),
  list(id = "azure_synapse", name = "Azure Synapse",    category = "Warehouse",
       description = "Dedicated SQL pool via AAD auth.",
       status = "available",   glyph = "\u25b2"),
  list(id = "postgres",      name = "PostgreSQL",       category = "Operational DB",
       description = "Live loan book via read replica.",
       status = "available",   glyph = "\u25ce"),
  list(id = "sftp_feed",     name = "SFTP partner feed",category = "File",
       description = "Daily / weekly batch from partner.",
       status = "coming_soon", glyph = "\u21c5")
)

.dvs_source_card <- function(s) {
  status_color <- switch(s$status,
    connected   = "#059669",
    available   = "#fb6e36",
    coming_soon = "#6b7280",
    "#6b7280")
  status_label <- switch(s$status,
    connected = "CONNECTED", available = "AVAILABLE",
    coming_soon = "COMING SOON", toupper(s$status))

  action_btn <- if (identical(s$status, "coming_soon")) {
    tags$button("Coming soon", type = "button", disabled = NA,
      style = "background:#f3f4f6; color:#9ca3af; border:none; border-radius:20px; padding:5px 14px; font-size:11px; cursor:not-allowed;")
  } else {
    lbl <- if (identical(s$status, "connected")) "Manage" else "Configure"
    bg  <- if (identical(s$status, "connected")) "transparent" else "#fb6e36"
    fg  <- if (identical(s$status, "connected")) "#fb6e36" else "white"
    tags$button(lbl, type = "button",
      onclick = sprintf("Shiny.setInputValue('dvs_src_configure','%s',{priority:'event'});", s$id),
      style = paste0("background:", bg, "; color:", fg,
                     "; border:1px solid #fb6e36; border-radius:20px;",
                     " padding:5px 14px; font-size:11px; cursor:pointer;"))
  }

  div(
    style = paste0("background:#ffffff; border:1px solid #e5e7eb; border-radius:10px;",
                   " padding:14px; color:#111827; display:flex; flex-direction:column;",
                   " gap:10px; min-height:170px;",
                   " box-shadow:0 1px 2px rgba(0,0,0,0.04);"),
    div(style = "display:flex; align-items:center; gap:12px;",
      div(style = paste0("font-size:22px; color:#fb6e36; width:36px; height:36px;",
                          " border-radius:8px; background:#fff4ec; display:flex;",
                          " align-items:center; justify-content:center;"),
          HTML(s$glyph)),
      div(
        div(s$name, style = "font-weight:600; color:#111827; font-size:14px;"),
        div(s$category, style = "font-size:11px; color:#6b7280;")
      )
    ),
    div(s$description, style = "font-size:12px; color:#6b7280; flex:1;"),
    div(style = "display:flex; justify-content:space-between; align-items:center;",
      tags$span(status_label,
        style = paste0("font-size:10px; font-weight:600; color:", status_color,
                       "; background:", status_color, "15; padding:3px 8px; border-radius:10px;")),
      action_btn
    )
  )
}

data_upload_vs_ui <- function() {
  tabItem(tabName = "data_upload_vs",
    tags$style(HTML("
      #data_upload_vs_tab .duv-card {
        background:#ffffff; color:#111827; border:1px solid #e5e7eb;
        border-radius:8px; padding:18px; margin-bottom:16px;
        box-shadow: 0 1px 2px rgba(0,0,0,0.04);
      }
      #data_upload_vs_tab .duv-card h4 {
        color:#111827; margin-top:0; margin-bottom:4px;
        font-size:15px; font-weight:600;
      }
      #data_upload_vs_tab .duv-card .duv-subtitle {
        color:#6b7280; font-size:12px; margin:0 0 14px 0;
      }
      #data_upload_vs_tab .check-row {
        padding:8px 0; border-bottom:1px solid #f3f4f6;
        display:flex; align-items:baseline; gap:10px;
      }
      #data_upload_vs_tab .check-row:last-child { border-bottom:none; }
      #data_upload_vs_tab .check-pass { color:#059669; font-weight:700; min-width:54px; }
      #data_upload_vs_tab .check-warn { color:#d97706; font-weight:700; min-width:54px; }
      #data_upload_vs_tab .check-fail { color:#dc2626; font-weight:700; min-width:54px; }
      #data_upload_vs_tab .check-name { font-weight:600; color:#111827; }
      #data_upload_vs_tab .check-detail { color:#6b7280; font-size:13px; }
      #data_upload_vs_tab .duv-banner-fail {
        background:#fef2f2; color:#991b1b; padding:10px 14px;
        border-radius:6px; margin-bottom:12px; font-weight:600;
      }
    ")),
    div(id = "data_upload_vs_tab",
      header_panel(
        "Data Upload",
        "Connect a data source or drop a validated file into the live session."
      ),

      # Product selector (matches other tabs)
      fluidRow(column(12, product_selector_ui("duv"))),

      # Active policy banner (reflects current active per product)
      active_policy_card_ui("duv"),

      # ---- Step 1: Choose data type ----
      div(class = "duv-card",
        h4("1. Choose data type"),
        p(class = "duv-subtitle",
          "Pick the kind of data you're bringing in. This drives validation and where it lands in the app."),
        selectInput("duv_data_type", NULL,
                    choices = c("Application data" = "application",
                                "Issued loans"     = "issued",
                                "Performance tape" = "performance",
                                "IRR data"         = "irr",
                                "Bureau data"      = "bureau"),
                    selected = "issued",
                    width = "360px")
      ),

      # ---- Step 2: Choose a connection ----
      div(class = "duv-card",
        h4("2. Choose a connection"),
        p(class = "duv-subtitle",
          uiOutput("duv_connection_hint", inline = TRUE)),
        div(style = "display:grid; grid-template-columns:repeat(auto-fit, minmax(260px, 1fr)); gap:12px;",
          do.call(tagList, lapply(.dvs_data_sources, .dvs_source_card)))
      ),

      # ---- Step 2b: Direct file drop ----
      div(class = "duv-card",
        h4("Or drop a file directly"),
        p(class = "duv-subtitle",
          "Upload a CSV or Excel file. The Data Quality report opens automatically after parsing."),
        fileInput("duv_file", NULL,
                  accept = c(".csv", ".xlsx", ".xls"),
                  width = "100%"),
        actionButton("duv_commit", "Use this data",
                     icon = icon("check"),
                     class = "btn-primary",
                     style = "width:auto; opacity:0.5;"),
        tags$small(style = "color:#6b7280; display:block; margin-top:8px;",
                   "Commit is disabled until structural checks pass.")
      ),

      tags$script(HTML("
        Shiny.addCustomMessageHandler('duv_toggle_commit', function(msg) {
          var btn = document.getElementById('duv_commit');
          if (!btn) return;
          btn.disabled = !msg.ok;
          btn.style.opacity = msg.ok ? '1' : '0.5';
          btn.style.cursor  = msg.ok ? 'pointer' : 'not-allowed';
        });
      "))
    )
  )
}

data_upload_vs_server <- function(input, output, session, perf_tape = NULL) {

  # ---- Active policy banner under product selector ----
  wire_active_policy_card(input, output, session, "duv")

  perf_cache_path <- "inst/data/perf_tape_cache.rds"
  irr_cache_path  <- "inst/data/loan_irr_cache.rds"

  # Sources grid is rendered inline in UI (see data_upload_vs_ui).
  # Force suspendWhenHidden = FALSE on data-driven outputs so they render
  # even when shinydashboard hasn't fired the tab-activation hook.

  .dvs_form_fields <- function(sid) {
    switch(sid,
      snowflake = tagList(
        textInput("dvs_cfg_account", "Account", placeholder = "xy12345.us-east-1"),
        textInput("dvs_cfg_warehouse", "Warehouse", placeholder = "ANALYTICS_WH"),
        textInput("dvs_cfg_database", "Database", placeholder = "RISK_DW"),
        textInput("dvs_cfg_schema", "Schema", placeholder = "LOANS"),
        textInput("dvs_cfg_role", "Role", placeholder = "ANALYST_READ"),
        passwordInput("dvs_cfg_auth", "Token / password")
      ),
      databricks = tagList(
        textInput("dvs_cfg_host", "Workspace host", placeholder = "adb-1234.5.databricks.com"),
        textInput("dvs_cfg_http", "SQL warehouse HTTP path", placeholder = "/sql/1.0/warehouses/abcd1234"),
        textInput("dvs_cfg_catalog", "Catalog", placeholder = "risk"),
        textInput("dvs_cfg_schema", "Schema", placeholder = "loans"),
        passwordInput("dvs_cfg_auth", "Personal access token")
      ),
      bigquery = tagList(
        textInput("dvs_cfg_project", "Project ID", placeholder = "my-gcp-project"),
        textInput("dvs_cfg_dataset", "Dataset", placeholder = "risk_analytics"),
        textInput("dvs_cfg_location", "Location", placeholder = "US"),
        fileInput("dvs_cfg_creds", "Service account JSON",
                  accept = c(".json", "application/json"))
      ),
      s3 = tagList(
        textInput("dvs_cfg_bucket", "Bucket", placeholder = "my-risk-data"),
        textInput("dvs_cfg_prefix", "Prefix / path", placeholder = "loans/parquet/"),
        textInput("dvs_cfg_region", "AWS region", placeholder = "us-east-1"),
        textInput("dvs_cfg_role_arn", "IAM role ARN",
                  placeholder = "arn:aws:iam::123:role/AnalystRead")
      ),
      azure_synapse = tagList(
        textInput("dvs_cfg_workspace", "Synapse workspace", placeholder = "mycompany-synapse"),
        textInput("dvs_cfg_pool", "Dedicated SQL pool", placeholder = "analytics_pool"),
        textInput("dvs_cfg_database", "Database", placeholder = "risk"),
        textInput("dvs_cfg_tenant", "Tenant ID", placeholder = "00000000-0000-...")
      ),
      postgres = tagList(
        textInput("dvs_cfg_host", "Host", placeholder = "db.internal.example.com"),
        numericInput("dvs_cfg_port", "Port", value = 5432),
        textInput("dvs_cfg_database", "Database", placeholder = "loans_replica"),
        textInput("dvs_cfg_schema", "Schema", placeholder = "public"),
        textInput("dvs_cfg_user", "User", placeholder = "read_only"),
        passwordInput("dvs_cfg_auth", "Password")
      ),
      local_csv = div(style = "color:#6b7280; font-size:12px;",
        "Local CSV is the active source. Drop files into the Validated upload panel above."),
      div(style = "color:#6b7280; font-size:12px;", "No configuration fields defined.")
    )
  }

  observeEvent(input$dvs_src_configure, {
    sid <- input$dvs_src_configure
    src <- NULL
    for (s in .dvs_data_sources) if (identical(s$id, sid)) { src <- s; break }
    if (is.null(src)) return()

    showModal(modalDialog(
      title = paste0("Configure \u2014 ", src$name),
      size = "l", easyClose = TRUE,
      div(style = "display:flex; gap:10px; align-items:center; margin-bottom:8px;",
          tags$span(src$category,
            style = paste0("font-size:11px; color:#6b7280; background:#f1f3f4;",
                            " padding:3px 8px; border-radius:10px;")),
          tags$span(src$description,
            style = "font-size:12px; color:#6b7280;")),
      tags$hr(),
      .dvs_form_fields(sid),
      footer = tagList(
        modalButton("Cancel"),
        if (!identical(sid, "local_csv"))
          actionButton("dvs_src_test", "Test connection", class = "btn btn-default btn-sm"),
        if (!identical(sid, "local_csv"))
          actionButton("dvs_src_save", "Save & activate", class = "btn-primary btn-sm")
      )
    ))
  })

  observeEvent(input$dvs_src_test, {
    showNotification("Demo mode: connection test simulated. Real drivers ship with the full deployment.",
                     type = "default", duration = 4)
  })
  observeEvent(input$dvs_src_save, {
    removeModal()
    showNotification("Demo mode: source configuration saved (not persisted).",
                     type = "default", duration = 5)
  })

  # ── Validated upload ────────────────────────────────────────
  parsed <- reactiveVal(NULL)
  report <- reactiveVal(NULL)

  # Human label for the selected data type (used in the connection hint).
  .duv_type_label <- function(t) {
    switch(t %||% "issued",
      application = "Application data",
      issued      = "Issued loans",
      performance = "Performance tape",
      irr         = "IRR data",
      bureau      = "Bureau data",
      "your data")
  }

  output$duv_connection_hint <- renderUI({
    HTML(paste0("How do you want to bring in <b>",
                .duv_type_label(input$duv_data_type),
                "</b>?"))
  })

  session$onFlushed(function() {
    session$sendCustomMessage("duv_toggle_commit", list(ok = FALSE))
  }, once = TRUE)

  # Build the Data Quality modal body from the current report().
  .duv_dq_modal <- function() {
    r <- report()
    if (is.null(r)) return(NULL)
    rows <- lapply(r$checks, function(ch) {
      cls <- paste0("check-", ch$status)
      div(class = "check-row",
          span(class = cls, toupper(ch$status)),
          span(class = "check-name", ch$name),
          span(class = "check-detail", "\u2014 ", ch$detail))
    })
    modalDialog(
      title = NULL, size = "l", easyClose = TRUE,
      div(id = "data_upload_vs_tab",
        h4("Data quality report", style = "margin-top:0;"),
        if (!isTRUE(r$ok_to_commit))
          div(class = "duv-banner-fail",
              "Fix the failing structural checks before committing."),
        rows,
        tags$hr(),
        h4("Preview (first 20 rows)", style = "margin-bottom:8px;"),
        if (!is.null(r$preview) && nrow(r$preview) > 0)
          DT::datatable(r$preview,
                        options = list(pageLength = 20, scrollX = TRUE, dom = "tp"),
                        rownames = FALSE)
        else
          em(style = "color:#6b7280;", "No rows to preview.")
      ),
      footer = tagList(
        modalButton("Close"),
        actionButton("duv_commit_modal", "Use this data",
                     icon = icon("check"), class = "btn-primary",
                     style = if (isTRUE(r$ok_to_commit)) "" else "opacity:0.5; pointer-events:none;")
      )
    )
  }

  observeEvent(input$duv_file, {
    req(input$duv_file)
    path <- input$duv_file$datapath
    df <- tryCatch({
      if (grepl("\\.xlsx?$", input$duv_file$name, ignore.case = TRUE)) {
        readxl::read_excel(path)
      } else {
        read.csv(path, stringsAsFactors = FALSE)
      }
    }, error = function(e) {
      showNotification(paste("Parse error:", e$message), type = "error")
      NULL
    })
    parsed(df)
    if (!is.null(df)) {
      r <- validate_upload(as.data.frame(df), input$duv_data_type)
      report(r)
      session$sendCustomMessage("duv_toggle_commit", list(ok = isTRUE(r$ok_to_commit)))
      showModal(.duv_dq_modal())
    } else {
      report(NULL)
      session$sendCustomMessage("duv_toggle_commit", list(ok = FALSE))
    }
  })

  # If the user changes data type after a file is parsed, re-run validation
  # and re-open the modal so they see the new report.
  observeEvent(input$duv_data_type, {
    df <- parsed()
    if (!is.null(df)) {
      r <- validate_upload(as.data.frame(df), input$duv_data_type)
      report(r)
      session$sendCustomMessage("duv_toggle_commit", list(ok = isTRUE(r$ok_to_commit)))
      showModal(.duv_dq_modal())
    }
  }, ignoreInit = TRUE)

  .duv_commit_dataset <- function() {
    r <- report()
    req(!is.null(r), isTRUE(r$ok_to_commit))
    df <- parsed()
    if (is.null(df)) return(invisible())
    target <- UPLOAD_DATA_CONTRACTS[[input$duv_data_type]]$target
    if (is.null(target)) {
      showNotification("No target defined for this data type.", type = "error")
      return(invisible())
    }
    # NOTE: single-session demo — writes to .GlobalEnv so every tab sees
    # the replacement immediately. Multi-tenant deployments would need
    # per-session state.
    assign(target, as.data.frame(df), envir = .GlobalEnv)
    showNotification(paste0("Dataset '", target, "' replaced \u2014 ",
                            nrow(df), " rows loaded."),
                     type = "message", duration = 6)
  }

  observeEvent(input$duv_commit, {
    .duv_commit_dataset()
  })

  observeEvent(input$duv_commit_modal, {
    .duv_commit_dataset()
    removeModal()
  })

  # (Current-tape metadata + Cache panel removed — not part of the demo UX.)

  outputOptions(output, "duv_connection_hint",   suspendWhenHidden = FALSE)

  invisible(NULL)
}
