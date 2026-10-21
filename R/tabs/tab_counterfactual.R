# ============================================================
# Tab: Counterfactual Explainer
# For declined loans, discover the shortest path to approval —
# what needs to change and by how much.
# ============================================================

counterfactual_ui <- function() {
  tabItem(tabName = "counterfactual",

    header_panel(
      "Counterfactual Explainer",
      "For declined loans, discover the shortest path to approval — what needs to change and by how much.",
      icon_name = "code-branch"
    ),

    # Product selector
    fluidRow(column(12, product_selector_ui("cf"))),

    # View mode toggle
    fluidRow(column(12, div(class = "card",
      shinyWidgets::radioGroupButtons(
        inputId  = "cf_view_mode",
        label    = "Analysis Mode",
        choices  = c("Loan-Level" = "loan", "Segment-Level" = "segment"),
        selected = "loan",
        justified = TRUE,
        status   = "warning",
        individual = TRUE
      )
    ))),

    # ---- Loan-Level View ----
    conditionalPanel(
      condition = "input.cf_view_mode == 'loan'",

      fluidRow(column(12, div(class = "card",
        h4("Declined Loans", style = "font-weight:bold; margin-bottom:4px;"),
        p("Select a loan to see what changes would have led to approval.",
          style = "color:#9ca3af; font-size:12px; margin-bottom:10px;"),
        DT::DTOutput("cf_declined_table")
      ))),

      uiOutput("cf_loan_result")
    ),

    # ---- Segment-Level View ----
    conditionalPanel(
      condition = "input.cf_view_mode == 'segment'",

      fluidRow(column(12, div(class = "card",
        h4("Segment Analysis", style = "font-weight:bold; margin-bottom:4px;"),
        p("Choose a decline reason to see how many loans would be rescued by relaxing each policy threshold.",
          style = "color:#9ca3af; font-size:12px; margin-bottom:10px;"),
        fluidRow(
          column(5,
            selectInput("cf_decline_reason", "Decline Reason",
              choices  = c("Loading..." = ""),
              selected = "",
              width    = "100%")
          ),
          column(3,
            div(style = "padding-top:25px;",
              actionButton("cf_seg_run", "Analyze Segment",
                icon  = icon("magnifying-glass-chart"),
                class = "custom-button",
                style = "width:100%;")
            )
          )
        )
      ))),

      uiOutput("cf_seg_result")
    )
  )
}


# ============================================================
# Server
# ============================================================

counterfactual_server <- function(input, output, session,
                                  credit_data, policy_input_data,
                                  loan_irr_cache) {

  # ---- Product selector wiring ----
  cf_selected_product <- reactiveVal("Personal Loan")
  observeEvent(input$cf_product, {
    cf_selected_product(input$cf_product)
  }, ignoreInit = TRUE)

  # ---- Variable display names ----
  cf_var_labels <- c(
    fico_score                 = "FICO Score",
    debt_to_income_ratio       = "Debt-to-Income Ratio",
    annual_income              = "Annual Income",
    delinquent_accounts_count  = "Delinquent Accounts",
    credit_file_age_months     = "Credit File Age (Mo)"
  )

  # ---- Declined loans reactive ----
  declined_data <- reactive({
    cd <- credit_data
    if (is.null(cd) || nrow(cd) == 0) return(NULL)
    prod <- cf_selected_product()
    if (!is.null(prod) && prod != "Personal Loan" && "product_type" %in% names(cd)) {
      cd <- cd[!is.na(cd$product_type) & cd$product_type == prod, , drop = FALSE]
    }
    if (!"offered_decision_flag" %in% names(cd)) return(NULL)
    cd[!is.na(cd$offered_decision_flag) & cd$offered_decision_flag != "APPR", , drop = FALSE]
  })

  # ---- Declined loans table ----
  output$cf_declined_table <- DT::renderDT({
    df <- declined_data()
    if (is.null(df) || nrow(df) == 0) {
      return(DT::datatable(data.frame(Message = "No declined loans found."),
                           options = list(dom = "t"), rownames = FALSE))
    }

    show_cols <- c("loan_id", "fico_score", "debt_to_income_ratio",
                   "annual_income", "delinquent_accounts_count",
                   "credit_file_age_months", "offered_decision_flag")
    show_cols <- intersect(show_cols, names(df))
    disp <- df[, show_cols, drop = FALSE]

    # Format display columns
    if ("debt_to_income_ratio" %in% names(disp))
      disp$debt_to_income_ratio <- paste0(round(disp$debt_to_income_ratio * 100, 1), "%")
    if ("annual_income" %in% names(disp))
      disp$annual_income <- paste0("$", formatC(round(disp$annual_income), format = "d", big.mark = ","))

    col_labels <- c(
      loan_id                   = "Loan ID",
      fico_score                = "FICO",
      debt_to_income_ratio      = "DTI",
      annual_income             = "Annual Income",
      delinquent_accounts_count = "Delinq. Accts",
      credit_file_age_months    = "File Age (Mo)",
      offered_decision_flag     = "Decline Reason"
    )
    colnames(disp) <- vapply(names(disp), function(n) col_labels[[n]] %||% n, character(1))

    DT::datatable(
      disp,
      selection = "single",
      rownames  = FALSE,
      options   = list(
        pageLength = 10,
        dom        = "lftip",
        scrollX    = TRUE
      )
    )
  })

  # ============================================================
  # Counterfactual engine
  # ============================================================

  # Perturbation spec: variable -> list(step, max_delta, direction)
  # direction "up" means we add steps, "down" means we subtract steps
  cf_perturb_spec <- list(
    fico_score = list(
      step      = 5,
      max_delta = 100,
      direction = "up"
    ),
    debt_to_income_ratio = list(
      step      = 0.01,
      max_delta = 0.20,
      direction = "down"
    ),
    annual_income = list(
      step      = 2000,
      max_delta = 100000,
      direction = "up"
    ),
    delinquent_accounts_count = list(
      step      = 1,
      max_delta = 5,
      direction = "down"
    ),
    credit_file_age_months = list(
      step      = 3,
      max_delta = 60,
      direction = "up"
    )
  )

  find_counterfactuals <- function(loan_row, pid_row) {
    # loan_row: 1-row data frame from credit_data (for display)
    # pid_row:  1-row data frame from policy_input_data (for re-running policy)
    default_params <- build_default_params(policy_manifest)
    paths <- list()

    for (var_name in names(cf_perturb_spec)) {
      spec <- cf_perturb_spec[[var_name]]
      if (!var_name %in% names(pid_row)) next

      orig_val <- pid_row[[var_name]]
      if (is.na(orig_val)) next

      n_steps_max <- floor(spec$max_delta / spec$step)

      for (k in seq_len(n_steps_max)) {
        delta <- spec$step * k
        new_val <- if (spec$direction == "up") orig_val + delta else orig_val - delta

        # Clamp sensible bounds
        if (var_name == "delinquent_accounts_count") new_val <- max(0, new_val)
        if (var_name == "debt_to_income_ratio")       new_val <- max(0, new_val)
        if (var_name == "fico_score")                 new_val <- min(850, new_val)
        if (var_name == "credit_file_age_months")     new_val <- max(0, new_val)
        if (var_name == "annual_income")              new_val <- max(0, new_val)

        test_row <- pid_row
        test_row[[var_name]] <- new_val

        result <- tryCatch(
          apply_policy_with_params(test_row, default_params),
          error = function(e) NULL
        )

        if (!is.null(result) && "offered_decision_flag" %in% names(result) &&
            nrow(result) > 0 && result$offered_decision_flag[1] == "APPR") {
          paths[[length(paths) + 1]] <- list(
            variable  = var_name,
            orig_val  = orig_val,
            new_val   = new_val,
            delta     = new_val - orig_val,
            n_steps   = k,
            label     = cf_var_labels[[var_name]] %||% var_name
          )
          break  # Found minimum steps for this variable; move to next
        }
      }
    }

    if (length(paths) == 0) return(NULL)

    # Sort by fewest steps; return top 3
    n_steps_vec <- vapply(paths, `[[`, numeric(1), "n_steps")
    paths[order(n_steps_vec)[seq_len(min(3, length(paths)))]]
  }

  # ---- Loan-level result (triggered by row click) ----
  output$cf_loan_result <- renderUI({
    sel <- input$cf_declined_table_rows_selected
    if (is.null(sel) || length(sel) == 0) return(NULL)

    df_dec <- declined_data()
    if (is.null(df_dec) || nrow(df_dec) == 0) return(NULL)
    if (sel > nrow(df_dec)) return(NULL)

    loan_row <- df_dec[sel, , drop = FALSE]

    # Match in policy_input_data by loan_id
    pid <- policy_input_data
    if (is.null(pid) || nrow(pid) == 0) return(NULL)

    lid <- loan_row$loan_id[1]
    pid_row <- pid[pid$loan_id == lid, , drop = FALSE]
    if (nrow(pid_row) == 0) pid_row <- pid[sel, , drop = FALSE]
    if (nrow(pid_row) == 0) return(NULL)
    pid_row <- pid_row[1, , drop = FALSE]

    decline_reason <- loan_row$offered_decision_flag[1]

    paths <- withProgress(
      message = "Computing counterfactuals...", value = 0.5,
      find_counterfactuals(loan_row, pid_row)
    )

    tagList(
      fluidRow(column(12, div(class = "card",
        h4("Counterfactual Analysis", style = "font-weight:bold; margin-bottom:4px;"),
        tags$p(
          icon("circle-xmark", style = "color:#ef4444; margin-right:6px;"),
          tags$b("Decline reason: "),
          tags$span(decline_reason, style = "color:#ef4444;"),
          style = "margin-bottom:12px; font-size:14px;"
        ),

        if (is.null(paths)) {
          div(style = "color:#9ca3af; padding:20px; text-align:center;",
            icon("circle-info"),
            " No single-variable perturbation can rescue this loan within the defined bounds."
          )
        } else {
          tagList(
            p("The table below shows the minimum change required to each variable to achieve approval.",
              style = "color:#9ca3af; font-size:12px; margin-bottom:10px;"),

            # Gap table
            div(style = "margin-bottom:16px;",
              tags$table(
                class = "table table-condensed",
                style = "width:100%; font-size:13px;",
                tags$thead(
                  tags$tr(
                    tags$th("Variable"),
                    tags$th("Current"),
                    tags$th("Needed"),
                    tags$th("Gap")
                  )
                ),
                tags$tbody(
                  lapply(paths, function(p) {
                    is_pct  <- p$variable == "debt_to_income_ratio"
                    is_dol  <- p$variable == "annual_income"
                    fmt_val <- function(v) {
                      if (is_pct) paste0(round(v * 100, 1), "%")
                      else if (is_dol) paste0("$", formatC(round(v), format = "d", big.mark = ","))
                      else formatC(round(v, 1), format = "f", digits = 1, big.mark = ",")
                    }
                    gap_txt <- fmt_val(p$new_val - p$orig_val)
                    gap_col <- if ((p$new_val - p$orig_val) > 0) "#10b981" else "#fb6e36"
                    tags$tr(
                      tags$td(tags$b(p$label)),
                      tags$td(fmt_val(p$orig_val)),
                      tags$td(fmt_val(p$new_val),
                        style = "color:#10b981; font-weight:600;"),
                      tags$td(gap_txt,
                        style = paste0("color:", gap_col, "; font-weight:600;"))
                    )
                  })
                )
              )
            ),

            # Horizontal bar chart — relative change per variable
            plotly::renderPlotly({
              labels   <- vapply(paths, `[[`, character(1), "label")
              deltas   <- vapply(paths, function(p) abs(p$delta / max(abs(p$orig_val), 1e-6)) * 100, numeric(1))
              bar_cols <- rep("#fb6e36", length(labels))

              plotly::plot_ly(
                x           = deltas,
                y           = labels,
                type        = "bar",
                orientation = "h",
                marker      = list(color = bar_cols)
              ) %>%
                plotly::layout(
                  title      = list(text = "Relative Change Required (%)", font = list(size = 13)),
                  xaxis      = list(title = "% Change from Current Value",
                                    ticksuffix = "%",
                                    color = "#d1d5db",
                                    gridcolor = "#374151"),
                  yaxis      = list(title = "",
                                    color = "#d1d5db",
                                    autorange = "reversed"),
                  paper_bgcolor = "#1a1a2e",
                  plot_bgcolor  = "#1a1a2e",
                  font          = list(color = "#d1d5db"),
                  margin        = list(l = 160, r = 30, t = 50, b = 50)
                )
            })
          )
        }
      )))
    )
  })

  # ---- Populate decline reason picker ----
  observe({
    df_dec <- declined_data()
    if (is.null(df_dec) || nrow(df_dec) == 0) return()
    if (!"offered_decision_flag" %in% names(df_dec)) return()
    reasons <- sort(unique(df_dec$offered_decision_flag))
    reasons <- reasons[!is.na(reasons) & reasons != "APPR"]
    updateSelectInput(session, "cf_decline_reason",
      choices  = setNames(reasons, reasons),
      selected = if (length(reasons) > 0) reasons[1] else NULL)
  })

  # ---- Segment-level analysis ----
  seg_result <- eventReactive(input$cf_seg_run, {
    df_dec <- declined_data()
    if (is.null(df_dec) || nrow(df_dec) == 0) return(NULL)

    reason <- input$cf_decline_reason
    if (is.null(reason) || reason == "") return(NULL)

    seg_df <- df_dec[!is.na(df_dec$offered_decision_flag) &
                     df_dec$offered_decision_flag == reason, , drop = FALSE]
    if (nrow(seg_df) == 0) return(NULL)

    pid <- policy_input_data
    if (is.null(pid)) return(NULL)

    # Match pid rows to segment
    pid_seg <- pid[pid$loan_id %in% seg_df$loan_id, , drop = FALSE]

    default_params <- build_default_params(policy_manifest)

    # Mapping: decline reason -> var and param info
    reason_map <- list(
      "01. Fico" = list(
        var        = "fico_score",
        param      = "fico_min",
        relax      = "lower",
        current    = default_params$fico_min,
        step_size  = 5,
        n_steps    = 10,
        threshold_label = "FICO Minimum"
      ),
      "02. Thin file" = list(
        var        = "credit_file_age_months",
        param      = "thin_file_months",
        relax      = "lower",
        current    = default_params$thin_file_months,
        step_size  = 2,
        n_steps    = 10,
        threshold_label = "Thin File Threshold (Mo)"
      ),
      "03. Delinquency decline" = list(
        var        = "delinquent_accounts_count",
        param      = "delinquency_max",
        relax      = "raise",
        current    = default_params$delinquency_max,
        step_size  = 1,
        n_steps    = 5,
        threshold_label = "Max Delinquent Accounts"
      ),
      "04. HighDTI" = list(
        var        = "debt_to_income_ratio",
        param      = "dti_cutoff",
        relax      = "raise",
        current    = default_params$dti_cutoff,
        step_size  = 0.02,
        n_steps    = 10,
        threshold_label = "DTI Cutoff"
      ),
      "05. max_amount_limit" = list(
        var        = "annual_income",
        param      = NULL,
        relax      = NA,
        current    = NA,
        step_size  = NA,
        n_steps    = NA,
        threshold_label = "Annual Income"
      ),
      "05. HardInquiry" = list(
        var        = "N_HARD_INQ_6M",
        param      = "n_hard_inq_6m_max",
        relax      = "raise",
        current    = default_params$n_hard_inq_6m_max %||% 3,
        step_size  = 1,
        n_steps    = 5,
        threshold_label = "Max Hard Inquiries (6M)"
      ),
      "06. Existing customer only" = list(
        var        = "borrower_repeat_type",
        param      = NULL,
        relax      = NA,
        current    = NA,
        step_size  = NA,
        n_steps    = NA,
        threshold_label = "Existing Customer Gate (not adjustable)"
      )
    )

    info <- reason_map[[reason]]
    if (is.null(info)) return(list(seg_df = seg_df, info = NULL, sim_table = NULL))

    # Segment histogram data (variable distribution for declined loans)
    hist_var  <- info$var
    hist_data <- if (hist_var %in% names(seg_df)) seg_df[[hist_var]] else numeric(0)

    # Rescue simulation (only for mappable decline reasons with param)
    sim_table <- NULL
    if (!is.na(info$relax) && !is.null(info$param) && nrow(pid_seg) > 0) {
      current_threshold <- info$current
      steps_seq <- seq_len(info$n_steps)
      sim_rows <- lapply(steps_seq, function(k) {
        delta <- info$step_size * k
        new_threshold <- if (info$relax == "lower") {
          current_threshold - delta
        } else {
          current_threshold + delta
        }

        mod_params <- default_params
        mod_params[[info$param]] <- new_threshold

        rescued_n <- tryCatch({
          result <- apply_policy_with_params(pid_seg, mod_params)
          if ("offered_decision_flag" %in% names(result)) {
            sum(result$offered_decision_flag == "APPR", na.rm = TRUE)
          } else 0L
        }, error = function(e) 0L)

        data.frame(
          threshold  = round(new_threshold, 4),
          rescued    = rescued_n,
          pct_of_seg = if (nrow(seg_df) > 0) round(rescued_n / nrow(seg_df) * 100, 1) else 0,
          stringsAsFactors = FALSE
        )
      })
      sim_table <- do.call(rbind, sim_rows)
    }

    list(
      seg_df    = seg_df,
      info      = info,
      hist_data = hist_data,
      sim_table = sim_table,
      reason    = reason
    )
  })

  output$cf_seg_result <- renderUI({
    res <- seg_result()
    if (is.null(res)) return(NULL)

    seg_df    <- res$seg_df
    info      <- res$info
    sim_table <- res$sim_table
    reason    <- res$reason

    if (is.null(info)) {
      return(fluidRow(column(12, div(class = "card",
        div(style = "color:#9ca3af; text-align:center; padding:20px;",
          icon("circle-info"),
          paste0(" '", reason, "' does not map to a single adjustable threshold.")
        )
      ))))
    }

    tagList(
      # Segment summary
      fluidRow(column(12, div(class = "card",
        fluidRow(
          column(4, kpi_box(
            value = formatC(nrow(seg_df), format = "d", big.mark = ","),
            label = paste0("Declined: ", reason),
            sub   = "loans in segment"
          )),
          column(4, kpi_box(
            value = if (!is.null(sim_table) && nrow(sim_table) > 0)
                      paste0(max(sim_table$rescued, na.rm = TRUE), " max")
                    else "N/A",
            label = "Max Rescuable (in bounds)",
            sub   = "at most relaxed threshold"
          )),
          column(4, kpi_box(
            value = if (!is.null(sim_table) && nrow(sim_table) > 0)
                      paste0(max(sim_table$pct_of_seg, na.rm = TRUE), "%")
                    else "N/A",
            label = "% of Segment Rescued",
            sub   = "at most relaxed threshold"
          ))
        )
      ))),

      # Variable distribution histogram
      fluidRow(column(12, div(class = "card",
        h4(paste0("Distribution: ", info$threshold_label),
           style = "font-weight:bold; margin-bottom:4px;"),
        p("Declined loan distribution vs current threshold (red dashed).",
          style = "color:#9ca3af; font-size:12px; margin-bottom:10px;"),
        plotly::renderPlotly({
          hd <- res$hist_data
          if (length(hd) == 0 || all(is.na(hd))) {
            return(plotly::plot_ly() %>%
              plotly::layout(
                title = list(text = "No data", font = list(color = "#d1d5db")),
                paper_bgcolor = "#1a1a2e",
                plot_bgcolor  = "#1a1a2e"
              ))
          }
          p <- plotly::plot_ly(
            x    = hd,
            type = "histogram",
            marker = list(color = "#fb6e36", opacity = 0.75,
                          line = list(color = "#d1d5db", width = 0.5))
          )
          # Add threshold line if applicable
          if (!is.na(info$current)) {
            p <- p %>%
              plotly::layout(
                shapes = list(list(
                  type = "line",
                  x0   = info$current, x1 = info$current,
                  y0   = 0, y1   = 1,
                  yref = "paper",
                  line = list(color = "#ef4444", width = 2, dash = "dash")
                )),
                annotations = list(list(
                  x         = info$current,
                  y         = 1,
                  yref      = "paper",
                  text      = paste0("Current: ", round(info$current, 3)),
                  showarrow = FALSE,
                  xanchor   = "left",
                  font      = list(color = "#ef4444", size = 11)
                ))
              )
          }
          p %>%
            plotly::layout(
              xaxis = list(title = info$threshold_label,
                           color = "#d1d5db", gridcolor = "#374151"),
              yaxis = list(title = "Count",
                           color = "#d1d5db", gridcolor = "#374151"),
              paper_bgcolor = "#1a1a2e",
              plot_bgcolor  = "#1a1a2e",
              font          = list(color = "#d1d5db"),
              margin        = list(l = 50, r = 30, t = 30, b = 50)
            )
        })
      ))),

      # Rescue simulation table
      if (!is.null(sim_table) && nrow(sim_table) > 0) {
        fluidRow(column(12, div(class = "card",
          h4("Threshold Relaxation Rescue Simulation",
             style = "font-weight:bold; margin-bottom:4px;"),
          p(paste0("Showing impact of relaxing '", info$threshold_label,
                   "' from its current value of ", round(info$current, 3),
                   " in increments of ", info$step_size, "."),
            style = "color:#9ca3af; font-size:12px; margin-bottom:10px;"),
          DT::renderDT({
            display_tbl <- sim_table
            names(display_tbl) <- c(
              paste0(info$threshold_label, " (New)"),
              "Loans Rescued",
              "% of Segment"
            )
            DT::datatable(
              display_tbl,
              rownames = FALSE,
              options  = list(
                dom        = "t",
                pageLength = 15,
                scrollX    = FALSE
              )
            ) %>%
              DT::formatStyle(
                "Loans Rescued",
                background = DT::styleColorBar(c(0, max(sim_table$rescued, na.rm = TRUE)), "#10b981"),
                backgroundSize   = "100% 90%",
                backgroundRepeat = "no-repeat",
                backgroundPosition = "center"
              )
          })
        )))
      } else {
        fluidRow(column(12, div(class = "card",
          div(style = "color:#9ca3af; text-align:center; padding:16px;",
            icon("circle-info"),
            " Rescue simulation not available for this decline reason."
          )
        )))
      }
    )
  })
}
