# ============================================================
# NL Q&A Bar — UI and helpers
# Design: docs/superpowers/specs/2026-04-21-nl-qa-bar-design.md
# ============================================================

NL_ENABLED_TABS <- c("smart_rec", "opp_find", "risk_driver", "whatif_tab")

# ---- Bar UI ---------------------------------------------------------------

nl_bar_ui <- function() {
  tagList(
    tags$style(HTML("
      #nl_search_bar {
        background:#ffffff; border:1px solid #e5e7eb; border-radius:8px;
        padding:10px 16px; margin-bottom:16px;
        display:flex; align-items:center; gap:12px;
      }
      #nl_search_bar.nl-disabled { background:#f3f4f6; opacity:0.65; cursor:not-allowed; }
      #nl_search_bar.nl-disabled input,
      #nl_search_bar.nl-disabled label { pointer-events:none; cursor:not-allowed; }
      #nl_search_bar.nl-disabled .nl-coming-soon {
        font-size:11px; color:#6b7280; font-style:italic; margin-left:8px; white-space:nowrap;
      }
      #nl_search_bar .nl-icon { color:#9ca3af; font-size:18px; }
      #nl_search_bar input.nl-input {
        flex:1; border:none; background:transparent; outline:none;
        color:#111827; font-size:14px;
      }
      #nl_search_bar .nl-scope-toggle {
        font-size:12px; color:#374151;
      }
      #nl_search_bar .nl-spinner {
        color:#6b7280; font-size:12px; font-style:italic;
      }
      #nl_answer_card {
        background:#ffffff; border:1px solid #e5e7eb; border-radius:8px;
        padding:16px 18px; margin-bottom:16px;
      }
      /* Hide the uiOutput placeholder when the server hasn't rendered an
         answer yet; otherwise the empty wrapper shows up as a blank white
         bar under the search input. */
      #nl_answer_card:empty { display:none; }
      #nl_answer_card .nl-headline {
        font-weight:700; font-size:15px; color:#111827; margin-bottom:12px;
      }
      #nl_answer_card .nl-footer {
        color:#6b7280; font-size:11px; margin-top:10px;
      }
      #nl_answer_card .nl-close {
        float:right; cursor:pointer; color:#9ca3af; font-size:14px;
      }
      #nl_answer_card.nl-fallback .nl-headline { color:#6b7280; }
    ")),
    div(id = "nl_search_bar", class = "nl-disabled",
      tags$i(class = "fa fa-magnifying-glass nl-icon"),
      # Use Shiny's textInput so input$nl_query is populated. Inline
      # styling collapses the default wrapper margin.
      div(style = "flex:1; margin:0;",
        tags$style(HTML("#nl_search_bar .form-group { margin:0; }
                         #nl_search_bar #nl_query { border:none; background:transparent;
                         box-shadow:none; padding:0; font-size:14px; }")),
        tagAppendAttributes(
          textInput("nl_query", label = NULL,
                    placeholder = "Ask anything about your portfolio",
                    width = "100%"),
          disabled = NA
        )
      ),
      uiOutput("nl_spinner", inline = TRUE)
    ),
    uiOutput("nl_answer_card")
  )
}

# ---- Answer card rendering ------------------------------------------------

nl_answer_card <- function(answer) {
  if (is.null(answer) || identical(answer$confidence, "low")) {
    return(nl_fallback_card(answer))
  }
  res <- answer$result
  chart <- nl_render_chart(res)
  div(id = "nl_answer_card",
    tags$span(class = "nl-close", onclick = "Shiny.setInputValue('nl_dismiss', Math.random(), {priority:'event'});", "✕"),
    div(class = "nl-headline", res$headline),
    chart,
    div(class = "nl-footer",
      sprintf("Based on %s · %s loans · computed %s",
              answer$scope_label,
              format(answer$n, big.mark = ","),
              format(Sys.time(), "%H:%M:%S")))
  )
}

nl_fallback_card <- function(answer) {
  tab_id <- answer$tab_id %||% ""
  examples <- nl_canonical_questions(tab_id)
  shown <- if (length(examples) >= 3) sample(examples, 3) else examples
  div(id = "nl_answer_card", class = "nl-fallback",
    tags$span(class = "nl-close", onclick = "Shiny.setInputValue('nl_dismiss', Math.random(), {priority:'event'});", "✕"),
    div(class = "nl-headline", "I'm not sure I understood. Try asking something like:"),
    tags$ul(lapply(shown, function(q) tags$li(tags$em(q)))),
    div(class = "nl-footer", "Or use the filters above.")
  )
}

# ---- Mini chart dispatch --------------------------------------------------

nl_render_chart <- function(result) {
  switch(result$chart_type,
    bar_horizontal = .nl_chart_bar_horizontal(result),
    bar_grouped    = .nl_chart_bar_grouped(result),
    donut          = .nl_chart_donut(result),
    table          = .nl_chart_table(result),
    scatter        = .nl_chart_scatter(result),
    div("(chart unavailable)")
  )
}

.nl_chart_bar_horizontal <- function(result) {
  rows <- head(result$rows, 8)
  dim_col <- names(rows)[1]
  plotly::plot_ly(
    x = rows$value,
    y = reorder(rows[[dim_col]], rows$value),
    type = "bar", orientation = "h",
    marker = list(color = "#fb6e36"),
    hovertemplate = sprintf("%%{y}: %%{x:.1%%}<extra></extra>")
  ) |>
  plotly::layout(
    margin = list(l = 80, r = 20, t = 10, b = 30),
    xaxis  = list(tickformat = ".1%", title = ""),
    yaxis  = list(title = ""),
    height = 240
  ) |>
  plotly::config(displayModeBar = FALSE)
}

.nl_chart_bar_grouped <- function(result) {
  rows <- result$rows
  plotly::plot_ly(
    x = rows$period, y = rows$value, type = "bar",
    marker = list(color = "#fb6e36"),
    hovertemplate = "%{x}: %{y:.1%}<extra></extra>"
  ) |>
  plotly::layout(
    margin = list(l = 40, r = 20, t = 10, b = 30),
    yaxis  = list(tickformat = ".1%", title = ""),
    xaxis  = list(title = ""),
    height = 240
  ) |>
  plotly::config(displayModeBar = FALSE)
}

.nl_chart_donut <- function(result) {
  rows <- result$rows
  plotly::plot_ly(
    labels = rows$bucket, values = rows$n, type = "pie", hole = 0.55,
    marker = list(colors = c("#fb6e36", "#d1d5db")),
    textinfo = "label+percent"
  ) |>
  plotly::layout(
    margin = list(l = 20, r = 20, t = 10, b = 10),
    showlegend = FALSE,
    height = 240
  ) |>
  plotly::config(displayModeBar = FALSE)
}

.nl_chart_table <- function(result) {
  rows <- result$rows
  if ("value" %in% names(rows)) {
    rows$value <- ifelse(abs(rows$value) <= 1,
                         sprintf("%.2f%%", rows$value * 100),
                         format(rows$value, big.mark = ","))
  }
  DT::datatable(rows, options = list(dom = "t", pageLength = 10, ordering = FALSE),
                rownames = FALSE, selection = "none")
}

.nl_chart_scatter <- function(result) {
  rows <- result$rows
  metric_cols <- setdiff(names(rows), c(names(rows)[1], "composite_score", "n"))
  if (length(metric_cols) < 2) return(.nl_chart_table(result))
  dim_col <- names(rows)[1]
  plotly::plot_ly(
    x = rows[[metric_cols[1]]], y = rows[[metric_cols[2]]],
    text = rows[[dim_col]], type = "scatter", mode = "markers+text",
    textposition = "top right",
    marker = list(color = ifelse(seq_len(nrow(rows)) == 1, "#fb6e36", "#9ca3af"),
                  size  = 12)
  ) |>
  plotly::layout(
    margin = list(l = 50, r = 20, t = 10, b = 40),
    xaxis = list(title = metric_cols[1]),
    yaxis = list(title = metric_cols[2]),
    height = 260
  ) |>
  plotly::config(displayModeBar = FALSE)
}

# ---- Canonical questions (tab-keyed, Task 16) -----------------------------

nl_canonical_questions <- function(tab_id) {
  switch(tab_id,
    smart_rec = c(
      "Which channel has the highest bad rate?",
      "What share of my book has 3+ hard inquiries?",
      "Compare 2026 Q1 vs 2025 Q4 vintage bad rate.",
      "Show the 5 segments with the worst IRR.",
      "Which segment has min IRR and max delinquency?"
    ),
    opp_find = c(
      "Which FICO band has the highest decline rate?",
      "Which segment has the worst bad rate?",
      "Show the 3 FICO bands with the lowest take rate.",
      "What share of issued loans have 3+ hard inquiries?",
      "Compare 2026 Q1 vs 2025 Q4 bad rate for this segment."
    ),
    risk_driver = c(
      "Which risk grade has the highest delinquency rate?",
      "What share of the book has 3+ hard inquiries?",
      "Which channel has the worst bad rate?",
      "Show the top 5 grades by delinquency.",
      "Which grade has min IRR and max delinquency?"
    ),
    whatif_tab = c(
      "Which channel has the highest bad rate under this policy?",
      "What share of approved loans have 3+ hard inquiries?",
      "Show the 5 grades with the worst IRR under this policy.",
      "Compare bad rate on 2026 Q1 vs 2025 Q4 for approved loans.",
      "Which segment has min IRR and max delinquency?"
    ),
    c("Which channel has the highest bad rate?",
      "What share of my book has 3+ hard inquiries?")
  )
}
