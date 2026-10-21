# ============================================================
# R/ui/skeletons.R — Shimmer loading placeholders
#
# These match final component geometry so there's no layout
# jump when data arrives. Each returns a shiny.tag.
# ============================================================

#' Row of KPI-card shaped skeletons
#' @param n Number of cards (default 4)
skeleton_kpi_row <- function(n = 4) {
  shiny::div(
    class = "cpt-skeleton-row-wrap",
    style = paste0(
      "display: grid; grid-template-columns: repeat(", n,
      ", 1fr); gap: 12px; margin-bottom: 16px;"
    ),
    lapply(seq_len(n), function(i) {
      shiny::div(class = "cpt-skeleton cpt-skeleton-kpi")
    })
  )
}

#' Chart-sized shimmer block
#' @param height CSS height (default "140px")
skeleton_chart <- function(height = "140px") {
  shiny::div(
    class = "cpt-skeleton cpt-skeleton-chart",
    style = paste0("height: ", height, "; margin-bottom: 16px;")
  )
}

#' Table skeleton with N shimmer rows
#' @param rows Number of rows (default 8)
skeleton_table <- function(rows = 8) {
  shiny::div(
    style = "padding: 6px; background: white; border: 1px solid #e7e5e4; border-radius: 10px;",
    lapply(seq_len(rows), function(i) {
      shiny::div(class = "cpt-skeleton cpt-skeleton-row")
    })
  )
}

#' Generic panel skeleton — for when we don't know the shape yet
skeleton_panel <- function(height = "200px") {
  shiny::div(
    class = "cpt-skeleton",
    style = paste0("height: ", height, "; border-radius: 10px;")
  )
}

#' Recommendation-card skeleton — matches the Auto Scan + Explorer card
#' geometry (rule line, metrics strip, AI caption, action button) so the
#' layout doesn't jump when real results land.
#' @param n Number of card placeholders (default 5)
skeleton_rec_cards <- function(n = 5) {
  shiny::div(
    class = "cpt-skeleton-rec-wrap",
    lapply(seq_len(n), function(i) {
      shiny::div(
        class = "cpt-rec-card cpt-skeleton-rec-card",
        shiny::div(class = "cpt-skeleton cpt-skeleton-rec-rule"),
        shiny::div(
          class = "cpt-skeleton-rec-metrics",
          shiny::div(class = "cpt-skeleton cpt-skeleton-rec-metric"),
          shiny::div(class = "cpt-skeleton cpt-skeleton-rec-metric"),
          shiny::div(class = "cpt-skeleton cpt-skeleton-rec-metric"),
          shiny::div(class = "cpt-skeleton cpt-skeleton-rec-metric")
        ),
        shiny::div(class = "cpt-skeleton cpt-skeleton-rec-caption"),
        shiny::div(class = "cpt-skeleton cpt-skeleton-rec-btn")
      )
    })
  )
}

#' Guardrail-pill skeleton row (What-If Lab).
skeleton_guardrail_row <- function() {
  shiny::div(
    style = "display:flex; gap:10px; flex-wrap:wrap;",
    lapply(seq_len(3), function(i) {
      shiny::div(
        class = "cpt-guardrail-pill cpt-skeleton cpt-skeleton-guardrail"
      )
    })
  )
}
