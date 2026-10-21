# ============================================================
# R/ui/components.R — Component library
#
# The only place tab files obtain visual building blocks.
# Rules:
#   - No hardcoded hex colors (all use CSS classes from app.css)
#   - No inline tags$style (all styling via classes)
#   - Every function returns a shiny.tag or shiny.tag.list
#   - Function names are snake_case and nouns where possible
# ============================================================

# NULL-coalesce operator used throughout
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# ---- Scaffolding: topbar / sidebar_group -----------------------------------

#' Topbar — dark brand strip with optional product pill and export button
#'
#' @param brand Brand text (e.g. "CREDIT POLICY")
#' @param crumb Optional breadcrumb list: c("Policy Engine", "Opportunity Explorer")
#'              Last element is rendered highlighted.
#' @param status Optional status text (e.g. "PL-CURRENT · LIVE")
#' @param show_product_switcher If TRUE, renders the product pill (default TRUE)
#' @param show_export_button If TRUE, renders the Export button (default FALSE — enabled in Week 5)
topbar <- function(brand = "CREDIT POLICY",
                   crumb = NULL,
                   status = NULL,
                   show_product_switcher = TRUE,
                   show_export_button    = FALSE) {
  crumb_html <- if (!is.null(crumb) && length(crumb) > 0) {
    parts <- list()
    for (i in seq_along(crumb)) {
      if (i < length(crumb)) {
        parts <- c(parts, list(shiny::span(crumb[[i]]),
                               shiny::span(class = "cpt-crumb-sep", " \u203a ")))
      } else {
        parts <- c(parts, list(shiny::span(class = "cpt-crumb-cur", crumb[[i]])))
      }
    }
    do.call(shiny::div, c(list(class = "cpt-crumb"), parts))
  }

  right_items <- shiny::tagList(
    if (show_product_switcher) {
      shiny::div(
        class = "cpt-product-pill",
        id = "topbar-product-pill",
        shiny::textOutput("topbar_product", inline = TRUE),
        shiny::tags$span(style = "margin-left:6px;", "\u25be")
      )
    },
    if (!is.null(status)) shiny::div(class = "cpt-status", status),
    if (show_export_button) {
      shiny::actionButton(
        "topbar_export",
        label = shiny::tagList(icon_svg("download"), " Export report"),
        class = "cpt-export-btn"
      )
    }
  )

  shiny::div(
    class = "cpt-topbar",
    shiny::div(
      class = "cpt-brand",
      shiny::div(class = "cpt-brand-dot"),
      brand
    ),
    crumb_html,
    shiny::div(class = "cpt-topbar-right", right_items)
  )
}

#' Sidebar group — labelled section with list of items
#'
#' @param label Section label (e.g. "01 · DISCOVER") — NULL for no label
#' @param items List of list(name, tab_id, icon, active) — active TRUE highlights
sidebar_group <- function(label, items) {
  shiny::div(
    class = "cpt-sb-group",
    if (!is.null(label)) shiny::div(class = "cpt-sb-label", label),
    lapply(items, function(it) {
      cls <- if (isTRUE(it$active)) "cpt-sb-item active" else "cpt-sb-item"
      shiny::tags$a(
        class = cls,
        `data-tab-id` = it$tab_id,
        href = paste0("#", it$tab_id),
        shiny::div(class = "cpt-sb-icon", icon_svg(it$icon %||% "home", size = 14)),
        it$name
      )
    })
  )
}

# ---- Section-level: section_header / filter_strip --------------------------

#' Serif section header with optional subtitle and mono meta tags
#'
#' @param title Main title (renders in serif)
#' @param subtitle Optional one-line description (sans-serif)
#' @param meta_tags Optional character vector of small tag strings, rendered on the right
section_header <- function(title, subtitle = NULL, meta_tags = NULL) {
  shiny::div(
    class = "cpt-section-header",
    shiny::div(
      class = "cpt-section-header-text",
      shiny::h1(class = "cpt-section-title", title),
      if (!is.null(subtitle)) shiny::p(class = "cpt-section-sub", subtitle)
    ),
    if (!is.null(meta_tags) && length(meta_tags) > 0) {
      shiny::div(
        class = "cpt-section-meta",
        lapply(meta_tags, function(t) shiny::span(class = "cpt-tag", t))
      )
    }
  )
}

#' Filter strip — label + chips (buttons) + optional action button on the right
#'
#' @param label Uppercase label on the left (e.g. "Objective")
#' @param chips List of list(value, label, active) or a shiny input (pickerInput etc.)
#' @param action_button Optional actionButton on the right side
filter_strip <- function(label = NULL, chips = NULL, action_button = NULL, ...) {
  extra <- list(...)

  chip_tags <- if (!is.null(chips)) {
    if (inherits(chips, "shiny.tag") || inherits(chips, "shiny.tag.list")) {
      chips
    } else {
      lapply(chips, function(c) {
        cls <- if (isTRUE(c$active)) "cpt-chip active" else "cpt-chip"
        shiny::tags$button(
          class = cls,
          type = "button",
          `data-chip-value` = c$value,
          c$label
        )
      })
    }
  }

  shiny::div(
    class = "cpt-filter-strip",
    if (!is.null(label)) shiny::span(class = "cpt-filter-label", label),
    chip_tags,
    extra,
    shiny::div(class = "cpt-filter-spacer"),
    action_button
  )
}

# ---- KPI cards: kpi_card / featured_kpi ------------------------------------

#' Standard KPI card — white, mono value, optional delta, optional benchmark
#'
#' @param value The headline value (character; pre-formatted)
#' @param label Uppercase label shown above value
#' @param delta Optional delta text (e.g. "+42 bps vs Q3")
#' @param delta_dir One of "up", "down", "flat"
#' @param benchmark Optional benchmark text (e.g. "vs. industry 11.2%")
kpi_card <- function(value, label, delta = NULL, delta_dir = c("up", "down", "flat"),
                     benchmark = NULL) {
  delta_dir <- match.arg(delta_dir)
  shiny::div(
    class = "cpt-kpi",
    shiny::div(class = "cpt-kpi-label", label),
    shiny::div(class = "cpt-kpi-val", value),
    if (!is.null(delta)) {
      shiny::div(
        class = paste("cpt-kpi-delta", delta_dir),
        delta
      )
    },
    if (!is.null(benchmark)) {
      shiny::div(class = "cpt-kpi-benchmark", benchmark)
    }
  )
}

#' Featured KPI — the one hero dark gradient card per view
#'
#' Identical API to kpi_card() but renders in the featured dark style.
#' Convention: use exactly one per view. Enforcement is by code review.
featured_kpi <- function(value, label, delta = NULL, delta_dir = c("up", "down", "flat"),
                         benchmark = NULL) {
  delta_dir <- match.arg(delta_dir)
  shiny::div(
    class = "cpt-kpi featured",
    shiny::div(class = "cpt-kpi-label", label),
    shiny::div(class = "cpt-kpi-val", value),
    if (!is.null(delta)) {
      shiny::div(
        class = paste("cpt-kpi-delta", delta_dir),
        delta
      )
    },
    if (!is.null(benchmark)) {
      shiny::div(class = "cpt-kpi-benchmark", benchmark)
    }
  )
}

# ---- Panels: panel_card / chart_card ---------------------------------------

#' Generic white card with optional title/subtitle and arbitrary body
#'
#' Replaces shinydashboard's box().
panel_card <- function(title = NULL, subtitle = NULL, ...) {
  shiny::div(
    class = "cpt-card",
    if (!is.null(title)) shiny::h3(class = "cpt-card-title", title),
    if (!is.null(subtitle)) shiny::div(class = "cpt-card-sub", subtitle),
    ...
  )
}

#' Chart card — panel_card specialization wrapping a plotOutput
#'
#' @param title Card title
#' @param subtitle Optional subtitle
#' @param plot_output_id The Shiny output ID for the plot
#' @param plot_type One of "plotly" (default) or "base" (renderPlot)
#' @param height CSS height passed to the plot output (default "280px")
chart_card <- function(title, subtitle = NULL, plot_output_id,
                       plot_type = c("plotly", "base"), height = "280px") {
  plot_type <- match.arg(plot_type)
  plot_out <- if (plot_type == "plotly") {
    plotly::plotlyOutput(plot_output_id, height = height)
  } else {
    shiny::plotOutput(plot_output_id, height = height)
  }
  panel_card(
    title = title,
    subtitle = subtitle,
    plot_out
  )
}

# ---- Segments + badges + AI caption + error card --------------------------

#' Soft pill badge (alert / warn / ok / neutral / info)
soft_badge <- function(text, status = c("alert", "warn", "ok", "neutral", "info")) {
  status <- match.arg(status)
  shiny::span(class = paste("cpt-badge", status), text)
}

#' AI-generated caption with sparkle icon
#'
#' Shows sparkle only when ai_generated = TRUE. When FALSE (offline fallback),
#' renders as a regular caption — looks natural, no broken placeholder.
#' If text is NULL or empty, renders nothing at all.
ai_caption <- function(text, ai_generated = TRUE) {
  if (is.null(text) || !nzchar(text)) return(NULL)
  shiny::div(
    class = "cpt-ai-caption",
    if (isTRUE(ai_generated)) {
      shiny::span(class = "cpt-ai-sparkle", icon_svg("sparkle", size = 12))
    },
    shiny::span(text)
  )
}

#' Flagged segment row — colored left border + rule + metrics + optional badge + AI caption
#'
#' @param rule_text Mono-formatted rule (e.g. "FICO in [620,660] & DTI > 0.45")
#' @param metrics A named list: list(n=412, irr="6.2%", loss="9.1%")
#' @param status One of "alert", "border", "ok", "neutral"
#' @param badge_text Optional badge label (defaults to capitalized status)
#' @param ai_explanation Optional AI-generated explanation paragraph
segment_row <- function(rule_text, metrics, status = c("alert", "border", "ok", "neutral"),
                        badge_text = NULL, ai_explanation = NULL) {
  status <- match.arg(status)
  badge_text <- badge_text %||% switch(status,
    alert = "Alert",
    border = "Border",
    ok = "Healthy",
    neutral = "Info"
  )
  badge_status <- switch(status, alert = "alert", border = "warn", ok = "ok", neutral = "neutral")

  metrics_html <- lapply(names(metrics), function(nm) {
    shiny::span(sprintf("%s=", nm), shiny::tags$strong(metrics[[nm]]))
  })

  shiny::div(
    class = paste("cpt-segment-row", status),
    shiny::div(
      class = "cpt-segment-head",
      shiny::span(class = "cpt-segment-rule", rule_text),
      soft_badge(badge_text, status = badge_status)
    ),
    do.call(shiny::div, c(list(class = "cpt-segment-metrics"), metrics_html)),
    if (!is.null(ai_explanation) && nzchar(ai_explanation)) {
      ai_caption(ai_explanation, ai_generated = TRUE)
    }
  )
}

#' Graceful error card — replaces red .shiny-output-error boxes
#'
#' Rendered by safe_reactive() wrapper on uncaught exceptions.
error_card <- function(title = "Something went wrong rendering this section",
                       body  = "The rest of the tab is still usable.",
                       action_label = "Reload tab",
                       action_id = NULL) {
  shiny::div(
    class = "cpt-error-card",
    shiny::div(
      class = "cpt-error-title",
      icon_svg("warn", size = 14),
      title
    ),
    shiny::div(class = "cpt-error-body", body),
    if (!is.null(action_id)) {
      shiny::actionButton(action_id, label = action_label, class = "cpt-btn-primary",
                          style = "margin-top: 8px;")
    }
  )
}

# ---- Tables + SQL modal stub -----------------------------------------------

#' Themed DT::datatable wrapper
#'
#' @param df A data.frame
#' @param ... Additional args passed to DT::datatable
data_table <- function(df, ...) {
  DT::datatable(
    df,
    rownames = FALSE,
    class = "compact stripe hover row-border cpt-datatable",
    options = list(
      dom = "tip",
      pageLength = 10,
      lengthChange = FALSE
    ),
    ...
  )
}

#' SQL preview modal — the Discovery 1 surfacing of sql_policy_generator.R.
#'
#' Shows the generated L10–L90 CTE waterfall SQL in a wide modal with a
#' Copy-to-clipboard button (client-side, no server round-trip) and a
#' Download button wired to `downloadHandler`. Tabs call this via
#' `showModal(sql_modal(sql_text, ...))`.
#'
#' @param sql_text Character(1) SQL string
#' @param title Modal title (e.g. policy id + name)
#' @param filename Suggested download filename (hint for the Copy button, the
#'        actual download uses the downloadHandler bound to id `cpt_sql_dl`)
sql_modal <- function(sql_text, title = "Generated Policy SQL",
                      filename = "policy.sql") {
  # Escape the SQL for safe embedding in the JS template literal
  js_safe <- gsub("`", "\\\\`", sql_text)
  js_safe <- gsub("\\$\\{", "\\\\${", js_safe)

  copy_script <- sprintf(
    "(function(){ var t = `%s`; navigator.clipboard.writeText(t).then(function(){ var b = document.getElementById('cpt_sql_copy_btn'); if(b){ var orig = b.innerText; b.innerText = 'Copied'; setTimeout(function(){ b.innerText = orig; }, 1200); } }); })()",
    js_safe
  )

  shiny::modalDialog(
    title = shiny::div(class = "cpt-sql-modal-title", title),
    size = "l",
    easyClose = TRUE,
    footer = shiny::tagList(
      shiny::tags$button(
        id = "cpt_sql_copy_btn",
        class = "cpt-btn-secondary",
        type = "button",
        onclick = copy_script,
        "Copy"
      ),
      shiny::downloadButton("cpt_sql_dl", label = "Download .sql",
                            class = "cpt-btn-secondary"),
      shiny::modalButton("Close")
    ),
    shiny::div(
      class = "cpt-sql-meta",
      shiny::span(class = "cpt-sql-meta-label", "Generated "),
      shiny::span(as.character(Sys.Date())),
      shiny::span(class = "cpt-sql-meta-sep", "\u00b7"),
      shiny::span(nchar(sql_text), " chars")
    ),
    shiny::tags$pre(
      class = "cpt-sql-code",
      shiny::tags$code(class = "language-sql", sql_text)
    )
  )
}

# ---- Legacy UI components migrated from 02b_globals_utils.R ----

# Product code to readable label
product_mapping <- c(
  "PL"   = "Personal Loan - Prime",
  "AL"   = "Auto Loan",
  "CC"   = "Credit Card",
  "PF"   = "Project Finance",
  "Auto" = "Auto Loan",
  "TU"   = "Top-Up",
  "MTG"  = "Mortgage"
)

# Reverse mapping: full name to code (for tabs that use product codes internally).
# Both "Personal Loan - Prime" and "Personal Loan - Near Prime" route to the same
# underlying PL data — Near Prime is a UI-level facade so the demo can show
# multi-product coverage without splitting the loan tape.
product_name_to_code <- c(
  "Personal Loan - Prime"      = "PL",
  "Personal Loan - Near Prime" = "PL",
  "Personal Loan"              = "PL",
  "Top-Up"                     = "TU",
  "Credit Card"                = "CC",
  "Mortgage"                   = "MTG"
)

# Shared product choices
PRODUCT_CHOICES <- c("Personal Loan - Prime", "Personal Loan - Near Prime",
                     "Top-Up", "Credit Card", "Mortgage")

# Dark KPI box (matches original design language)
kpi_box <- function(value, label, sub = NULL) {
  div(
    style = paste0(
      "background:#1f2937; border-radius:8px; padding:12px 8px; color:white;",
      " text-align:center; margin-bottom:10px; min-height:90px;"
    ),
    div(style = "font-size:18px; font-weight:bold; color:#fb6e36;", value),
    div(style = "font-size:10px; color:#9ca3af; margin-top:3px;", label),
    if (!is.null(sub)) div(style = "font-size:9px; color:#6b7280; margin-top:2px;", sub)
  )
}

# Shared product selector (consistent switch-btn pattern)
product_selector_ui <- function(prefix, default = "Personal Loan - Prime") {
  buttons <- lapply(PRODUCT_CHOICES, function(prod) {
    clean_id  <- paste0("btn-prod-", prefix, "-",
                        gsub("[^a-zA-Z0-9]", "-", tolower(prod)))
    is_active <- (prod == default)
    tags$button(prod, type = "button",
      class = if (is_active) "switch-btn active" else "switch-btn inactive",
      `data-value` = prod, id = clean_id)
  })

  container_id <- paste0("product-buttons-", prefix)
  input_id     <- paste0(prefix, "_product")

  tagList(
    tags$div(
      style = "display:block; text-align:center; width:100%; margin-bottom:10px;",
      tags$p("Select Product",
        style = "font-size:100%; font-weight:bold; margin-bottom:5px; color:#f97316;"),
      tags$div(id = container_id, class = "switch-group",
        do.call(tagList, buttons))
    ),
    tags$script(HTML(sprintf("
      $(document).on('click', '#%s .switch-btn', function() {
        $('#%s .switch-btn').removeClass('active').addClass('inactive');
        $(this).addClass('active').removeClass('inactive');
        Shiny.setInputValue('%s', $(this).data('value'));
      });
      $(function() {
        if (window.Shiny && Shiny.setInputValue) {
          Shiny.setInputValue('%s', '%s', {priority: 'event'});
        }
      });
    ", container_id, container_id, input_id, input_id, default)))
  )
}

# ---- active_policy_card_ui: shared "Current Active Policy" banner ----------
# Renders an orange banner (matching the Policy Repository banner) that reflects
# the currently active policy for the selected product. Each tab supplies its
# own prefix (e.g. "opp", "wi", "smart") so the output slot is unique.
active_policy_card_ui <- function(prefix) {
  fluidRow(column(12,
    div(style = "margin-bottom:10px;",
      uiOutput(paste0(prefix, "_active_policy_card"))
    )
  ))
}

# ---- render_active_policy_card: server-side renderer ----------------------
# Resolves the active policy row for the given product by combining
# `mockPolicies` with the user-saved `added_policy()` store and applying any
# `status_overrides` map (policyId -> status). Returns the rendered banner or
# a subdued "no active policy" placeholder when nothing matches.
render_active_policy_card <- function(product, added_policy_df, status_overrides = NULL) {
  if (is.null(product) || !nzchar(product)) return(NULL)
  # Product may be a code ("PL") or a full name ("Personal Loan"); normalise
  # to the internal code used by mockPolicies$product.
  prod_code <- if (product %in% names(product_name_to_code)) {
    unname(product_name_to_code[product])
  } else {
    product
  }
  df <- mockPolicies
  if (!is.null(added_policy_df) && nrow(added_policy_df) > 0) {
    df <- dplyr::bind_rows(df, added_policy_df)
  }
  if (!is.null(status_overrides) && length(status_overrides) > 0) {
    ids <- names(status_overrides)
    m <- match(df$policyId, ids)
    df$status[!is.na(m)] <- unlist(status_overrides)[m[!is.na(m)]]
  }
  df <- df[!is.na(df$product) & df$product == prod_code, , drop = FALSE]
  active <- df[tolower(as.character(df$status)) == "active", , drop = FALSE]
  if (nrow(active) == 0) {
    return(div(
      style = paste0(
        "background-color:#374151; color:#d1d5db; padding:12px 15px;",
        " border-radius:8px; font-size:13px;"
      ),
      span("No active policy for this product.",
        style = "font-weight:500;")
    ))
  }
  info <- active[1, , drop = FALSE]
  eff <- if ("effective_from" %in% names(info)) as.Date(info$effective_from[[1]]) else as.Date(NA)
  upd <- if ("updatedAt"      %in% names(info)) as.Date(info$updatedAt[[1]])      else as.Date(NA)
  eff_txt <- if (is.na(eff)) "Effective date not set" else paste("Effective", format(eff, "%Y-%m-%d"))
  upd_txt <- if (is.na(upd)) "\u2014" else format(upd, "%Y-%m-%d")
  div(
    style = paste0(
      "background-color:#FF6B35; color:white; padding:15px; border-radius:8px;",
      " margin:0; display:flex; justify-content:space-between; align-items:center;"
    ),
    div(style = "display:flex; flex-direction:column;",
      span("Current Active Policy", style = "font-weight:500; font-size:14px;"),
      div(style = "display:flex; gap:10px; margin-top:5px;",
        span(info$policyId,   style = "font-weight:500;"),
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
}

# ---- wire_active_policy_card: convenience server wrapper -------------------
# Call this from a tab's server to render the active-policy banner into the
# slot created by active_policy_card_ui(prefix). Reads shared reactives from
# session$userData (added_policy, policy_status_overrides).
wire_active_policy_card <- function(input, output, session, prefix) {
  product_input <- paste0(prefix, "_product")
  slot_id       <- paste0(prefix, "_active_policy_card")
  output[[slot_id]] <- renderUI({
    prod <- input[[product_input]]
    # product_selector_ui sets the input via async JS after DOMReady; until
    # that fires, input is NULL and the banner would render blank. Fall back
    # to the default "Personal Loan" so the card is populated on first paint.
    if (is.null(prod) || !nzchar(prod)) prod <- "Personal Loan - Prime"
    ap <- session$userData$added_policy
    so <- session$userData$policy_status_overrides
    render_active_policy_card(
      product            = prod,
      added_policy_df    = if (!is.null(ap)) ap() else NULL,
      status_overrides   = if (!is.null(so)) so() else NULL
    )
  })
}

# Header panel HTML generator.
# `icon_name` (preferred) — Font Awesome icon name shown in an orange circular
#   badge to the left of the title. Renders crisp at any zoom.
# `img_src` (legacy fallback) — path to a bitmap icon. Kept for tabs that
#   haven't been migrated yet.
# `data_scope` — accepted for backward compatibility but no longer renders
#   a pill (per CRO feedback: scope is communicated via the per-tab
#   Applications/Issued radio toggle, not a header badge).
header_panel <- function(title, subtitle, img_src = NULL, icon_name = NULL,
                         data_scope = NULL) {
  scope_pill <- NULL

  badge_el <- if (!is.null(icon_name)) {
    tags$span(
      style = paste0(
        "display:inline-flex; align-items:center; justify-content:center;",
        " width:34px; height:34px; border-radius:50%;",
        " background:#fb6e36; color:white; margin-right:12px;",
        " vertical-align:middle; box-shadow:0 1px 3px rgba(0,0,0,0.25);"
      ),
      icon(icon_name, style = "font-size:15px;")
    )
  } else if (!is.null(img_src)) {
    tags$img(src = img_src,
             style = "height:32px; vertical-align:middle; margin-right:10px; filter:invert(1); opacity:0.9;")
  } else {
    NULL
  }

  title_el <- h4(
    badge_el,
    HTML(paste0("<b>", title, "</b>")),
    style = "color:#fff; margin:0 0 4px;"
  )

  fluidRow(column(12,
    wellPanel(
      style = paste0(
        "background-color:#222D32; color:#fff; border-radius:10px;",
        " text-align:center; padding:12px; margin-bottom:15px;"
      ),
      title_el,
      p(subtitle, style = "color:#d9e3ea; margin:0; font-size:14px;"),
      scope_pill
    )
  ))
}

# ---- scope_toggle_ui: Applications / Issued radio for a tab ----------------
#
# Shared UI component so every tab (except SMART Risk Scanner) offers the
# same Applications/Issued scope switch. Wiring is per-tab: read
# input[[input_id]] and pass it into get_scoped_data().
#
# @param input_id  Input id for the radioButtons (must be unique per tab).
# @param selected  Default selection ("issued" or "applications").
scope_toggle_ui <- function(input_id, selected = "issued") {
  div(style = paste0("background:#f3f4f6; padding:10px 15px; border-radius:6px;",
                     " margin-bottom:12px; display:flex; align-items:center; gap:16px;"),
      tags$b("Data scope:"),
      radioButtons(input_id, label = NULL,
                   choices = c("Applications" = "applications",
                               "Issued"       = "issued"),
                   selected = selected, inline = TRUE),
      tags$span(style = "color:#6b7280; font-size:12px; margin-left:auto;",
                "Applications = full funnel  \u00b7  Issued = approved loans only"))
}
