# ============================================================
# R/ui/page_shell.R — Page shell
#
# Builds the bslib page_sidebar() with custom dark topbar, journey-
# grouped sidebar, and nav_hidden() holding all tab bodies. Tab
# navigation is driven by sidebar clicks that update input$cpt_tab.
# ============================================================

#' Build the full app UI
#'
#' @param tab_uis A named list of tab UI outputs: list(home = tab_home_ui(), ...)
#' @param theme The bs_theme() object from build_app_theme()
page_shell <- function(tab_uis, theme) {

  # Sidebar navigation groups (journey sections from the spec, Section 4)
  sidebar_content <- shiny::tagList(
    sidebar_group("Portfolio", list(
      list(name = "Portfolio Home", tab_id = "home",    icon = "home",    active = TRUE),
      list(name = "Policy Catalog", tab_id = "catalog", icon = "catalog")
    )),
    sidebar_group("01 \u00b7 Discover", list(
      list(name = "Auto Scan",       tab_id = "autoscan", icon = "scan"),
      list(name = "Guided Explorer", tab_id = "explorer", icon = "explorer"),
      list(name = "Risk Drivers",    tab_id = "drivers",  icon = "drivers")
    )),
    sidebar_group("02 \u00b7 Simulate", list(
      list(name = "What-If Lab",     tab_id = "whatif",   icon = "whatif")
    )),
    sidebar_group("03 \u00b7 Measure", list(
      list(name = "IRR Engine",      tab_id = "irr",      icon = "irr")
    )),
    sidebar_group("04 \u00b7 Govern", list(
      list(name = "Policy History",  tab_id = "history",  icon = "history")
    )),
    shiny::tags$hr(class = "cpt-sb-separator"),
    sidebar_group(NULL, list(
      list(name = "Data & Upload", tab_id = "upload", icon = "upload")
    ))
  )

  # Main body: topbar + nav_hidden holding tabs
  # Topbar is hand-rolled here (instead of calling topbar()) because the
  # breadcrumb and product-pill labels are reactive outputs — the server
  # fills them on tab-change and product-switcher-click.
  main_body <- shiny::div(
    class = "cpt-main",
    shiny::div(
      class = "cpt-topbar",
      shiny::div(
        class = "cpt-brand",
        shiny::div(class = "cpt-brand-dot"),
        "CREDIT POLICY"
      ),
      shiny::uiOutput("topbar_crumb", container = function(...)
        shiny::div(class = "cpt-crumb", ...)),
      shiny::div(
        class = "cpt-topbar-right",
        shiny::downloadLink(
          outputId = "topbar_export_btn",
          label = "\u2193 Export report",
          class = "cpt-topbar-export"
        ),
        shiny::tags$button(
          class = "cpt-product-pill",
          id = "topbar_product_pill_btn",
          type = "button",
          onclick = "Shiny.setInputValue('topbar_product_click', Math.random());",
          shiny::textOutput("topbar_product", inline = TRUE),
          shiny::tags$span(style = "margin-left:6px;", "\u25be")
        ),
        shiny::div(class = "cpt-status", "PL-CURRENT \u00b7 LIVE")
      )
    ),
    shiny::div(
      style = "padding: 28px 24px;",
      bslib::navset_hidden(
        id = "cpt_tab",
        tab_uis$home,
        tab_uis$catalog,
        tab_uis$autoscan,
        tab_uis$explorer,
        tab_uis$drivers,
        tab_uis$whatif,
        tab_uis$irr,
        tab_uis$history,
        tab_uis$upload
      )
    )
  )

  # The page_sidebar shell
  bslib::page_sidebar(
    theme = theme,
    title = NULL,  # We render our own topbar inside main instead
    window_title = "Credit Policy Tool v2",

    # Custom assets
    shiny::tags$head(
      shiny::tags$link(rel = "icon", type = "image/svg+xml",
                       href = "assets/favicon.svg"),
      shiny::tags$link(rel = "stylesheet", type = "text/css",
                       href = "assets/css/app.css"),
      shiny::tags$script(shiny::HTML("
        // ---- Sidebar clicks drive nav_hidden + active highlight ----
        function cptHighlightSidebar(tabId) {
          document.querySelectorAll('.cpt-sb-item').forEach(function(el){
            el.classList.remove('active');
            if (el.getAttribute('data-tab-id') === tabId) el.classList.add('active');
          });
        }
        document.addEventListener('click', function(e) {
          var a = e.target.closest('.cpt-sb-item');
          if (!a) return;
          e.preventDefault();
          var tabId = a.getAttribute('data-tab-id');
          if (!tabId) return;
          if (typeof Shiny !== 'undefined') {
            Shiny.setInputValue('cpt_tab', tabId);
          }
          cptHighlightSidebar(tabId);
        });

        // ---- Shiny-dependent setup ----
        // Shiny JS loads via deferred <script> tags, so it may not be
        // available when this inline script executes during HTML parsing.
        // DOMContentLoaded fires after deferred scripts, guaranteeing
        // that Shiny and $ are defined.
        document.addEventListener('DOMContentLoaded', function() {
          // Cross-tab seam: server-side code can trigger a tab switch via
          // session$sendCustomMessage('cpt_set_tab', list(name=..., value=...))
          if (typeof Shiny !== 'undefined') {
            Shiny.addCustomMessageHandler('cpt_set_tab', function(message) {
              var tabId = message.value;
              Shiny.setInputValue(message.name || 'cpt_tab', tabId);
              cptHighlightSidebar(tabId);
            });
          }

          // Any Shiny input change to cpt_tab (including setInputValue from
          // anywhere in the app) should sync the sidebar highlight.
          if (typeof $ !== 'undefined') {
            $(document).on('shiny:inputchanged', function(e) {
              if (e.name === 'cpt_tab' && typeof e.value === 'string') {
                cptHighlightSidebar(e.value);
              }
            });

            // App-load overlay: hide once Shiny finishes its first idle
            $(document).one('shiny:idle', function() {
              var ov = document.getElementById('cpt-loading-overlay');
              if (!ov) return;
              ov.classList.add('cpt-loading-hidden');
              setTimeout(function() { if (ov.parentNode) ov.parentNode.removeChild(ov); }, 600);
            });
          }

          // Fallback: if shiny:idle hasn't fired after 15s, remove overlay
          // (covers edge cases like slow initial reactive flush)
          setTimeout(function() {
            var ov = document.getElementById('cpt-loading-overlay');
            if (ov && !ov.classList.contains('cpt-loading-hidden')) {
              ov.classList.add('cpt-loading-hidden');
              setTimeout(function() { if (ov.parentNode) ov.parentNode.removeChild(ov); }, 600);
            }
          }, 15000);
        });

        // ---- Export report button: show 'Building...' toast on click ----
        // downloadLink fires the download in the background; the server
        // takes ~1s to render the HTML so users think nothing happened.
        // This gives a visible signal and auto-hides when the browser
        // actually serves the file.
        document.addEventListener('click', function(e) {
          var link = e.target.closest('#topbar_export_btn');
          if (!link) return;
          var toast = document.getElementById('cpt-export-toast');
          if (!toast) {
            toast = document.createElement('div');
            toast.id = 'cpt-export-toast';
            toast.className = 'cpt-export-toast';
            toast.innerHTML = '<span class=\"cpt-ai-spinner\"></span> Building report\u2026';
            document.body.appendChild(toast);
          }
          toast.classList.add('visible');
          setTimeout(function() { toast.classList.remove('visible'); }, 2400);
        });
      "))
    ),

    sidebar = bslib::sidebar(
      width = 240,
      bg = "#0f0d0b",
      padding = 0,
      class = "cpt-sidebar",
      sidebar_content
    ),

    # Initial app-load overlay — covers the screen until Shiny establishes
    # the websocket and the first reactive pass completes. Removed by the
    # JS handler in <head> on shiny:idle.
    shiny::div(
      id = "cpt-loading-overlay",
      class = "cpt-loading-overlay",
      shiny::div(
        class = "cpt-loading-card",
        shiny::div(class = "cpt-loading-brand", "CREDIT POLICY"),
        shiny::div(class = "cpt-loading-spinner"),
        shiny::div(class = "cpt-loading-text", "Loading 100K loans + 800K tape rows\u2026"),
        shiny::div(class = "cpt-loading-sub", "Building reactive graph and connecting to AI provider")
      )
    ),

    main_body
  )
}
