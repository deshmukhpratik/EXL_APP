# ============================================================
# R/ui/icons.R — Inline SVG icon registry
#
# Use icon_svg("name") instead of shiny::icon() or Font Awesome.
# All icons are stroke-only SVG at 16×16 viewBox.
# Add new icons by extending the .icon_registry list below.
# ============================================================

.icon_registry <- list(
  # Sidebar / navigation
  home     = '<circle cx="8" cy="9" r="1.2" fill="currentColor"/><path d="M2 8 L8 2 L14 8 V14 H2 Z"/>',
  catalog  = '<rect x="2" y="3" width="12" height="10" rx="1"/><line x1="5" y1="6" x2="11" y2="6"/><line x1="5" y1="9" x2="11" y2="9"/><line x1="5" y1="12" x2="9" y2="12"/>',
  scan     = '<circle cx="7" cy="7" r="4"/><line x1="10" y1="10" x2="14" y2="14"/>',
  explorer = '<circle cx="8" cy="8" r="6"/><circle cx="8" cy="8" r="2" fill="currentColor"/>',
  drivers  = '<line x1="3" y1="13" x2="3" y2="7"/><line x1="7" y1="13" x2="7" y2="4"/><line x1="11" y1="13" x2="11" y2="9"/><line x1="2" y1="13" x2="14" y2="13"/>',
  whatif   = '<polyline points="3,10 6,7 9,11 13,4"/><circle cx="3" cy="10" r="1" fill="currentColor"/><circle cx="13" cy="4" r="1" fill="currentColor"/>',
  irr      = '<path d="M3 13 L6 6 L9 9 L13 3"/><text x="8" y="14" font-size="4" text-anchor="middle" fill="currentColor" stroke="none">$</text>',
  history  = '<circle cx="8" cy="8" r="6"/><polyline points="8,4 8,8 10,10"/>',
  upload   = '<path d="M8 2 V10"/><polyline points="5,5 8,2 11,5"/><line x1="3" y1="13" x2="13" y2="13"/>',

  # Action / state
  bolt     = '<polygon points="9,2 4,9 8,9 7,14 12,7 8,7"/>',
  download = '<path d="M8 2 V10"/><polyline points="5,7 8,10 11,7"/><line x1="3" y1="13" x2="13" y2="13"/>',
  external = '<path d="M10 2 H14 V6"/><line x1="14" y1="2" x2="8" y2="8"/><path d="M12 10 V13 H3 V4 H6"/>',
  sparkle  = '<path d="M8 2 L9 6 L13 8 L9 10 L8 14 L7 10 L3 8 L7 6 Z" fill="currentColor"/>',
  check    = '<polyline points="3,8 7,12 13,4"/>',
  warn     = '<path d="M8 2 L14 13 H2 Z"/><line x1="8" y1="7" x2="8" y2="10"/><circle cx="8" cy="12" r="0.6" fill="currentColor"/>',
  close    = '<line x1="4" y1="4" x2="12" y2="12"/><line x1="12" y1="4" x2="4" y2="12"/>'
)

#' Render an inline SVG icon by name
#'
#' @param name Icon name from .icon_registry
#' @param size Pixel size (default 16)
#' @param color CSS color value (default "currentColor", inherits from parent)
#' @param title Optional accessible title
icon_svg <- function(name, size = 16, color = "currentColor", title = NULL) {
  body <- .icon_registry[[name]]
  if (is.null(body)) {
    warning("icon_svg: unknown icon name '", name, "'")
    return(NULL)
  }
  title_tag <- if (!is.null(title)) sprintf("<title>%s</title>", title) else ""
  shiny::HTML(sprintf(
    '<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 16 16" fill="none" stroke="%s" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">%s%s</svg>',
    size, size, color, title_tag, body
  ))
}
