# ============================================================
# R/00_theme.R — Design tokens + bs_theme() + helpers
#
# Single source of truth for every color, font, spacing, and
# radius in the app. Change a value here, it changes everywhere.
# NEVER duplicate a token value elsewhere. NEVER hardcode a hex
# color in a tab or service file.
# ============================================================

# Ensure ggplot2 is available (this file loads before 01_packages.R)
library(ggplot2)

# ---- Design tokens ----------------------------------------------------------
tokens <- list(
  # Surface
  color_canvas       = "#faf9f7",
  color_surface      = "#ffffff",
  color_surface_alt  = "#f5f5f4",
  color_border       = "#e7e5e4",
  color_border_soft  = "#f0ede8",
  color_topbar       = "#0f0d0b",
  color_topbar_alt   = "#1c1917",

  # Ink (warm-neutral instead of pure black/gray)
  color_ink_primary   = "#1c1917",
  color_ink_secondary = "#57534e",
  color_ink_tertiary  = "#78716c",
  color_ink_muted     = "#a8a29e",
  color_ink_inverse   = "#fafaf9",

  # Accent & semantic
  color_accent        = "#fb6e36",
  color_accent_soft   = "#fff7ed",
  color_accent_border = "#fed7aa",
  color_accent_text   = "#c2410c",

  color_warn          = "#eab308",
  color_warn_soft     = "#fefce8",
  color_warn_border   = "#fde68a",
  color_warn_text     = "#854d0e",

  color_ok            = "#16a34a",
  color_ok_soft       = "#f0fdf4",
  color_ok_border     = "#bbf7d0",
  color_ok_text       = "#15803d",

  color_down          = "#b91c1c",
  color_down_soft     = "#fef2f2",
  color_down_border   = "#fecaca",

  # Typography
  font_sans  = "'Inter', -apple-system, 'Segoe UI', sans-serif",
  font_serif = "'Charter', 'Georgia', 'Times New Roman', serif",
  font_mono  = "'IBM Plex Mono', 'JetBrains Mono', 'Consolas', monospace",

  fs_section_title  = "26px",
  fs_card_title     = "14px",
  fs_kpi_value      = "26px",
  fs_kpi_featured   = "30px",
  fs_kpi_label      = "10px",
  fs_body           = "13px",
  fs_meta           = "11px",
  fs_micro          = "10px",

  fw_regular = 400,
  fw_medium  = 500,
  fw_semi    = 600,
  fw_bold    = 700,

  # Spacing (4px base)
  sp_1 = "4px",  sp_2 = "8px",  sp_3 = "12px", sp_4 = "16px",
  sp_5 = "20px", sp_6 = "24px", sp_8 = "32px", sp_10 = "40px",

  # Radii
  radius_sm = "5px",
  radius_md = "7px",
  radius_lg = "10px",
  radius_xl = "14px",

  # Shadows (stone-tinted)
  shadow_sm   = "0 1px 2px rgba(17, 24, 39, 0.03)",
  shadow_md   = "0 1px 3px rgba(17, 24, 39, 0.04)",
  shadow_glow = "0 0 0 1px rgba(251, 110, 54, 0.25), 0 4px 20px rgba(251, 110, 54, 0.12)",

  # Transitions
  transition = "all 150ms ease"
)

# ---- bs_theme() -------------------------------------------------------------
# Defined for forward compatibility. The app currently uses shinydashboard,
# so this function is not yet called from app.R. When the app migrates to
# bslib::page_sidebar, call build_app_theme() to theme the shell.
build_app_theme <- function() {
  bslib::bs_theme(
    version = 5,

    bg       = tokens$color_canvas,
    fg       = tokens$color_ink_primary,
    primary  = tokens$color_accent,
    success  = tokens$color_ok,
    warning  = tokens$color_warn,
    danger   = tokens$color_down,

    # Fonts — fall back to local if Google fetch fails (Windows offline case)
    base_font    = bslib::font_google("Inter",         local = TRUE, wght = c(400, 500, 600, 700)),
    heading_font = bslib::font_google("Inter",         local = TRUE, wght = c(500, 600, 700)),
    code_font    = bslib::font_google("IBM Plex Mono", local = TRUE, wght = c(400, 500, 600)),

    # Bootstrap variable overrides keyed to tokens
    "body-color"          = tokens$color_ink_primary,
    "body-bg"             = tokens$color_canvas,
    "border-radius"       = tokens$radius_md,
    "border-radius-lg"    = tokens$radius_lg,
    "card-border-color"   = tokens$color_border,
    "card-bg"             = tokens$color_surface,
    "card-border-radius"  = tokens$radius_lg,
    "card-box-shadow"     = tokens$shadow_md,
    "enable-shadows"      = TRUE,
    "enable-rounded"      = TRUE
  )
}

# ---- Export to .GlobalEnv ---------------------------------------------------
# Shiny auto-sources R/*.R files into a session env that is NOT globalenv().
# Function closures defined in *other* auto-sourced files (e.g. R/tabs/*.R
# sourced manually from app.R) may not see `tokens` via lexical scope.
# Force it into globalenv() so any closure can find it.
assign("tokens", tokens, envir = globalenv())
assign("build_app_theme", build_app_theme, envir = globalenv())

# ---- ggplot2 theme ----------------------------------------------------------
# Consistent ggplot2 theme for AG policy repo charts.
# VS does not need this because it doesn't use ggplot2.
theme_custom <- theme_minimal(base_size = 11) +
  theme(
    legend.position    = "bottom",
    legend.title       = element_text(size = 10),
    legend.text        = element_text(size = 9),
    axis.text          = element_text(size = 12, face = "bold"),
    axis.title         = element_text(size = 12, face = "bold"),
    plot.title         = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 10, face = "bold")
  )
