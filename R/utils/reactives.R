# ============================================================
# R/utils/reactives.R — Shared reactive helpers
# ============================================================

#' Wrap a reactive expression so errors render a graceful error card
#' instead of propagating. Used by tab servers for every non-trivial
#' render* output.
#'
#' Usage:
#'   my_output <- safe_reactive({
#'     compute_expensive_thing(perf_tape())
#'   }, error_title = "Couldn't build the vintage chart")
#'
#' @param expr An unevaluated expression (wrapped in {})
#' @param error_title Title shown on the error card
#' @return A reactive that either returns the value or an error_card tag
safe_reactive <- function(expr, error_title = "Something went wrong rendering this section") {
  expr_quoted <- substitute(expr)
  env <- parent.frame()
  shiny::reactive({
    tryCatch(
      eval(expr_quoted, envir = env),
      error = function(e) {
        message("cpt safe_reactive caught: ", conditionMessage(e))
        error_card(
          title = error_title,
          body  = conditionMessage(e)
        )
      }
    )
  })
}
