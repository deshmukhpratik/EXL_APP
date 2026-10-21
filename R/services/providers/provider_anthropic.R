# ============================================================
# R/services/providers/provider_anthropic.R
#
# Anthropic (Claude) provider. Uses the standard /v1/messages
# endpoint. Respects ANTHROPIC_BASE_URL override (for custom
# / proxy endpoints).
# ============================================================

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

make_anthropic_provider <- function() {
  api_key  <- Sys.getenv("ANTHROPIC_API_KEY", "")
  if (!nzchar(api_key)) return(NULL)
  base_url <- Sys.getenv("ANTHROPIC_BASE_URL", "https://api.anthropic.com")
  model    <- Sys.getenv("ANTHROPIC_MODEL", "claude-haiku-4-5")

  # Strip trailing slash and an accidental /v1 suffix
  base_url <- sub("/+$", "", base_url)
  base_url <- sub("/v1$", "", base_url)

  call_fn <- function(system, user, max_tokens = 200,
                      temperature = 0.2, timeout_sec = 8) {
    t0 <- Sys.time()
    req <- httr2::request(paste0(base_url, "/v1/messages"))
    req <- httr2::req_headers(req,
      "x-api-key"         = api_key,
      "anthropic-version" = "2023-06-01",
      "content-type"      = "application/json"
    )
    req <- httr2::req_body_json(req, list(
      model       = model,
      max_tokens  = max_tokens,
      temperature = temperature,
      system      = system,
      messages    = list(list(role = "user", content = user))
    ))
    req <- httr2::req_timeout(req, timeout_sec)
    req <- httr2::req_error(req, is_error = function(resp) FALSE)

    resp <- httr2::req_perform(req)
    status <- httr2::resp_status(resp)
    if (status >= 400) {
      body <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
      stop("anthropic http ", status, ": ", substr(body, 1, 200))
    }

    body <- httr2::resp_body_json(resp)
    text_parts <- vapply(body$content %||% list(), function(b) {
      if (identical(b$type, "text")) b$text %||% "" else ""
    }, character(1))
    text <- paste(text_parts, collapse = "")

    tokens_used <- (body$usage$input_tokens %||% 0L) +
                   (body$usage$output_tokens %||% 0L)

    list(
      text        = text,
      tokens_used = as.integer(tokens_used),
      latency_ms  = as.integer(as.numeric(Sys.time() - t0, units = "secs") * 1000)
    )
  }

  list(
    name  = "anthropic",
    model = model,
    call  = call_fn
  )
}
