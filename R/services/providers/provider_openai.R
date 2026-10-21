# ============================================================
# R/services/providers/provider_openai.R
#
# OpenAI (api.openai.com) provider. Respects OPENAI_BASE_URL
# for OpenAI-compatible custom endpoints.
# ============================================================

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

make_openai_provider <- function() {
  api_key <- Sys.getenv("OPENAI_API_KEY", "")
  if (!nzchar(api_key)) return(NULL)
  base_url <- Sys.getenv("OPENAI_BASE_URL", "https://api.openai.com")
  model    <- Sys.getenv("OPENAI_MODEL", "gpt-4o-mini")

  base_url <- sub("/+$", "", base_url)
  base_url <- sub("/v1$", "", base_url)

  call_fn <- function(system, user, max_tokens = 200,
                      temperature = 0.2, timeout_sec = 8) {
    t0 <- Sys.time()
    req <- httr2::request(paste0(base_url, "/v1/chat/completions"))
    req <- httr2::req_headers(req,
      "Authorization" = paste("Bearer", api_key),
      "Content-Type"  = "application/json"
    )
    req <- httr2::req_body_json(req, list(
      model       = model,
      max_tokens  = max_tokens,
      temperature = temperature,
      messages = list(
        list(role = "system", content = system),
        list(role = "user",   content = user)
      )
    ))
    req <- httr2::req_timeout(req, timeout_sec)
    req <- httr2::req_error(req, is_error = function(resp) FALSE)

    resp <- httr2::req_perform(req)
    status <- httr2::resp_status(resp)
    if (status >= 400) {
      body <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
      stop("openai http ", status, ": ", substr(body, 1, 200))
    }

    body <- httr2::resp_body_json(resp)
    text <- body$choices[[1]]$message$content %||% ""
    tokens_used <- body$usage$total_tokens %||% 0L

    list(
      text        = text,
      tokens_used = as.integer(tokens_used),
      latency_ms  = as.integer(as.numeric(Sys.time() - t0, units = "secs") * 1000)
    )
  }

  list(name = "openai", model = model, call = call_fn)
}
