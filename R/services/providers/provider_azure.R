# ============================================================
# R/services/providers/provider_azure.R
#
# Azure OpenAI provider. Requires three env vars:
#   AZURE_OPENAI_API_KEY
#   AZURE_OPENAI_ENDPOINT (https://<resource>.openai.azure.com)
#   AZURE_OPENAI_DEPLOYMENT (deployment name)
# Optional: AZURE_OPENAI_API_VERSION (default "2024-06-01")
# ============================================================

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

make_azure_provider <- function() {
  api_key    <- Sys.getenv("AZURE_OPENAI_API_KEY", "")
  endpoint   <- Sys.getenv("AZURE_OPENAI_ENDPOINT", "")
  deployment <- Sys.getenv("AZURE_OPENAI_DEPLOYMENT", "")
  if (!nzchar(api_key) || !nzchar(endpoint) || !nzchar(deployment)) return(NULL)
  api_version <- Sys.getenv("AZURE_OPENAI_API_VERSION", "2024-06-01")
  endpoint <- sub("/+$", "", endpoint)

  call_fn <- function(system, user, max_tokens = 200,
                      temperature = 0.2, timeout_sec = 8) {
    t0 <- Sys.time()
    url <- sprintf("%s/openai/deployments/%s/chat/completions?api-version=%s",
                   endpoint, deployment, api_version)
    req <- httr2::request(url)
    req <- httr2::req_headers(req,
      "api-key"      = api_key,
      "Content-Type" = "application/json"
    )
    req <- httr2::req_body_json(req, list(
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
      stop("azure http ", status, ": ", substr(body, 1, 200))
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

  list(name = "azure", model = deployment, call = call_fn)
}
