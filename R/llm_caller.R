# deps: httr2, jsonlite
library(httr2)
library(jsonlite)

# A robust function to extract a JSON string from LLM output
extract_json <- function(text) {
  # This regex looks for a ```json ... ``` or ``` ... ``` block
  # and captures only the content inside.
  match <- regexpr("```(?:json)?\\s*(\\{[\\s\\S]*\\})\\s*```", text, perl = TRUE)
  
  if (attr(match, "capture.start") > 0) {
    # If fences are found, extract the captured group (the part in parentheses)
    start <- attr(match, "capture.start")[1]
    end <- start + attr(match, "capture.length")[1] - 1
    return(substr(text, start, end))
  } else {
    # If no fences are found, fall back to finding the first raw JSON object
    match <- regexpr("\\{[\\s\\S]*\\}", text)
    if (match > -1) {
      return(regmatches(text, match))
    }
  }
  
  # If no JSON is found at all, return the original text for debugging
  return(text)
}


#' Call the LLM with a fully constructed prompt.
#'
#' @param user_prompt A single string containing the full prompt for the user role.
#' @param out_path The file path to save the resulting JSON output.
#' @param model The model to use for the API call.
#' @param api_key Your API key. Defaults to the CEREBRAS_API_KEY environment variable.
#' @param system_preamble An optional string for the system role message.
#' @param temperature The sampling temperature.
#' @param top_p The nucleus sampling probability.
#' @param seed A seed for reproducibility.
#' @param max_tokens The maximum number of tokens to generate.
#' @param retries The number of times to retry the request on failure.
#'
#' @return An invisible list containing the status of the call and the parsed/raw results.
call_llm <- function(user_prompt,
                     out_path,
                     model = "llama-4-scout-17b-16e-instruct",
                     api_key = Sys.getenv("CEREBRAS_API_KEY"),
                     system_preamble = NULL,
                     temperature = 0,
                     top_p = 1,
                     seed = 0,
                     max_tokens = -1,
                     retries = 2) {
  
  if (!nzchar(api_key)) stop("CEREBRAS_API_KEY is not set")
  
  # Construct the message list
  msgs <- list()
  if (!is.null(system_preamble)) {
    msgs <- c(msgs, list(list(role = "system", content = system_preamble)))
  }
  msgs <- c(msgs, list(list(role = "user", content = user_prompt)))
  
  # Construct the request body
  body <- list(
    model = model,
    stream = FALSE,
    messages = msgs,
    temperature = temperature,
    max_tokens = max_tokens,
    seed = seed,
    top_p = top_p
  )
  
  # Build and perform the request
  req <- request("https://api.cerebras.ai/v1/chat/completions") |>
    req_headers(
      "Authorization" = paste("Bearer", api_key),
      "Content-Type" = "application/json"
    ) |>
    req_body_json(body, auto_unbox = TRUE)
  
  resp <- req |>
    req_retry(max_tries = retries) |>
    req_perform()
  
  resp_check_status(resp)
  
  # Process the response
  res <- resp_body_json(resp, simplifyVector = FALSE)
  content <- res$choices[[1]]$message$content
  txt <- extract_json(content)
  
  # Try to parse the JSON and save the output
  parsed_ok <- TRUE
  parsed <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) {
    warning("JSON parsing failed with error: ", e$message)
    parsed_ok <<- FALSE
    NULL
  })
  
  if (parsed_ok) {
    writeLines(jsonlite::toJSON(parsed, auto_unbox = TRUE, null = "null", pretty = TRUE), out_path)
  } else {
    raw_path <- sub("\\.json$", ".txt", out_path)
    writeLines(content, raw_path)
    warning("Model output did not contain valid JSON; saved raw text to: ", raw_path)
  }
  
  invisible(list(ok = parsed_ok, text = txt, parsed = parsed, raw = res))
}
