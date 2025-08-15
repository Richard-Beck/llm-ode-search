library(httr2)
library(jsonlite)

#' @description A simplified function to call the Cerebras LLM API.
#'
#' @param user_prompt The main prompt to send to the model.
#' @param system_preamble An optional system message to guide the model's behavior.
#' @param model The specific model endpoint to use.
#' @param api_key Your API key. Defaults to the CEREBRAS_API_KEY environment variable.
#' @param temperature Controls the randomness of the output. 0 is deterministic.
#' @param max_tokens The maximum number of tokens to generate.
#' @param retries The number of times to retry the request on failure.
#'
#' @return On success, returns the raw text content (a string) from the LLM's response.
#'   On failure, returns the error object itself for inspection.
call_llm <- function(user_prompt,
                     system_preamble = NULL,
                     model = "llama-4-scout-17b-16e-instruct",
                     api_key = Sys.getenv("CEREBRAS_API_KEY"),
                     temperature = 0,
                     max_tokens = -1,
                     retries = 2) {
  
  # Use tryCatch to wrap the entire operation for robust error handling.
  tryCatch({
    # 1. Validate API key
    if (!nzchar(api_key)) {
      stop("CEREBRAS_API_KEY is not set or is empty.")
    }
    
    # 2. Construct the message list
    msgs <- list()
    if (!is.null(system_preamble)) {
      msgs <- c(msgs, list(list(role = "system", content = system_preamble)))
    }
    msgs <- c(msgs, list(list(role = "user", content = user_prompt)))
    
    # 3. Construct the request body
    body <- list(
      model = model,
      messages = msgs,
      temperature = temperature,
      max_tokens = max_tokens,
      stream = FALSE
    )
    
    # 4. Build and perform the request
    # req_perform() will automatically error for non-2xx HTTP status codes,
    # which will be caught by the tryCatch block.
    resp <- request("https://api.cerebras.ai/v1/chat/completions") |>
      req_headers(
        "Authorization" = paste("Bearer", api_key),
        "Content-Type" = "application/json"
      ) |>
      req_body_json(body, auto_unbox = TRUE) |>
      req_retry(max_tries = retries) |>
      req_perform()
    
    # 5. If successful, extract and return the raw text content
    resp_body_json(resp, simplifyVector = FALSE)$choices[[1]]$message$content
    
  }, error = function(e) {
    # 6. If any error occurs, issue a warning and return the error object
    warning("API call failed. Returning the error object for inspection.")
    return(e)
  })
}