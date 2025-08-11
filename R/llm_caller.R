# deps: httr2, jsonlite, glue
library(httr2); library(jsonlite); library(glue)

render_prompt <- function(prompt_template, vars=list()) {
  do.call(glue, c(list(prompt_template, .open="{{", .close="}}"), vars))
}

read_packet_text <- function(path) {
  txt <- readLines(path, warn=FALSE)
  if (length(txt) > 1) paste0("[", paste(txt, collapse=","), "]") else txt
}

# A more robust function to extract the first complete JSON object from a string
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

# strip code fences if the model adds them
strip_fences <- function(s){
  sub("^\\s*```(?:json)?\\s*|\\s*```\\s*$", "", s)
}

cerebras_call <- function(packet_path,
                          prompt_template,
                          template_vars=list(),
                          model="llama-4-scout-17b-16e-instruct",
                          api_key=Sys.getenv("CEREBRAS_API_KEY"),
                          out_path="llm_out.json",
                          system_preamble=NULL,
                          temperature=0,
                          top_p=1,
                          seed=0,
                          max_tokens=-1,
                          retries=2) {
  
  if (!nzchar(api_key)) stop("CEREBRAS_API_KEY is not set")
  
  # if your template expects {{packet_json}}, make it available
  if (!("packet_json" %in% names(template_vars))) {
    template_vars$packet_json <- read_packet_text(packet_path)
  }
  user_prompt <- render_prompt(prompt_template, template_vars)
  
  msgs <- list()
  if (!is.null(system_preamble)) msgs <- c(msgs, list(list(role="system", content=system_preamble)))
  msgs <- c(msgs, list(list(role="user", content=user_prompt)))
  
  body <- list(
    model = model,
    stream = FALSE,
    messages = msgs,
    temperature = temperature,
    max_tokens = max_tokens,
    seed = seed,
    top_p = top_p
  )
  
  req <- request("https://api.cerebras.ai/v1/chat/completions") |>
    req_headers(
      "Authorization" = paste("Bearer", api_key),
      "Content-Type" = "application/json"
    ) |>
    req_body_json(body, auto_unbox=TRUE)
  
  resp <- req |>
    req_retry(max_tries = retries) |>   # httr2 retry
    req_perform()
  
  resp_check_status(resp)               # <-- httr2 status check
  
  res <- resp_body_json(resp, simplifyVector = FALSE)
  
  # OpenAI-compatible extraction; some models may return a list of content parts
  content <- res$choices[[1]]$message$content
  txt <- extract_json(content)
  
  # The tryCatch block now works on the cleanly extracted text
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
    # Write the original, pre-extracted content for debugging
    writeLines(content, raw_path)
    warning("Model output did not contain valid JSON; saved raw text to: ", raw_path)
  }
  
  invisible(list(ok = parsed_ok, text = txt, parsed = parsed, raw = res))
}

