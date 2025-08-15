library(glue)
library(jsonlite)
library(readr)

run_llm_ode_search <- function(xdx,outDir, LLM_ID, max_iter = 10) {
  
  # --- 1. Setup and Initialization ---
  dir.create(outDir, recursive = TRUE, showWarnings = FALSE)
  sapply(c("prompts", "responses", "models", "opt"), function(d) {
    dir.create(file.path(outDir, d), showWarnings = FALSE)
  })
  
  source("R/llm_report_gen.R")
  source("R/api_utils.R")
  source("R/optim_utils.R") # Added as requested
  
  # --- 2. Inner Function to Optimize a Set of Models ---
  optimize_from_specs <- function(model_specs, iter_num) {
    process_llm_model <- function(model_spec) {
      full_body_string <- paste0("{", model_spec$deriv_R_function_body, "}")
      deriv_function <- function(x, theta) {}
      body(deriv_function) <- parse(text = full_body_string)
      list(
        model_name = model_spec$model_name,
        justification = model_spec$justification,
        states = unlist(model_spec$states),
        param_names = unlist(model_spec$param_names),
        lower = unlist(model_spec$param_bounds$lower),
        upper = unlist(model_spec$param_bounds$upper),
        deriv = deriv_function
      )
    }
    
    models <- lapply(model_specs, process_llm_model)
    saveRDS(models, file.path(outDir, "models", sprintf("models_%d.Rds", iter_num)))
    
    optim_wrapper <- function(model) {
      p0 <- runif(length(model$lower), min = model$lower, max = model$upper)
      names(p0) <- model$param_names
      upper <- model$upper
      lower <- model$lower
      # Assumes optim_deriv returns a list containing at least $opt and $dX_hat
      optim_deriv(upper,lower, model, xdx$X, xdx$dX) 
    }
    
    opt_list <- lapply(models, optim_wrapper)
    saveRDS(opt_list, file.path(outDir, "opt", sprintf("opt_%d.Rds", iter_num)))
    
    err <- sapply(opt_list, function(o) o$opt$value)
    best_idx <- which.min(err)
    
    list(
      opt = opt_list[[best_idx]],
      model = models[[best_idx]],
      spec = toJSON(model_specs[[best_idx]], auto_unbox = TRUE, pretty = TRUE)
    )
  }
  
  # --- 3. Initial Model Generation (Iter 0) ---
  discovery_report <- generate_llm_discovery_report(xdx$dX, xdx$X)
  init_template <- read_file("prompts/grad_prompts/init_template")
  schema <- read_file("prompts/grad_prompts/json_rules")
  model_hints <- ""
  
  prompt <- glue(init_template, .open = "{{", .close = "}}")
  writeLines(prompt, file.path(outDir, "prompts", "prompt_0.txt"))
  
  resp <- call_llm(prompt, model = LLM_ID, temperature = 0.1)
  writeLines(resp, file.path(outDir, "responses", "resp_0.json"))
  
  model_specs <- fromJSON(resp, simplifyVector = FALSE)
  best_result <- optimize_from_specs(model_specs, 0)
  
  # --- 4. Refinement Loop ---
  if (max_iter > 0) {
    for (i in 1:max_iter) {
      cat(sprintf("\n--- Starting Refinement Iteration %d ---\n", i))
      
      # Correctly access nested elements based on your template's logic
      opt <- best_result$opt
      model <- best_result$model
      spec <- best_result$spec
      
      diagnostic_report <- generate_llm_diagnostic_report(xdx$dX, opt$dX_hat, xdx$X)
      opt_smm <- toJSON(opt$opt, auto_unbox = TRUE)
      
      refine_template <- read_file("prompts/grad_prompts/refine_template")
      prompt <- glue(refine_template, .open = "{{", .close = "}}")
      writeLines(prompt, file.path(outDir, "prompts", sprintf("prompt_%d.txt", i)))
      
      resp <- call_llm(prompt, model = LLM_ID, temperature = 0.1)
      writeLines(resp, file.path(outDir, "responses", sprintf("resp_%d.json", i)))
      
      model_specs <- fromJSON(resp, simplifyVector = FALSE)
      best_result <- optimize_from_specs(model_specs, i)
    }
  }
  
  # --- 5. Return the final best result ---
  cat("\n--- Workflow Complete ---\n")
  return(best_result)
}