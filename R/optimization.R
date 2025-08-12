# R/optimization.R
#
# This script provides a generalized function to run model optimization using
# either a standard serial optimizer (optim) or a parallel version (optimParallel).
# It relies on the centrally defined `simulate_from_spec` function.

# 1. SETUP
# ==============================================================================
suppressPackageStartupMessages({
  library(rxode2)
  library(dplyr)
  library(tidyr)
  library(optimParallel)
})

# Source the ground truth simulator which contains the `simulate_from_spec` function.
source("R/simulation.R")

#' Modify a spec object with a named vector of parameters
#'
#' This function robustly updates a model specification using a two-step process:
#' 1. It first updates any global parameters.
#' 2. It then updates any condition-specific parameter overrides.
#'
#' @param params_vec A named numeric vector of parameters, e.g.,
#'                   `c(k_out = 0.1, Rstar_basal = 0.01, k_in_slower_activation = 0.25)`.
#' @param spec The original model specification object to modify.
#' @return A new, updated spec object.

modify_spec_parms <- function(params_vec, spec) {
  
  # Create a copy to ensure the original spec is not modified
  new_spec <- spec
  
  # Step 1: Overwrite any global parameters with exact matches from params_vec.
  # --------------------------------------------------------------------------
  # Iterate through the names of the global parameters in the spec.
  for (p_name in names(new_spec$param_values)) {
    # If a parameter with this exact name exists in the input vector...
    if (p_name %in% names(params_vec)) {
      # ...update it in the new spec.
      new_spec$param_values[[p_name]] <- params_vec[[p_name]]
    }
  }
  
  # Step 2: Iterate over conditions and update any condition-specific overrides.
  # --------------------------------------------------------------------------
  # Loop through each condition defined in the spec (e.g., "baseline", "slower_activation").
  for (i in seq_along(new_spec$conditions)) {
    cond_name <- new_spec$conditions[[i]]$name
    
    # Now, loop through all possible base parameter names.
    # We check against all global parameters to see if a condition-specific
    # version of it exists.
    for (p_name in names(spec$param_values)) {
      
      # Create the hypothetical condition-specific name (e.g., "k_in_slower_activation").
      hypothetical_name <- paste(p_name, cond_name, sep = "_")
      
      # If this constructed name exists in the input parameter vector...
      if (hypothetical_name %in% names(params_vec)) {
        
        # ...update the parameter for this specific condition.
        # Initialize the params block if it doesn't exist.
        if (is.null(new_spec$conditions[[i]]$params)) {
          new_spec$conditions[[i]]$params <- list()
        }
        new_spec$conditions[[i]]$params[[p_name]] <- params_vec[[hypothetical_name]]
      }
    }
  }
  
  return(new_spec)
}

# 2. OBJECTIVE FUNCTION
# ==============================================================================
# This function is passed to the optimizer. It calculates the difference between
# the model simulation and the target data.
objective_function <- function(params_vec, compiled_model_obj, spec, data) {
  
 
  temp_spec <- modify_spec_parms(params_vec,spec)
  # 3. Perform a SINGLE simulation with the fully updated, multi-condition spec.
  # The `simulate_from_spec` function handles the multiple conditions internally.
  sim_df <- tryCatch({
    simulate_from_spec(spec = temp_spec, mdl = compiled_model_obj, seed = 1)
  }, error = function(e) {
    warning("Simulation with the current parameter set failed. Error: ", e$message)
    return(NULL) # Return NULL if the simulation crashes.
  })
  
  # If simulation fails, return a large error value to the optimizer.
  if (is.null(sim_df) || nrow(sim_df) == 0) {
    return(1e10)
  }
  
  # 4. Calculate the sum of squared errors against the target data.
  comparison_df <- inner_join(data, sim_df, by = c("time", "variable", "condition"), suffix = c("_target", "_model"))
  
  # Ensure the join was successful before calculating SSE.
  if (nrow(comparison_df) == 0) {
    warning("No matching timepoints between simulation and data. Check spec definitions.")
    return(1e10)
  }
  
  sse <- sum((comparison_df$value_target - comparison_df$value_model)^2, na.rm = TRUE)
  print(sse)
  return(sse)
}


# 3. CORE OPTIMIZATION WRAPPER 
# ==============================================================================
optimize_model <- function(spec, target_data_df, method = "optimParallel", ncores = 4) {
  
  message("--- Preparing for optimization ---")
  
  # 1. Pre-compile the rxode2 model for performance.
  message("1. Pre-compiling the rxode2 model...")
  compiled_model <- rxode2::rxode(spec$model$ode_text)
  message("✔ Model compiled successfully.")
  
  # 2. Prepare the initial parameter vector from the spec.
  message("2. Preparing initial parameter vector...")
  
  # --- CORRECTED PARAMETER PREPARATION LOGIC ---
  initial_params_list <- list()
  
  # 2a. Add all GLOBAL parameters. These will always be optimized.
  initial_params_list <- c(initial_params_list, spec$param_values)
  
  # 2b. Add uniquely named LOCAL parameters for any condition-specific overrides.
  for (cond in spec$conditions) {
    if (!is.null(cond$params) && length(cond$params) > 0) {
      for (p_name in names(cond$params)) {
        unique_name <- paste(p_name, cond$name, sep = "_")
        initial_params_list[[unique_name]] <- cond$params[[p_name]]
      }
    }
  }
  initial_params <- unlist(initial_params_list)
  # --- End of corrected logic ---
  
  message("Parameters to be optimized:")
  print(initial_params)
  
  message("\n--- Starting optimization with method: ", method, " ---")
  
  # 3. Select and run the optimizer (This part remains unchanged)
  if (method == "optimParallel") {
    cl <- makeCluster(ncores)
    setDefaultCluster(cl = cl)
    clusterEvalQ(cl, {
      suppressPackageStartupMessages({
        library(rxode2); library(dplyr); library(tidyr)
      })
      source("R/simulation.R")
    })
    clusterExport(cl, c("compiled_model", "modify_spec_parms"), envir = environment())
    
    opt_results <- optimParallel(
      par = initial_params, fn = objective_function,
      compiled_model_obj = compiled_model, spec = spec, data = target_data_df,
      method = "L-BFGS-B", lower = rep(0.0001, length(initial_params)),
      control = list(trace = 1, maxit = 1000)
    )
    stopCluster(cl)
    
  } else if (method == "optim") {
    opt_results <- optim(
      par = initial_params, fn = objective_function,
      compiled_model_obj = compiled_model, spec = spec, data = target_data_df,
      method = "L-BFGS-B", lower = rep(0.0001, length(initial_params)),
      control = list(trace = 1, maxit = 1000)
    )
  } else if (method == "none") {
    ## for convenience allow return of unoptimized value.
    return(objective_function(initial_params,compiled_model,spec,target_data_df))
  } else {
    stop("Invalid optimization method. Choose 'optim' or 'optimParallel'.")
  }
  
  message("\n--- Optimization complete ---")
  final_params <- setNames(opt_results$par, names(initial_params))
  message("Final optimized parameters:")
  print(final_params)
  
  return(opt_results)
}