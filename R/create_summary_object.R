# R/create_summary_object.R

# Load necessary libraries
suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(tidyr)
})

#' Summarize the quality of a model fit
#'
#' This function compares the model's output to the target data and calculates
#' key summary statistics to quantify the goodness-of-fit.
#'
#' @param target_df A data frame of the target data.
#' @param optimized_df A data frame of the simulation from the optimized parameters.
#'
#' @return A list containing summary statistics of the fit.
summarize_fit_quality <- function(target_df, optimized_df) {
  comparison_df <- inner_join(
    target_df,
    optimized_df,
    by = c("time", "variable", "condition"),
    suffix = c("_target", "_model")
  ) %>%
    mutate(residual = value_target - value_model)
  
  fit_summary <- comparison_df %>%
    group_by(condition, variable) %>%
    summarise(
      n_points = n(),
      rmse = sqrt(mean(residual^2)),
      correlation = cor(value_target, value_model),
      .groups = "drop"
    )
  return(fit_summary)
}


#' Create a comprehensive, structured summary of a model fitting run
#'
#' This function packages all key artifacts from an optimization run into a
#' single, machine-readable list object, ready to be saved as JSON.
#'
#' @param llm_spec A list representing the initial model specification from the LLM.
#' @param opt_results The object returned by the `optim()` function.
#' @param initial_objective_value The value of the objective function for the *initial* parameters.
#' @param target_data_df A data frame of the target data used for fitting.
#' @param optimized_fit_df A data frame of the simulation from the optimized parameters.
#'
#' @return A list containing the structured summary.
create_summary_object <- function(llm_spec,
                                  opt_results,
                                  initial_objective_value,
                                  target_data_df,
                                  optimized_fit_df) {
  
  # Extract the final optimized parameters
  optimized_params <- setNames(opt_results$par, names(opt_results$par))
  
  # Generate the concise fit summary statistics
  fit_quality_summary <- summarize_fit_quality(target_data_df, optimized_fit_df)
  
  # Build the final summary list
  summary_list <- list(
    # The full initial proposal for context
    initial_proposal = llm_spec,
    
    # Key results from the optimization process
    optimization_summary = list(
      initial_objective_value = initial_objective_value,
      final_objective_value = opt_results$value,
      # Improvement ratio: lower is better. < 1 means improvement.
      improvement_ratio = opt_results$value / initial_objective_value,
      optimized_parameters = as.list(optimized_params),
      convergence_code = opt_results$convergence,
      convergence_message = opt_results$message
    ),
    
    # A concise breakdown of fit quality by condition and variable
    fit_quality = fit_quality_summary
  )
  
  return(summary_list)
}

#' Save the summary object to a JSON file
#'
#' @param summary_obj The summary object created by `create_summary_object`.
#' @param output_path The file path where the JSON file will be saved.
save_summary_as_json <- function(summary_obj, output_path) {
  # Create the directory if it doesn't exist
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  # Write the JSON file
  json_output <- toJSON(summary_obj, auto_unbox = TRUE, pretty = TRUE, null = "null")
  writeLines(json_output, output_path)
}