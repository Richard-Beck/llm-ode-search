# R/optimize_llm_proposal.R
#
# This script takes an LLM-generated model specification and uses it as a
# starting point to fit the model to a target dataset via optimization.

# 1. SETUP
# ==============================================================================
# Load necessary libraries
suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(ggplot2)
})

# Source the simulation functions from your existing script
source("R/ground_truth_simulator.R")

# --- Configuration ---
# Define which model we are working with. You can change "droop" to "edelstein"
# or any other model you've generated a proposal for.
MODEL_NAME <- "beddington"
#MODEL_NAME <- "droop"

# Define the paths based on the model name
llm_proposal_path <- file.path("data", "llm_proposals", paste0(MODEL_NAME, ".json"))
# This assumes you have a CSV of the data you're fitting to.
# We will generate it from the ground truth spec if it doesn't exist.
target_data_path <- file.path("results", "csv", paste0(MODEL_NAME, ".csv"))
ground_truth_spec_path <- file.path("data", "ground_truth_specs", paste0(MODEL_NAME, ".json"))
output_plot_path <- file.path("results", "plots", paste0(MODEL_NAME, "_fit_comparison.png"))


# 2. LOAD DATA AND MODEL
# ==============================================================================
message("--- Loading experiment files ---")

# Load the LLM's proposed model specification
if (!file.exists(llm_proposal_path)) {
  stop("LLM proposal file not found: ", llm_proposal_path)
}
llm_spec <- fromJSON(llm_proposal_path, simplifyVector = FALSE)
message("✔ Loaded LLM proposal: ", llm_spec$model$name)

# Generate or load the target data to fit against
if (!file.exists(target_data_path)) {
  stop("Target data CSV not found. ")
} else {
  target_data_df <- read_csv(target_data_path, show_col_types = FALSE)
  message("✔ Loaded target data from: ", target_data_path)
}


# 3. DEFINE OBJECTIVE FUNCTION
# ==============================================================================
objective_function <- function(params_vec, spec, data) {
  temp_spec <- spec
  
  # Unpack the params_vec into the spec's global and local slots
  for (p_name in names(params_vec)) {
    p_val <- params_vec[[p_name]]
    
    # Check if the parameter is local (e.g., "alpha_damped")
    if (grepl("_", p_name, fixed = TRUE)) {
      # Split the name to get the base parameter and condition
      parts <- strsplit(p_name, "_")[[1]]
      base_name <- parts[1]
      cond_name <- parts[2]
      
      # Find the correct condition and update its local parameter value
      for (i in seq_along(temp_spec$conditions)) {
        if (temp_spec$conditions[[i]]$name == cond_name) {
          temp_spec$conditions[[i]]$params[[base_name]] <- p_val
        }
      }
    } else {
      # It's a global parameter, so update the top-level value
      temp_spec$param_values[[p_name]] <- p_val
    }
  }
  
  # Simulate the model with the new set of parameters
  sim_df <- tryCatch({
    simulate_from_spec(temp_spec, seed = 1)
  }, error = function(e) {
    # Return NULL if simulation fails, to be handled below
    return(NULL)
  })
  
  # If simulation failed, return a large error value
  if (is.null(sim_df)) {
    return(1e10)
  }
  
  # Join simulation with target data to calculate the error
  comparison_df <- inner_join(
    data,
    sim_df,
    by = c("time", "variable", "condition")
  )
  
  # Calculate and return the Sum of Squared Errors (SSE)
  sse <- sum((comparison_df$value.x - comparison_df$value.y)^2, na.rm = TRUE)
  print(sse)
  return(sse)
}


# 4. RUN OPTIMIZATION
# ==============================================================================
message("\n--- Preparing for optimization ---")

# Infer parameter scopes from the LLM spec structure
local_params_list <- unlist(lapply(llm_spec$conditions, function(c) names(c$params)))
local_param_names <- unique(local_params_list)

all_model_params <- names(llm_spec$param_values)
global_param_names <- setdiff(all_model_params, local_param_names)

# Build the initial parameter vector for the optimizer
initial_params_list <- list()
# Add global parameters
initial_params_list <- c(initial_params_list, llm_spec$param_values[global_param_names])
# Add local parameters, creating unique names (e.g., alpha_damped)
for (cond in llm_spec$conditions) {
  cond_local_params <- names(cond$params)
  for (p_name in cond_local_params) {
    unique_name <- paste(p_name, cond$name, sep = "_")
    initial_params_list[[unique_name]] <- cond$params[[p_name]]
  }
}
initial_params <- unlist(initial_params_list)

message("Parameters to be optimized:")
print(initial_params)

# Run the optimization
message("\n--- Starting optimization ---")
opt_results <- optim(
  par = initial_params,
  fn = objective_function,
  spec = llm_spec,
  data = target_data_df,
  method = "L-BFGS-B",
  lower = rep(0, length(initial_params)),
  control = list(trace = 1, maxit = 1000)
)

message("\n--- Optimization complete ---")
optimized_params <- setNames(opt_results$par, names(initial_params))
message("Final optimized parameters:")
print(optimized_params)


# 5. VISUALIZE RESULTS
# ==============================================================================
# Helper function to simulate with a named parameter vector
simulate_with_params <- function(spec, params_vec) {
  sim_spec <- spec
  for (p_name in names(params_vec)) {
    p_val <- params_vec[[p_name]]
    if (grepl("_", p_name, fixed = TRUE)) {
      parts <- strsplit(p_name, "_")[[1]]
      base_name <- parts[1]; cond_name <- parts[2]
      for (i in seq_along(sim_spec$conditions)) {
        if (sim_spec$conditions[[i]]$name == cond_name) {
          sim_spec$conditions[[i]]$params[[base_name]] <- p_val
        }
      }
    } else {
      sim_spec$param_values[[p_name]] <- p_val
    }
  }
  simulate_from_spec(sim_spec, seed = 1)
}

message("\n--- Generating comparison plot ---")

initial_fit_df <- simulate_with_params(llm_spec, initial_params) %>%
  mutate(fit_type = "Initial LLM Guess")
optimized_fit_df <- simulate_with_params(llm_spec, optimized_params) %>%
  mutate(fit_type = "Optimized Fit")

comparison_plot_df <- bind_rows(initial_fit_df, optimized_fit_df)

# --- Pre-calculate plot limits to constrain axes ---
# Calculate the min and max y-value for each facet from the target data
y_limits <- target_data_df %>%
  group_by(variable, condition) %>%
  summarise(
    min_y = min(value, na.rm = TRUE),
    max_y = max(value, na.rm = TRUE),
    .groups = 'drop'
  )

# Join these limits to the simulation data and create a "squished" y-value
# that cannot go outside the limits of the target data.
comparison_plot_df <- comparison_plot_df %>%
  left_join(y_limits, by = c("variable", "condition")) %>%
  mutate(
    value = pmax(min_y, pmin(max_y, value))
  )

p <- ggplot(target_data_df, aes(x = time, y = value)) +
  geom_point(aes(color = "Target Data"), size = 2.5, alpha = 0.8) +
  geom_line(data = comparison_plot_df, aes(x = time, y = value, linetype = fit_type, color = fit_type), size = 1) +
  facet_grid(variable ~ condition, scales = "free_y") +
  labs(
    title = paste("Model Fit Comparison:", llm_spec$model$name),
    subtitle = "Comparing LLM's initial guess to the final optimized fit",
    x = "Time", y = "Value", color = "Legend", linetype = "Legend"
  ) +
  scale_color_manual(values = c("Target Data" = "black", "Initial LLM Guess" = "orange", "Optimized Fit" = "dodgerblue")) +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom")

ggsave(output_plot_path, p, width = 12, height = 7, bg = "white")
message("✔ Saved comparison plot to: ", output_plot_path)

print(p)


