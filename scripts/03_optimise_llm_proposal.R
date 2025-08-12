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
  library(optimParallel)
})

# Source the simulation functions from your existing script
source("R/ground_truth_simulator.R")
source("R/create_summary_object.R")

# --- Configuration ---
# Define which model we are working with. You can change "droop" to "edelstein"
# or any other model you've generated a proposal for.
ncores <- 16
MODEL_NAME <- "beddington"
#MODEL_NAME <- "droop"
#MODEL_NAME <- "edelstein"
RUN_SUFFIX <- "_refined_1"
#RUN_SUFFIX <- ""
# Define the paths based on the model name
llm_proposal_path <- file.path("data", "llm_proposals", paste0(MODEL_NAME,RUN_SUFFIX, ".json"))
# This assumes you have a CSV of the data you're fitting to.
# We will generate it from the ground truth spec if it doesn't exist.
target_data_path <- file.path("results", "csv", paste0(MODEL_NAME, ".csv"))
ground_truth_spec_path <- file.path("data", "ground_truth_specs", paste0(MODEL_NAME, ".json"))
output_plot_path <- file.path("results", "plots", paste0(MODEL_NAME,RUN_SUFFIX, "_fit_comparison.png"))


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


# 3. DEFINE OBJECTIVE FUNCTION (CORRECTED LOGIC)
# ==============================================================================
objective_function <- function(params_vec, compiled_model_obj, spec, data) {
  # (The rest of the logic remains the same)
  temp_spec <- spec
  sim_results_list <- lapply(temp_spec$conditions, function(cond) {
    condition_params <- temp_spec$param_values
    for (p_name in names(params_vec)) {
      if (endsWith(p_name, paste0("_", cond$name))) {
        base_name <- sub(paste0("_", cond$name), "", p_name)
        condition_params[[base_name]] <- params_vec[[p_name]]
      } else if (!grepl("_", p_name, fixed = TRUE)) {
        condition_params[[p_name]] <- params_vec[[p_name]]
      }
    }
    # Pass the pre-compiled model to the simulator
    simulate_from_spec_for_condition(compiled_model_obj, temp_spec, cond$name, condition_params)
  })
  sim_df <- bind_rows(sim_results_list)
  if (is.null(sim_df) || nrow(sim_df) == 0) return(1e10)
  comparison_df <- inner_join(data, sim_df, by = c("time", "variable", "condition"), suffix = c("_target", "_model"))
  sse <- sum((comparison_df$value_target - comparison_df$value_model)^2, na.rm = TRUE)
  return(sse)
}

# Helper function also updated to take the compiled model
simulate_from_spec_for_condition <- function(model_obj, spec, condition_name, params) {
  single_condition_spec <- spec
  single_condition_spec$conditions <- list(Filter(function(c) c$name == condition_name, spec$conditions)[[1]])
  single_condition_spec$param_values <- params
  single_condition_spec$replicates <- 1
  # This now calls the modified simulator that uses the pre-compiled object
  tryCatch({
    simulate_from_spec_compiled(model_obj, single_condition_spec)
  }, error = function(e) { NULL })
}

# We need a new simulator function that works with the compiled object
simulate_from_spec_compiled <- function(model_obj, spec, seed = 1) {
  # This is a simplified version of your original simulator
  spec <- validate_spec(spec)
  t0 <- spec$sampling$t0; t1 <- spec$sampling$t1; dt <- spec$sampling$dt
  times <- seq(t0, t1, by = dt)
  cond <- spec$conditions[[1]]
  init <- modifyList(as.list(spec$init), cond$init %||% list())
  y0 <- unlist(init)[unlist(spec$model$states)]
  y0 <- setNames(as.numeric(y0), names(y0))
  parms <- unlist(spec$param_values)
  ev <- inputs_to_event_df(spec$inputs, times)
  sol <- rxode2::rxSolve(model_obj, params = parms, inits = y0, events = ev, atol = 1e-8, rtol = 1e-8) |> as.data.frame()
  sol <- sol[sol$time %in% times, , drop = FALSE]
  obs_map <- spec$observe
  obs_df <- sol[, c("time", unname(unlist(obs_map))), drop = FALSE]
  names(obs_df) <- c("time", names(obs_map))
  noisy <- apply_noise(obs_df, spec$noise)
  long <- noisy |>
    tidyr::pivot_longer(-time, names_to = "variable", values_to = "value") |>
    mutate(condition = cond$name, replicate = 1, model = spec$model$name)
  return(long)
}

# 4. RUN OPTIMIZATION (CORRECTED LOGIC)
# ==============================================================================
message("\n--- Preparing for optimization ---")

message("--- Pre-compiling the rxode2 model ---")
# This creates the compiled model object that we can safely pass around
compiled_model <- rxode2::rxode(llm_spec$model$ode_text)
message("✔ Model compiled successfully.")

# --- REVISED PARAMETER PREPARATION LOGIC ---
initial_params_list <- list()

# 1. Add all GLOBAL parameters from the top-level `param_values`
initial_params_list <- c(initial_params_list, llm_spec$param_values)

# 2. Identify parameters that have local overrides
local_param_names <- unique(unlist(lapply(llm_spec$conditions, function(c) names(c$params))))

# 3. Remove the identified local parameters from the main list
#    They will be replaced by their condition-specific versions.
initial_params_list <- initial_params_list[!names(initial_params_list) %in% local_param_names]

# 4. Add the uniquely named LOCAL parameters
for (cond in llm_spec$conditions) {
  if (!is.null(cond$params) && length(cond$params) > 0) {
    for (p_name in names(cond$params)) {
      unique_name <- paste(p_name, cond$name, sep = "_")
      initial_params_list[[unique_name]] <- cond$params[[p_name]]
    }
  }
}
initial_params <- unlist(initial_params_list)

message("Parameters to be optimized:")
print(initial_params)

# Run the optimization
cl <- makeCluster(ncores)
setDefaultCluster(cl = cl)

clusterEvalQ(cl, {
  suppressPackageStartupMessages({
    library(rxode2)
    library(dplyr)
    library(tidyr)
  })
})

# Export all custom functions from the global environment that are needed
# by the objective function. Be thorough to avoid errors.
clusterExport(cl, c(
  "simulate_from_spec_for_condition", # The function that was missing
  "simulate_from_spec",             # Its dependency
  "validate_spec",                  # All helpers from ground_truth_simulator.R
  "inputs_to_event_df",
  "apply_noise",
  "compiled_model",
  "input_change_times"
))

# The call is almost identical to optim(), just with the new function name
opt_results <- optimParallel(
  par = initial_params,
  fn = objective_function,
  spec = llm_spec,
  data = target_data_df,
  method = "L-BFGS-B", # optimParallel specializes in this method
  lower = rep(0.0001, length(initial_params)),
  control = list(trace = 1, maxit = 1000)
)

stopCluster(cl)

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


message("\n--- Creating model summary object ---")

# Define the output path for the summary
summary_output_path <- file.path("results", "summaries", paste0(MODEL_NAME,RUN_SUFFIX, "_summary.json"))

# Create the directory if it doesn't exist
dir.create(dirname(summary_output_path), showWarnings = FALSE, recursive = TRUE)

# Create the summary object
model_summary <- create_summary_object(
  llm_spec = llm_spec,
  opt_results = opt_results,
  initial_objective_value = initial_objective_value, # Pass the new value here
  target_data_df = target_data_df,
  optimized_fit_df = optimized_fit_df
)

# Save the summary object as a JSON file
save_summary_as_json(model_summary, summary_output_path)


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


