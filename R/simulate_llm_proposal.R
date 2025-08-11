# R/simulate_llm_proposal.R

# 1. Load libraries and helper functions
suppressPackageStartupMessages({
  library(jsonlite)
  library(readr)
  library(dplyr)
})
source("R/ground_truth_simulator.R") # Make sure the path is correct

# 2. Define input and output paths
llm_proposal_path <- "data/llm_proposals/spec_guess.json"
output_csv_path <- "results/csv/llm_simulation_output.csv"

# 3. Load the LLM's proposed model specification
message("Loading LLM proposal from: ", llm_proposal_path)
llm_spec <- fromJSON(llm_proposal_path, simplifyVector = FALSE)

# 4. Simulate the model
message("Simulating the LLM's proposed model...")
# The seed is set for reproducibility
simulation_df <- simulate_from_spec(llm_spec, seed = 123)

# 5. Save the results
message("Saving simulation results to: ", output_csv_path)
write_csv(simulation_df, output_csv_path)

message("Simulation complete!")