# scripts/2_generate_initial_proposal.R
#
# This script generates an initial model proposal from the LLM.
#
# Example Usage (from project root):
# Rscript scripts/2_generate_initial_proposal.R
#

# 1. SETUP
# ==============================================================================
# Load necessary libraries
suppressPackageStartupMessages({
  library(jsonlite)
  library(readr)
  library(glue)
})

# Source the updated LLM calling function
source("R/llm_caller.R")


# 2. CONFIGURATION
# ==============================================================================
MODEL_NAME <- "edelstein"
LLM_ID <- "qwen-3-coder-480b"
message(paste("--- Preparing to generate proposal for model:", MODEL_NAME, "---"))


# 3. DEFINE PATHS AND PREPARE PROMPT
# ==============================================================================
# Define paths
data_packet_path <- file.path("data", "LLM_data_packets", paste0(MODEL_NAME, ".json"))
prompt_template_path <- file.path("prompts", "init_template_v1")
output_proposal_path <- file.path("data", "llm_proposals", paste0(MODEL_NAME, ".json"))

# Check if the data packet exists
if (!file.exists(data_packet_path)) {
  stop("Data packet not found at: ", data_packet_path)
}

# Load the prompt template
prompt_template <- read_file(prompt_template_path)
message("✔ Loaded prompt template from: ", prompt_template_path)

# Load the data packet content
# Using a helper function to ensure it's read correctly as a single string
read_packet_text <- function(path) {
  txt <- readLines(path, warn = FALSE)
  if (length(txt) > 1) paste0("[", paste(txt, collapse = ","), "]") else txt
}
packet_json <- read_packet_text(data_packet_path)
model_hints <- "" # Optional hints can be added here

# Build the final prompt
final_prompt <- glue(
  prompt_template,
  .open = "{{",
  .close = "}}",
  packet_json = packet_json,
  model_hints = model_hints
)
message("✔ Final prompt constructed.")


# 4. CALL THE LLM TO GENERATE THE PROPOSAL
# ==============================================================================
message("\n--- Calling LLM to generate model proposal ---")
message("Output will be saved to: ", output_proposal_path)

# Create the output directory if it doesn't exist
dir.create(dirname(output_proposal_path), showWarnings = FALSE, recursive = TRUE)

# Call the refactored LLM function with the complete prompt
llm_result <- call_llm(
  user_prompt = final_prompt,
  out_path = output_proposal_path,
  model = LLM_ID,
  temperature = 0.1,
  seed = 42
)


# 5. FINAL CONFIRMATION
# ==============================================================================
if (llm_result$ok) {
  message("\n✔ Successfully generated and saved model proposal to: ", output_proposal_path)
} else {
  warning("\n! LLM call failed or returned invalid JSON. Check the raw output file.")
}

message("--- Script finished ---")