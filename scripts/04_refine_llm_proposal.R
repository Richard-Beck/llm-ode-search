# scripts/4_run_refinement_step.R
#
# This script performs a single model refinement step. It loads the summary
# from a previous run, combines it with the data packet and the refinement
# prompt template, and then calls the LLM to generate a new, improved proposal.
#
# For now, it is hardcoded to run on the 'edelstein' model.
#

# 1. SETUP
# ==============================================================================
# Load necessary libraries
# scripts/4_run_refinement_step.R
#
# This script performs a single model refinement step.
#
# For now, it is hardcoded to run on the 'edelstein' model.
#

# 1. SETUP
# ==============================================================================
suppressPackageStartupMessages({
  library(jsonlite)
  library(readr)
  library(glue)
})

source("R/llm_caller.R")


# 2. CONFIGURATION
# ==============================================================================
MODEL_NAME <- "beddington"
REFINEMENT_ITERATION <- 1
LLM_ID <- "qwen-3-coder-480b"
message(paste("--- Preparing refinement step", REFINEMENT_ITERATION, "for model:", MODEL_NAME, "---"))


# 3. DEFINE PATHS AND PREPARE PROMPT
# ==============================================================================
summary_input_path <- file.path("results", "summaries", paste0(MODEL_NAME, "_summary.json"))
data_packet_path <- file.path("data", "LLM_data_packets", paste0(MODEL_NAME, ".json"))
prompt_template_path <- file.path("prompts", "refine_template_v1")
output_proposal_path <- file.path(
  "data", "llm_proposals",
  paste0(MODEL_NAME, "_refined_", REFINEMENT_ITERATION, ".json")
)

if (!file.exists(summary_input_path)) {
  stop("Model summary file not found at: ", summary_input_path)
}

prompt_template <- read_file(prompt_template_path)
model_summary_list <- fromJSON(summary_input_path, simplifyVector = FALSE)
model_summary_json <- toJSON(model_summary_list, auto_unbox = TRUE, pretty = TRUE, null = "null")

read_packet_text <- function(path) {
  txt <- readLines(path, warn = FALSE)
  if (length(txt) > 1) paste0("[", paste(txt, collapse = ","), "]") else txt
}
packet_json <- read_packet_text(data_packet_path)

message("✔ Loaded all necessary components for refinement prompt.")


# 4. BUILD THE FINAL PROMPT AND CALL THE LLM
# ==============================================================================
message("\n--- Building final prompt for LLM ---")

final_prompt <- glue(
  prompt_template,
  .open = "{{",
  .close = "}}",
  model_summary_json = model_summary_json,
  packet_json = packet_json
)

message("✔ Prompt constructed successfully.")
message("\n--- Calling LLM to generate refined model proposal ---")
message("Output will be saved to: ", output_proposal_path)

# Call the refactored LLM function with the complete prompt
llm_result <- call_llm(
  user_prompt = final_prompt,
  out_path = output_proposal_path,
  model = LLM_ID,
  temperature = 0.2,
  seed = 43
)


# 5. FINAL CONFIRMATION
# ==============================================================================
if (llm_result$ok) {
  message("\n✔ Successfully generated and saved refined model proposal to: ", output_proposal_path)
} else {
  warning("\n! LLM call failed or returned invalid JSON. Check the raw output file.")
}

message("--- Refinement script finished ---")