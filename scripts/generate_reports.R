# R/generate_reports.R

# This script generates an HTML report for each model summary JSON file.

# Load necessary libraries
suppressPackageStartupMessages({
  library(rmarkdown)
  library(jsonlite)
})

# Define paths and create output directory
report_template_path <- "templates/report_template.Rmd"
summary_files <- list.files("results/summaries", pattern = "\\.json$", full.names = TRUE)
output_dir <- "results/reports/"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Loop through each summary file and render the report
for (summary_file in summary_files) {
  
  # --- FIX: Read the JSON data and prepare all parameters here ---
  
  # 1. Read the JSON file into an R list object.
  summary_data <- jsonlite::fromJSON(summary_file, simplifyVector = FALSE)
  
  # 2. Construct the path to the corresponding plot.
  plot_path <- sub("results/summaries/", "results/plots/", summary_file)
  plot_path <- sub(".json", "_fit_comparison.png", plot_path)
  
  # 3. Define the final output file name.
  output_file <- basename(sub("\\.json$", ".html", summary_file))
  
  # Render the R Markdown report, passing the data object and plot path
  rmarkdown::render(
    report_template_path,
    output_file = output_file,
    output_dir = output_dir,
    params = list(
      summary_data = summary_data, # Pass the R object
      plot_path = plot_path       # Pass the plot path
    ),
    quiet = TRUE
  )
  
  message("✔ Generated report for: ", output_file)
}