########################################################################
### MASTER ORCHESTRATION PIPELINE: run_all.R
########################################################################
library(here)

# Define the console log destination relative to root folder
console_log_path <- here("outputs", "All_Console_Outputs.txt")

# Ensure the destination directories are fully initialized before processing
if (!dir.exists(here("outputs"))) dir.create(here("outputs"), recursive = TRUE)
if (!dir.exists(here("data", "processed"))) dir.create(here("data", "processed"), recursive = TRUE)

# --- START CONSOLE INTERCEPTION ---
# split = TRUE allows seeing the execution status in RStudio console 
# while simultaneously writing a perfect mirror copy to the text file.
sink(console_log_path, append = FALSE, split = TRUE)

cat("========================================================================\n")
cat("   MASTER ANALYSIS PIPELINE: ACUSTIC-BIOMETRIC THESIS SANDBOX\n")
cat("========================================================================\n")
cat("Log Initialization Date/Time: ", as.character(Sys.time()), "\n")
cat("Computational Workspace Anchor: ", here(), "\n")
cat("========================================================================\n\n")

# Chronological execution sequence of all 21 scripts
scripts_to_execute <- c(
  "Step 1 -- Merge & De-deplicate.R",
  "Step 2 -- Ghost Removal.R",
  "Step 3 -- Conflict Resolve.R",
  "Step 4 -- Senor Data & Temporal Frequency Check.R",
  "Step 5 -- Participant Mapping & Reconciliation.R",
  "Step 5a -- Temporal Frequency Histogram.R",
  "Step 6 -- Temporal Trimming & Buffering.R",
  "Step 6a -- Sleep Statistics.R",
  "Step 7 -- Z-score Algorithm.R",
  "Step 8 -- L90 Algorithm.R",
  "Step 9 -- Rise Time Algorithm.R",
  "Step 10 -- Heart Rate Spike Predication.R",
  "Step 11 -- Sleep Stage Transition Predication.R",
  "Step 12 -- Graphic Comparison of 3 Algoirthms.R",
  "Step 13 -- All Descriptive Statistics.R",
  "Step 14 -- Individual Sensitivity Martrix.R",
  "Step 15 -- Clustering Maping and Phenotype Comparison.R",
  "Step 16 -- Clinic Sleep Quality vs Phenotype.R",
  "Step 17 -- ISRUC Reference Baseline & Decouopling Ratio.R",
  "Step 18 -- Soundless Sessional Baseline.R",
  "Step 19 -- Temporal Window Sensitivity Analysis.R"
)

# Execute scripts sequentially within a protective error-catching matrix
for (i in seq_along(scripts_to_execute)) {
  current_script_name <- scripts_to_execute[i]
  script_relative_path <- here("scripts", current_script_name)
  
  cat("\n------------------------------------------------------------------------\n")
  cat(sprintf("[%02d/%02d] RUNNING: %s\n", i, length(scripts_to_execute), current_script_name))
  cat("------------------------------------------------------------------------\n")
  
  if (!file.exists(script_relative_path)) {
    sink() # Safeguard: close the file connection before throwing a hard error
    stop(sprintf("Pipeline Halted: Script asset missing at endpoint -> %s", script_relative_path))
  }
  
  # Execute script and gracefully intercept downstream failures
  pipeline_exception <- tryCatch({
    source(script_relative_path, local = FALSE)
    NULL
  }, error = function(e) {
    e
  })
  
  if (!is.null(pipeline_exception)) {
    cat(sprintf("\n=> !!! CRITICAL DOWNSTREAM SCRIPT FAILURE IN: %s !!!\n", current_script_name))
    cat("System Error Message: ", pipeline_exception$message, "\n")
    cat("------------------------------------------------------------------------\n")
    sink() # Safely close the active log file link
    stop("Pipeline execution halted due to a critical downstream script failure.")
  }
}

cat("\n=======================================\n")
cat(" ALL SCRIPTS WERE EXECUTED SUCCESSFULY\n")
cat("=======================================\n")
cat("Master Console Log Exported to:\n => ", console_log_path, "\n")

# --- STOP CONSOLE INTERCEPTION ---
sink()
