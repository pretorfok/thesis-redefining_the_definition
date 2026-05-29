### --- STEP 17: ISRUC REFERENCE BASELINE & DECOUPLING RATIO ---

library(data.table)
library(here)

cat("\n--- STEP 17: ISRUC REFERENCE BASELINE & DECOUPLING RATIO ---\n")

# 1. SETUP RELATIVE DIRECTORIES 
isruc_dir   <- here("data", "raw", "isruc-group-3")
isruc_files <- list.files(isruc_dir, pattern = "\\.csv$", full.names = TRUE)

# 2. CALCULATION FUNCTION
analyze_isruc <- function(file_path) {
  dt <- fread(file_path)
  
  # Ensure chronological order by Epoch
  setorder(dt, Epoch)
  
  # 3. DEFINE SPONTANEOUS EVENTS (30s Windows)
  # HR Spike: Current HR is 5+ BPM higher than the previous 30s epoch
  dt[, hr_spike := (HR - shift(HR)) >= 5]
  
  # Stage Shift: Check if current Stage is 'lighter' than the previous
  # (1-Deep, 2-REM, 3-Light, 4-Wake)
  dt[, stage_shift := Stage > shift(Stage)]
  
  # 4. TEMPORAL NORMALIZATION (Standardizing to "Per Hour")
  total_hrs <- (nrow(dt) * 30) / 3600
  
  # 5. CALCULATE METRICS
  n_hr_spikes <- sum(dt$hr_spike, na.rm = TRUE)
  n_shifts    <- sum(dt$stage_shift, na.rm = TRUE)
  
  return(data.table(
    participant_id   = gsub(".csv", "", basename(file_path)),
    Total_Hrs        = round(total_hrs, 2),
    HR_Spikes_Hr     = round(n_hr_spikes / total_hrs, 2),
    Shifts_Hr        = round(n_shifts / total_hrs, 2),
    # This is the critical "Standardized Decoupling Ratio"
    Decoupling_Ratio = round(n_hr_spikes / (n_shifts + 0.001), 2)
  ))
}

# 6. EXECUTE AND SAVE
if (length(isruc_files) > 0) {
  isruc_results <- rbindlist(lapply(isruc_files, analyze_isruc))
  fwrite(isruc_results, here("data", "processed", "step_17_ISRUC_BASELINE_RESULTS.csv"))
  
  # 7. SUMMARY REPORT FOR THESIS TABLE
  cat("\n--- STEP 17: ISRUC HEALTHY REFERENCE SUMMARY ---\n")
  print(isruc_results)
  cat("\nMean Spontaneous HR Spikes/Hr:   ", mean(isruc_results$HR_Spikes_Hr, na.rm = TRUE))
  cat("\nMean Spontaneous Shifts/Hr:      ", mean(isruc_results$Shifts_Hr, na.rm = TRUE))
  cat("\nMean Baseline Decoupling Ratio:  ", mean(isruc_results$Decoupling_Ratio, na.rm = TRUE), "\n")
} else {
  stop("Execution halted: No clean reference files found in data/raw/isruc-group-3/")
}

cat("\n--- STEP 17 COMPLETE ---\n")