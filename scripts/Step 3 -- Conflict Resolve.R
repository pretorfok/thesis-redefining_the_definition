### --- STEP 3: CONFLICT RESOLUTION ---

library(data.table)
library(here)

cat("\n--- STEP 3: CONFLICT RESOLUTION ---\n")

### --- HELPER: ENERGETIC MEAN FOR dB ---
# This calculates the logarithmic average (L_Aeq) rather than a simple arithmetic mean
energetic_mean_db <- function(db_vector) {
  if (all(is.na(db_vector))) return(as.numeric(NA))
  db_vector <- db_vector[!is.na(db_vector)]
  # Uses 10*log10 for power/energy averaging
  as.numeric(10 * log10(mean(10^(db_vector / 10))))
}

### 1. SETUP RELATIVE PATHS
input_file <- here("data", "processed", "step_2_cleansed_merged.csv")

### 2. LOAD DATA
cat("Loading cleansed data from Step 2...\n")
dt <- fread(input_file)

# Standardize types safely using suppressWarnings to convert textual artifacts to NAs
cat("Standardizing vector classes safely...\n")
dt[, dB        := suppressWarnings(as.numeric(dB))]
dt[, heartRate := suppressWarnings(as.numeric(heartRate))]

total_rows_before <- nrow(dt)

### 3. GLOBAL RESOLUTION (The "Squash")
cat("Resolving hard conflicts and squashing to unique timestamps...\n")
final_dt <- dt[, .(
  dB         = if(.N > 1) energetic_mean_db(dB) else dB[1],
  heartRate  = if(.N > 1) mean(heartRate, na.rm = TRUE) else heartRate[1],
  sleepStage = sleepStage[1] # Taking the first recorded stage if conflicted
), by = .(uuid, timestamp)]

total_rows_after <- nrow(final_dt)

### 4. EXPORT TO PROCESSED DIRECTORY
cat("Saving final resolved dataset...\n")
output_file_path <- here("data", "processed", "step_3_resolved_merged.csv")
fwrite(final_dt, output_file_path)

### 5. FINAL RESOLUTION REPORT
rows_removed <- total_rows_before - total_rows_after
cat("\n--- STEP 3: RESOLUTION AUDIT REPORT ---\n")
cat("1. Total Rows Before Resolution: ", total_rows_before, "\n")
cat("2. Total Rows After Resolution:  ", total_rows_after, "\n")
cat("3. Total Redundant Rows Removed: ", rows_removed, "\n")
cat("---------------------------------------\n")
cat("Final Output saved to:           ", output_file_path, "\n")

cat("\n--- Step 3 Complete ---\n") 