### --- STEP 2 GHOST REMOVAL ---

library(data.table)
library(here) 

### 1. SETUP
input_file <- here("data", "processed", "step_1_merged.csv")

### 2. LOAD DATA
cat("Loading merged data from Step 1...\n")
dt <- fread(input_file)
initial_n <- nrow(dt)

### 3. PROCESSING

# A. Neutralize Artifacts
dt[, heartRate := as.numeric(heartRate)]
dt[heartRate <= 0, heartRate := NA]

# B. Ghost Removal
cat("Removing ghost rows...\n")
dt[, has_valid_hr := !is.na(heartRate)]
dt[, any_valid_exists := any(has_valid_hr), by = .(uuid, timestamp)]
dt_no_ghosts <- dt[!(any_valid_exists == TRUE & has_valid_hr == FALSE)]
ghosts_removed_n <- initial_n - nrow(dt_no_ghosts)

# C. Flag Remaining Hard Conflicts
cat("Flagging hard conflicts...\n")
dt_no_ghosts[, is_hard_conflict := .N > 1, by = .(uuid, timestamp)]
hard_conflicts_dt <- dt_no_ghosts[is_hard_conflict == TRUE]

### 4. EXPORT TO PROCESSED DIRECTORY
cat("Saving cleansed data and audit...\n")
output_data_path <- here("data", "processed", "step_2_cleansed_merged.csv")
fwrite(dt_no_ghosts, output_data_path)

### 5. AUDIT REPORT
if (nrow(hard_conflicts_dt) > 0) {
  core_cols <- c("uuid", "timestamp", "dB", "heartRate", "sleepStage")
  available_cols <- intersect(names(hard_conflicts_dt), core_cols)
  
  # Keeping the audit data table inside data/processed alongside the step 2 dataset
  output_audit_path <- here("data", "processed", "step_2_hard_conflicts_audit.csv")
  fwrite(hard_conflicts_dt[, ..available_cols], output_audit_path)
  
  cat("\n--- STEP 2: AUDIT COMPLETE ---\n")
  cat("1. Input Rows (from Step 1): ", initial_n, "\n")
  cat("2. Ghosted Data Removed:    ", ghosts_removed_n, "\n")
  cat("3. Hard Conflicts Found:     ", nrow(hard_conflicts_dt), "\n")
  cat("Cleansed file saved to:      ", output_data_path, "\n")
  cat("Audit report saved to:       ", output_audit_path, "\n")
} else {
  cat("\nNo hard conflicts found!\n")
}

cat("\n--- Step 2 Complete ---\n") 