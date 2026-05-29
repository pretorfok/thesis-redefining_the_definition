### --- STEP 1: GLOBAL MERGE & DE-DUPLICATION ---

library(data.table) 
library(stringi)
library(here) 

### 1. Setup relative paths
input_folder <- here("data", "raw", "soundless-data-no-geo")

### 2. Map Sibling Files
all_files <- list.files(path = input_folder, pattern = "\\.csv$", full.names = TRUE)
file_meta <- data.table(full_path = all_files)
file_meta[, base_id := stri_replace_last_regex(basename(full_path), "(_\\d+)?\\.csv$", "")] 
unique_sessions <- unique(file_meta$base_id)

### 3. Storage for Global Merge
all_unique_data <- list() 
report1         <- list()

### 4. Process and Audit
for (i in seq_along(unique_sessions)) { 
  current_id <- unique_sessions[i] 
  paths <- file_meta[base_id == current_id, full_path]
  
  ### Load and stack all versions for this specific session ID
  dt_stack <- rbindlist(lapply(paths, function(x) fread(file = x, fill = TRUE)), use.names = TRUE, fill = TRUE)
  
  ### Audit: Raw Count before cleaning
  raw_n <- nrow(dt_stack)
  
  ### STRICT DE-DUPLICATION:
  cols_to_check <- intersect(colnames(dt_stack), c("uuid", "timestamp", "dB", "heartRate", "sleepStage")) 
  dt_unique <- unique(dt_stack, by = cols_to_check)
  unique_n <- nrow(dt_unique)
  
  ### Store for final report and final merge
  report1[[current_id]] <- data.table(session = current_id, raw = raw_n, unique = unique_n) 
  all_unique_data[[current_id]] <- dt_unique
  
  cat("Processed session:", current_id, "| Removed", raw_n - unique_n, "duplicates.\n") 
}

### 5. Final Consolidation
cat("Finalizing global merge...\n") 
final_merged_dt <- rbindlist(all_unique_data, use.names = TRUE, fill = TRUE)

### Final "Safety" De-duplication:
final_merged_dt <- unique(final_merged_dt, by = c("uuid", "timestamp", "dB", "heartRate", "sleepStage"))

### 6. Save Single Output File (Moved to data/processed/)
output_file_path <- here("data", "processed", "step_1_merged.csv")
fwrite(final_merged_dt, output_file_path)

### 7. Summary Report
final_report1 <- rbindlist(report1) 
cat("Total Raw Rows Processed: ", sum(final_report1$raw), "\n")
cat("Total Unique Rows Saved:   ", nrow(final_merged_dt), "\n")
cat("Total Duplicates Removed:  ", sum(final_report1$raw) - nrow(final_merged_dt), "\n") 
cat("File saved to:", output_file_path, "\n")

cat("\n--- Step 1 Complete ---\n") 