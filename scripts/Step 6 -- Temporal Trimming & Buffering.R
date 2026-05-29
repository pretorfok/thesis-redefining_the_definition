### --- STEP 6: TEMPORAL TRIMMING & BUFFERING ---

library(data.table)
library(here)

cat("\n--- STEP 6: TEMPORAL TRIMMING & BUFFERING ---\n")

# 1. SETUP PATHS
input_path  <- here("data", "processed", "step_5_MAPPED_GOLDEN_DATASET.csv")
output_path <- here("data", "processed", "step_6_TRIMMED_ANALYSIS_DATASET.csv")

# 2. LOAD DATA
cat("Loading mapped data from Step 5...\n")
dt <- fread(input_path)

# 3. SORT
dt[, timestamp_dt := as.POSIXct(as.numeric(timestamp) / 1000, origin = "1970-01-01", tz = "UTC")]
setkey(dt, uuid, timestamp_dt)

# 4. SURGICAL TRIM (Grouped by UUID)
cat("Performing surgical trim on validated sessions safely...\n")

# Step A: Identify the exact positional boundaries of sleep per session
dt[, is_sleep := sleepStage %in% c(1, 2, 3)]
dt[, row_idx  := seq_len(.N), by = uuid]

boundaries <- dt[is_sleep == TRUE, .(
  first_s = min(row_idx),
  last_s  = max(row_idx)
), by = uuid]

# Step B: Safe join to filter data without bracket-loop omissions
dt <- merge(dt, boundaries, by = "uuid", all.x = TRUE)

# Filter: Keep rows that fall entirely within the sleep start/end envelope.
# If a session has zero sleep stages recorded, we preserve its structural rows.
trimmed_dt <- dt[is.na(first_s) | (row_idx >= first_s & row_idx <= last_s)]

# CRITICAL FIX: Properly bracketed assignment operator to remove helper tracking columns cleanly
trimmed_dt[, `:=`(is_sleep = NULL, row_idx = NULL, first_s = NULL, last_s = NULL)]

# 5. SUMMARY REPORT
cat("\n--- STEP 6: FINAL TRIMMING SUMMARY ---\n")
cat("Original Row Count: ", nrow(dt), "\n")
cat("Trimmed Row Count:  ", nrow(trimmed_dt), "\n")
cat("Rows Removed (Wake):", nrow(dt) - nrow(trimmed_dt), "\n")
cat("Total Sessions Kept:", uniqueN(trimmed_dt$uuid), "\n")
cat("Total Participants: ", uniqueN(trimmed_dt$participant_id, na.rm = TRUE), "\n")

# 6. EXPORT
fwrite(trimmed_dt, output_path)
cat("\nFinal Analytical Dataset saved to:", output_path, "\n")

cat("\n--- Step 6 Complete ---\n")