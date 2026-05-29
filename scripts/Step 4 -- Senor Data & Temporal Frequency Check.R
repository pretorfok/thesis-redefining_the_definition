### --- STEP 4: GLOBAL SENSOR INTEGRITY & TEMPORAL CHECK ---

library(data.table)
library(ggplot2)
library(here)

cat("\n--- STEP 4: GLOBAL SENSOR INTEGRITY & TEMPORAL CHECK ---\n")

# 1. SETUP PATHS
master_path <- here("data", "processed", "step_3_resolved_merged.csv")

# 2. LOAD DATA
master <- fread(master_path)

# --- INITIAL VOLUME CHECK ---
initial_row_count  <- nrow(master)
initial_uuid_count <- uniqueN(master$uuid)
cat("--- DATA FLOW START ---\n")
cat("Initial Observations:", initial_row_count, "\n")
cat("Initial Recording Sessions (UUIDs):", initial_uuid_count, "\n\n")

# 3. CONVERT AND SORT
master[, timestamp_dt := as.POSIXct(as.numeric(timestamp) / 1000, origin = "1970-01-01", tz = "UTC")]
setkey(master, uuid, timestamp_dt)

# =====================================================================
# PART A: GLOBAL TRIPLE-CHANNEL INTEGRITY AUDIT
# =====================================================================
cat("Starting Global Integrity Audit...\n")
integrity_audit <- master[, .(
  sd_db    = sd(dB, na.rm = TRUE),
  sd_hr    = sd(heartRate, na.rm = TRUE),
  n_stages = uniqueN(sleepStage)
), by = uuid]

# Filter Logic
integrity_audit[, is_failed := (sd_db < 0.1 | is.na(sd_db) | sd_hr < 0.1 | is.na(sd_hr) | n_stages < 2)]

# Separate surviving and failed UUIDs
valid_uuids  <- integrity_audit[is_failed == FALSE, uuid]
failed_uuids <- integrity_audit[is_failed == TRUE, uuid]

# Perform the filter
master_clean <- master[uuid %in% valid_uuids]

# =====================================================================
# PART B: TEMPORAL DENSITY AUDIT (On Validated Data)
# =====================================================================
cat("Running stabilized temporal density audit...\n")

master_clean[, gap := as.numeric(difftime(timestamp_dt, data.table::shift(timestamp_dt, fill = timestamp_dt[1]), units = "secs")), by = uuid]

# Safe filter catch to overwrite any internal telemetry anomalies
master_clean[is.na(gap), gap := 0]

# =====================================================================
# PART C: FINAL RESULTS & EXPORT
# =====================================================================
final_row_count    <- nrow(master_clean)
filtered_row_count <- initial_row_count - final_row_count
cat("\n--- DATA FLOW SUMMARY ---\n")
cat("Observations at Start:    ", initial_row_count, "\n")
cat("Observations Filtered Out:", filtered_row_count, " (", round((filtered_row_count/initial_row_count)*100, 1), "%)\n")
cat("Observations Kept:         ", final_row_count, "\n\n")

cat("UUIDs at Start:           ", initial_uuid_count, "\n")
cat("UUIDs Filtered Out:       ", length(failed_uuids), "\n")
cat("UUIDs Kept (Valid):       ", length(valid_uuids), "\n")

# Final Exports using pure here() paths to keep execution portable
fwrite(master_clean,    here("data", "processed", "step_4_CLEAN_CONSOLIDATED_DATA.csv"))
fwrite(integrity_audit, here("data", "processed", "step_4_global_integrity_report.csv"))

cat("\nStep 4 Complete.\n")