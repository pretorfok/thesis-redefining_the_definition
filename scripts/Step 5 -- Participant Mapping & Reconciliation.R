### --- STEP 5: IDENTITY MAPPING & RECONCILIATION ---

library(data.table)
library(here)

cat("\n--- STEP 5: IDENTITY MAPPING & RECONCILIATION ---\n")

# 1. SETUP PATHS
clean_data_path <- here("data", "processed", "step_4_CLEAN_CONSOLIDATED_DATA.csv")
history_dir     <- here("data", "raw", "soundless-history")
output_mapping  <- here("data", "processed", "step_5_MAPPED_GOLDEN_DATASET.csv")

# 2. LOAD VALIDATED DATA
cat("Loading validated global corpus...\n")
dt <- fread(clean_data_path)
valid_uuids <- unique(dt$uuid)

# 3. SCAN METADATA 
# Pull all files in the folder globally
all_found_files <- list.files(history_dir, full.names = TRUE)

# Filter out hidden OS operating files and R project tracking files defensively
history_files <- all_found_files[!grepl("(\\.DS_Store|Thumbs\\.db|\\.Rproj)$", basename(all_found_files), ignore.case = TRUE)]

if (length(history_files) == 0) {
  stop("Pipeline execution halted: No valid history metadata files found in data/raw/soundless-history/")
}

mapping_list <- lapply(history_files, function(f) {
  if (file.info(f)$isdir) return(NULL)
  
  # Safe tryCatch block handles unhandled permissions or binary read failures without crashing
  raw_lines <- tryCatch({
    readLines(f, warn = FALSE)
  }, error = function(e) {
    return(character(0))
  })
  
  if (length(raw_lines) == 0) {
    return(data.table(
      source_filename = basename(f),
      has_uuids = FALSE,
      uuids = list(NA_character_)
    ))
  }
  
  all_uuids <- tolower(trimws(unlist(strsplit(raw_lines, ","))))
  all_uuids <- all_uuids[all_uuids != ""]
  
  data.table(
    source_filename = basename(f),
    has_uuids = length(all_uuids) > 0,
    uuids = if(length(all_uuids) > 0) list(all_uuids) else list(NA_character_)
  )
})
meta_audit <- rbindlist(mapping_list, use.names = TRUE, fill = TRUE)

# 4. CALCULATE ATTRITION CATEGORIES
empty_meta_count <- nrow(meta_audit[has_uuids == FALSE])
potential_participants <- meta_audit[has_uuids == TRUE]

# Safely check matching array intersections
potential_participants[, is_valid := sapply(uuids, function(u) any(u %in% valid_uuids))]
invalidated_participants_count <- nrow(potential_participants[is_valid == FALSE])
active_participants_count      <- nrow(potential_participants[is_valid == TRUE])

# 5. ASSIGN FINAL IDs & MAP
active_participants <- potential_participants[is_valid == TRUE]
active_participants[, participant_id := seq_len(.N)]

# Create structural bridge for safe data.table matching
bridge <- active_participants[, .(uuid = unlist(uuids)), by = participant_id]
setDT(bridge)

# Force identical matching data formats to eliminate type collisions
dt[, uuid := as.character(uuid)]
bridge[, uuid := as.character(uuid)]

# Execute explicit high-speed merge join
dt <- merge(dt, bridge, by = "uuid", all.x = TRUE)

# 6. EXPORT
fwrite(dt, output_mapping)

# 7. FINAL TABLE DISPLAY
cat("\n======================================================\n")
cat("                DATA RECONCILIATION REPORT            \n")
cat("======================================================\n")
cat("PARTICIPANTS\n")
cat("Empty Metadata Files:                     ", empty_meta_count, "\n")
cat("Participants Matched but Invalidated:     ", invalidated_participants_count, "\n")
cat("Active Participants (Final Sample):       ", active_participants_count, "\n")
cat("Total Participants Accounted For:         ", nrow(meta_audit), "\n")
cat("------------------------------------------------------\n")
cat("RECORDED SESSIONS (UUIDs)\n")
cat("Sessions Mapped to Participants:          ", uniqueN(dt[!is.na(participant_id), uuid]), "\n")
cat("Orphaned Sessions (Unattributed):         ", uniqueN(dt[is.na(participant_id), uuid]), "\n")
cat("Total Validated Sessions:                 ", uniqueN(dt$uuid), "\n")
cat("------------------------------------------------------\n")
cat("OBSERVATIONS\n")
cat("Attributed Observations:                  ", nrow(dt[!is.na(participant_id)]), "\n")
cat("Unattributed Observations:                ", nrow(dt[is.na(participant_id)]), "\n")
cat("Total Consolidated Observations:          ", nrow(dt), "\n")
cat("======================================================\n")

cat("\nStep 5 Complete.\n")