### --- STEP 18: SOUNDLESS SESSIONAL BASELINE ---

library(data.table)
library(here)

cat("\n--- STEP 18: SOUNDLESS SESSIONAL BASELINE ---\n")

# 1. LOAD DATA FROM STEP 11
cat("Loading final mapped data from Step 11...\n")
dt <- fread(here("data", "processed", "step_11_FINAL_MAPPED_DATA.csv"))

# Ensure data sorting consistency before sequential shifting
setorder(dt, uuid, timestamp)

# 2. CALCULATE BASELINE METRICS PER SESSION (Isolating Session Boundaries)
cat("Calculating spontaneous sessional baseline metrics...\n")
soundless_baseline <- dt[, .(
  Total_Hrs       = (.N * 11) / 3600,
  Total_HR_Spikes = sum((heartRate - data.table::shift(heartRate)) >= 5, na.rm = TRUE),
  Total_Shifts    = sum(stage_shift == TRUE, na.rm = TRUE)
), by = .(participant_id, uuid)]

# 3. NORMALIZE TO HOURLY RATES (Matching historical decimal rounding definitions)
soundless_baseline[, `:=`(
  Baseline_HR_Spikes_Hr = round(Total_HR_Spikes / Total_Hrs, 2),
  Baseline_Shifts_Hr    = round(Total_Shifts / Total_Hrs, 2)
)]

# 4. CALCULATE MEAN PER PARTICIPANT (FILTERING NAs DEFENSIVELY)
participant_summary <- soundless_baseline[!is.na(participant_id), .(
  Mean_Baseline_HR_Hr       = mean(Baseline_HR_Spikes_Hr, na.rm = TRUE),
  Mean_Baseline_Shifts_Hr   = mean(Baseline_Shifts_Hr, na.rm = TRUE),
  Baseline_Decoupling_Ratio = round(mean(Baseline_HR_Spikes_Hr, na.rm = TRUE) / (mean(Baseline_Shifts_Hr, na.rm = TRUE) + 0.01), 2)
), by = .(participant_id = as.character(participant_id))]

# Ensure alphabetical/numerical sorting stability before table display
participant_summary[, participant_id_num := as.numeric(participant_id)]
setorder(participant_summary, participant_id_num)
participant_summary[, participant_id_num := NULL]

# 5. GLOBAL MEAN METRICS (Shielded with na.rm = TRUE)
cat("\n--- STEP 18: SOUNDLESS PARTICIPANT BASELINE SUMMARY ---\n")
print(participant_summary)

cat("\nMean Spontaneous HR Spikes/Hr (Soundless): ", mean(participant_summary$Mean_Baseline_HR_Hr, na.rm = TRUE))
cat("\nMean Spontaneous Shifts/Hr (Soundless):    ", mean(participant_summary$Mean_Baseline_Shifts_Hr, na.rm = TRUE))
cat("\nMean Baseline Decoupling Ratio (Soundless):", mean(participant_summary$Baseline_Decoupling_Ratio, na.rm = TRUE), "\n")

# 6. SAVE TO PROCESSED DIRECTORY VIA STANDARDIZED PATH CODES
fwrite(participant_summary, here("data", "processed", "step_18_SOUNDLESS_SESSIONAL_BASELINES.csv"))

cat("\nSessional baselines saved to data/processed/\n")
cat("\nStep 18 Complete.\n")