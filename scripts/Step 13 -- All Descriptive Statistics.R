### --- STEP 13: ALL DESCRIPTIVE STATISTICS ---

cat("\n--- STEP 13: ALL DESCRIPTIVE STATISTICS ---\n")
library(data.table)
library(here)

# 1. LOAD DATA
dt <- fread(here("data", "processed", "step_9_SENSITIVITY_MASTER.csv"))
setkey(dt, uuid, timestamp_dt)

# 2. DATA PREPARATION
dt[, sleep_label := fcase(
  sleepStage == -1, "Wake",
  sleepStage == 1,  "Light",
  sleepStage == 2,  "Deep",
  sleepStage == 3,  "REM"
)]

# 3. STUDY-WIDE CUMULATIVE SCALE
cat("\n--- STUDY-WIDE CUMULATIVE SCALE ---\n")
total_observations <- nrow(dt)

# Calculate hours based only on actual sleep (non-wake) safely
sleep_rows <- nrow(dt[sleepStage %in% c(1, 2, 3)])
total_sleep_hrs <- (sleep_rows * 11) / 3600
cat("Total Sleep Time (TST) used for density:", round(total_sleep_hrs, 2), "hours\n")

# Prevent down-stream script execution crashes if TST happens to be zero
if (total_sleep_hrs == 0) total_sleep_hrs <- 0.001

# 4. AMBIENT NOISE ENVIRONMENT (3.1.2) - LOCALIZED NIGHTLY PROFILE WITH L50
cat("\n--- AMBIENT NOISE ENVIRONMENT (MEAN PER NIGHT) ---\n")

# Step A: Calculate individual percentiles and individual dynamic range per night
nightly_noise <- dt[, .(
  L_10     = quantile(dB, 0.90, na.rm = TRUE),
  L_50     = quantile(dB, 0.50, na.rm = TRUE),
  L_90     = quantile(dB, 0.10, na.rm = TRUE),
  Night_DR = quantile(dB, 0.90, na.rm = TRUE) - quantile(dB, 0.10, na.rm = TRUE)
), by = uuid]

# Step B: Aggregate across nights
noise_summary <- nightly_noise[, .(
  Metric = c("Mean L10 (Peaks)", "Mean L50 (Typical Loudness)", "Mean L90 (Background Floor)", "Mean Nightly Dynamic Range"),
  Value  = round(c(
    mean(L_10, na.rm = TRUE),
    mean(L_50, na.rm = TRUE),
    mean(L_90, na.rm = TRUE),
    mean(Night_DR, na.rm = TRUE)
  ), 2)
)]
print(noise_summary)

# 5. ALGORITHM PERFORMANCE: FLAGS vs INCIDENTS
cat("\n--- ALGORITHM PERFORMANCE: FLAGS vs INCIDENTS ---\n")
flag_cols <- grep("^flag_", names(dt), value = TRUE)

algo_summary <- lapply(flag_cols, function(f) {
  # High-speed data.table alternative to replace the heavy unvectorized rle() loop
  total_flags     <- sum(dt[, get(f)] == TRUE, na.rm = TRUE)
  total_incidents <- sum(dt[, get(f)] == TRUE & data.table::shift(dt[, get(f)], fill = FALSE) == FALSE, na.rm = TRUE)
  
  avg_dur       <- if (total_incidents > 0) (total_flags * 11) / total_incidents else 0
  obs_per_event <- if (total_incidents > 0) total_flags / total_incidents else 0
  
  data.table(
    Algorithm     = f,
    Density_ObsHr = round(total_flags / total_sleep_hrs, 2),
    Freq_EventsHr = round(total_incidents / total_sleep_hrs, 2),
    Avg_Dur_Sec   = round(avg_dur, 2),
    Obs_Per_Event = round(obs_per_event, 2)
  )
})
algo_report <- rbindlist(algo_summary)
print(algo_report)

# 6. PHYSIOLOGICAL RESPONSE INDICATORS
cat("\n--- PHYSIOLOGICAL RESPONSE INDICATORS ---\n")

# Identifying transitions cleanly using native data.table shifting rules
dt[, stage_change := (sleepStage != data.table::shift(sleepStage, fill = sleepStage[1])), by = uuid]

physio_stats <- dt[, .(
  Mean_HR      = mean(heartRate, na.rm = TRUE),
  SD_HR        = sd(heartRate, na.rm = TRUE),
  Trans_per_Hr = sum(stage_change, na.rm = TRUE) / (.N * 11 / 3600)
), by = uuid]

# Filter out empty entries or NAs before building final summary averages
physio_summary <- physio_stats[!is.na(Mean_HR), .(
  Metric = c("Mean Heart Rate (BPM)", "HR Standard Deviation", "Stage Transitions per Hour"),
  Mean   = round(c(mean(Mean_HR), mean(SD_HR), mean(Trans_per_Hr)), 2),
  SD     = round(c(sd(Mean_HR),  sd(SD_HR),  sd(Trans_per_Hr)),  2)
)]
print(physio_summary)

# 7. AUTOMATED EXPORT (Aligning with directory specifications)
fwrite(noise_summary,  here("data", "processed", "step_13_noise_environment_summary.csv"))
fwrite(algo_report,    here("data", "processed", "step_13_algorithm_performance_summary.csv"))
fwrite(physio_summary, here("data", "processed", "step_13_physiological_response_summary.csv"))

cat("\n--- Summaries exported to data/processed/ ---\n")
cat("\nSTEP 13 REPORT COMPLETE\n")