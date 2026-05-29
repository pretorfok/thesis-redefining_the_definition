### --- STEP 6a: SLEEP STATISTICS ---

library(data.table)
library(here)

cat("\n--- STEP 6a: SLEEP STATISTICS ---\n")

# 1. LOAD DATA
dt <- fread(here("data", "processed", "step_6_TRIMMED_ANALYSIS_DATASET.csv"))

# 2. GENERATE UNIQUE SESSION IDS
setorder(dt, uuid, timestamp)
dt[, gap := timestamp - shift(timestamp), by = uuid]
dt[is.na(gap), gap := 0]
dt[, session_id := cumsum(gap > 14400000), by = uuid]
dt[, unique_session_key := paste(uuid, session_id, sep = "_")]

# 3. CALCULATE NIGHTLY STATISTICS PER SESSION
nightly_stats <- dt[, .(
  recording_hrs = (max(as.numeric(timestamp)) - min(as.numeric(timestamp))) / (1000 * 60 * 60),
  n_total = .N,
  n_light = sum(sleepStage == 1, na.rm = TRUE),
  n_deep  = sum(sleepStage == 2, na.rm = TRUE),
  n_rem   = sum(sleepStage == 3, na.rm = TRUE)
), by = .(unique_session_key)]

# 4. APPLY REMAINDER LOGIC SAFE FROM DIVISION-BY-ZERO
nightly_stats[, n_wake := n_total - (n_light + n_deep + n_rem)]

# Initialize columns as safe numerics
nightly_stats[, `:=`(light_p = 0, deep_p = 0, rem_p = 0, waso_p = 0, efficiency = 0, sleeping_hrs = 0)]

# Calculate only where valid observations exist to prevent NaN propagation
nightly_stats[n_total > 0, `:=`(
  light_p      = (n_light / n_total) * 100,
  deep_p       = (n_deep / n_total) * 100,
  rem_p        = (n_rem / n_total) * 100,
  waso_p       = (n_wake / n_total) * 100,
  efficiency   = ((n_light + n_deep + n_rem) / n_total) * 100,
  sleeping_hrs = recording_hrs * ((n_light + n_deep + n_rem) / n_total)
)]

# 5. GENERATE FINAL TABLE FOR THESIS
final_table <- data.frame(
  Metric = c("Nightly Total Recording Time (hrs)", "Actual Sleeping Time (hrs)", "Overall Sleep Efficiency (%)", "Light Sleep (%)", "Deep Sleep (%)", "REM Sleep (%)", "Wake After Sleep Onset (%)"),
  Mean = round(c(
    mean(nightly_stats$recording_hrs, na.rm = TRUE),
    mean(nightly_stats$sleeping_hrs, na.rm = TRUE),
    mean(nightly_stats$efficiency, na.rm = TRUE),
    mean(nightly_stats$light_p, na.rm = TRUE),
    mean(nightly_stats$deep_p, na.rm = TRUE),
    mean(nightly_stats$rem_p, na.rm = TRUE),
    mean(nightly_stats$waso_p, na.rm = TRUE)
  ), 2),
  SD = round(c(
    sd(nightly_stats$recording_hrs, na.rm = TRUE),
    sd(nightly_stats$sleeping_hrs, na.rm = TRUE),
    sd(nightly_stats$efficiency, na.rm = TRUE),
    sd(nightly_stats$light_p, na.rm = TRUE),
    sd(nightly_stats$deep_p, na.rm = TRUE),
    sd(nightly_stats$rem_p, na.rm = TRUE),
    sd(nightly_stats$waso_p, na.rm = TRUE)
  ), 2)
)

# 6. OUTPUT RESULTS
print("--- Final Sleep Metrics Table ---")
print(final_table, row.names = FALSE)

cat("\n--- Verification Summary ---")
cat("\nTotal Sessions Analyzed:       ", nrow(nightly_stats))
cat("\nCumulative Recording Time:     ", round(sum(nightly_stats$recording_hrs), 2), "hours")
cat("\nCumulative Sleeping Time:      ", round(sum(nightly_stats$sleeping_hrs), 2), "hours")
cat("\nCheck (Light+Deep+REM+Wake %):", round(mean(nightly_stats$light_p + nightly_stats$deep_p + nightly_stats$rem_p + nightly_stats$waso_p), 2), "%\n")

# Export table cleanly to data/processed folder using pure here() paths
fwrite(final_table, here("data", "processed", "step_6a_sleep_macrostructure_table.csv"))

cat("\n--- Step 6a Complete ---\n")