### --- STEP 9: RISE TIME ALGORITHM ---

cat("\n--- STEP 9: RISE TIME ALGORITHM ---\n")
library(data.table)
library(here)

# 1. LOAD DATA
dt <- fread(here("data", "processed", "step_8_SENSITIVITY_MASTER.csv"))
dt[, timestamp_dt := as.POSIXct(timestamp_dt)]
setkey(dt, uuid, timestamp_dt)

# 2. CALCULATE 11s RISE TIME (DELTA) WITH EXPLICIT BOUNDARY FILLING
cat("Calculating 11s Rise Time (Delta dB between consecutive samples)...\n")

# Using fill = dB[1] safely anchors the first row subtraction to its own value, yielding a true 0 delta
dt[, delta_dB := abs(dB - data.table::shift(dB, fill = dB[1])), by = uuid]

# Protect against any inner-loop missing telemetry anomalies
dt[is.na(delta_dB), delta_dB := 0]

# Generate binary dynamic delta jump threshold flags
dt[, flag_D05 := delta_dB > 5]
dt[, flag_D10 := delta_dB > 10]
dt[, flag_D15 := delta_dB > 15]

# 3. COMPREHENSIVE METRICS FUNCTION
get_comprehensive_metrics <- function(flag_vec, uuid_vec, time_vec) {
  total_hours <- length(flag_vec) / (3600 / 11)
  incidents <- data.table(u = uuid_vec, t = time_vec, is_flagged = flag_vec)[is_flagged == TRUE]
  
  if (nrow(incidents) > 0) {
    setkey(incidents, u, t)
    incidents[, diff_t := as.numeric(difftime(t, shift(t), units = "secs")), by = u]
    incidents[, event_id := cumsum(fifelse(is.na(diff_t) | diff_t > 55, 1, 0)), by = u]
    
    summary_dt <- incidents[, .(
      dur_sec = as.numeric(difftime(max(t), min(t), units = "secs")) + 11,
      obs_cnt = .N
    ), by = .(u, event_id)]
    
    total_flags  <- sum(summary_dt$obs_cnt)
    total_events <- nrow(summary_dt)
    
    dens <- total_flags / total_hours
    freq <- total_events / total_hours
    dur  <- mean(summary_dt$dur_sec)
    cnt  <- mean(summary_dt$obs_cnt)
    
    return(list(dens = round(dens, 2), freq = round(freq, 2), dur = round(dur, 2), cnt = round(cnt, 2)))
  } else {
    return(list(dens = 0, freq = 0, dur = 0, cnt = 0))
  }
}

# 4. GENERATE SUMMARY TABLE
results <- list()
flag_names <- c("flag_D05", "flag_D10", "flag_D15")

for (fn in flag_names) {
  m <- get_comprehensive_metrics(dt[[fn]], dt$uuid, dt$timestamp_dt)
  thresh_label <- switch(fn, "flag_D05" = ">5dB Jump", "flag_D10" = ">10dB Jump", "flag_D15" = ">15dB Jump")
  results[[fn]] <- data.table(
    Method = "Rise Time",
    Threshold = thresh_label,
    Density_ObsHr = m$dens,
    Freq_EventsHr = m$freq,
    Avg_Dur_Sec = m$dur,
    Obs_Per_Event = m$cnt
  )
}

final_report <- rbindlist(results)
cat("\n--- RISE TIME SENSITIVITY & CHARACTERIZATION ---\n")
print(final_report)

# 5. SAVE MASTER DATASET
fwrite(dt, here("data", "processed", "step_9_SENSITIVITY_MASTER.csv"))
cat("\nStep 9 Master saved successfully with flags of all 3 methods.\n")

cat("\n--- Step 9 Complete ---\n")