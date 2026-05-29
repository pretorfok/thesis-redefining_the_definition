### --- STEP 8: L90 ALGORITHM ---

cat("\n--- STEP 8: L90 ALGORITHM ---\n")
library(data.table)
library(here)

# 1. LOAD DATA RELATIVELY FROM STEP 7
dt <- fread(here("data", "processed", "step_7_SENSITIVITY_MASTER.csv"))
dt[, timestamp_dt := as.POSIXct(timestamp_dt)]
setkey(dt, uuid, timestamp_dt)

# 2. CALCULATE L90 BASELINES
cat("Calculating Rolling L90 baselines ...\n")

# Window = 60 samples (~11 minutes) to evaluate the ambient background noise floor
k_val <- 60

# We compute a fast rolling mean and standard deviation window to statistically estimate 
# the 10th percentile (L90) via a standard Z-score offset (-1.282)
dt[, r_mean := data.table::frollmean(dB, n = k_val, fill = NA, align = "right"), by = uuid]
dt[, r_sq   := data.table::frollmean(dB^2, n = k_val, fill = NA, align = "right"), by = uuid]
dt[, r_sd   := sqrt(pmax(0, r_sq - r_mean^2)), by = uuid]

dt[, L90_baseline := r_mean - (1.282 * r_sd)]

# Clean up helper calculation tracking columns cleanly
dt[, `:=`(r_mean = NULL, r_sq = NULL, r_sd = NULL)]

# Handle NAs at the initial window boundaries using session global averages safely
dt[is.na(L90_baseline), L90_baseline := mean(dB, na.rm = TRUE), by = uuid]

# Create Binary Flags for L90 + Thresholds
dt[, flag_L10 := (dB - L90_baseline) > 10]
dt[, flag_L15 := (dB - L90_baseline) > 15]
dt[, flag_L20 := (dB - L90_baseline) > 20]

# 3. COMPREHENSIVE METRICS FUNCTION
get_comprehensive_metrics <- function(flag_vec, uuid_vec, time_vec) {
  total_hours <- length(flag_vec) / (3600 / 11)
  incidents <- data.table(u = uuid_vec, t = time_vec, is_flagged = flag_vec)[is_flagged == TRUE]
  
  if (nrow(incidents) > 0) {
    setkey(incidents, u, t)
    incidents[, diff_t := as.numeric(difftime(t, shift(t), units = "secs")), by = u]
    incidents[, event_id := cumsum(fifelse(is.na(diff_t) | diff_t > 55, 1, 0)), by = u]
    summary_dt = incidents[, .(
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
flag_names <- c("flag_L10", "flag_L15", "flag_L20")

for (fn in flag_names) {
  m <- get_comprehensive_metrics(dt[[fn]], dt$uuid, dt$timestamp_dt)
  thresh_label <- switch(fn, "flag_L10" = "10dB", "flag_L15" = "15dB", "flag_L20" = "20dB")
  results[[fn]] <- data.table(
    Method = "L90+",
    Threshold = thresh_label,
    Density_ObsHr = m$dens,
    Freq_EventsHr = m$freq,
    Avg_Dur_Sec = m$dur,
    Obs_Per_Event = m$cnt
  )
}

final_report <- rbindlist(results)
cat("\n--- L90 SENSITIVITY & CHARACTERIZATION ---\n")
print(final_report)

# 5. SAVE MASTER DATASET TO THE SANDBOX PATH
fwrite(dt, here("data", "processed", "step_8_SENSITIVITY_MASTER.csv"))
cat("\nStep 8 Master saved successfully with Z-score and L90 flags inside data/processed/.\n")

cat("\n--- Step 8 Complete ---\n")