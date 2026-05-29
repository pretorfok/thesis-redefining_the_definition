### --- STEP 7: Z-SCORE ALGORITHM ---

cat("\n--- STEP 7: Z-SCORE ALGORITHM ---\n")
library(data.table)
library(here)

# 1. LOAD DATA RELATIVELY FROM STEP 6
dt <- fread(here("data", "processed", "step_6_TRIMMED_ANALYSIS_DATASET.csv"))
dt[, timestamp_dt := as.POSIXct(timestamp_dt)]
setkey(dt, uuid, timestamp_dt)

# 2. CALCULATE Z-SCORES VIA FAST VECTORIZED C-ENGINE
cat("Calculating Global and Rolling Z-scores...\n")

# Global Z-Score: Grouped by session UUID
dt[, `:=`(g_mu = mean(dB, na.rm = TRUE), g_sd = sd(dB, na.rm = TRUE)), by = uuid]
dt[, z_global := (dB - g_mu) / g_sd]

# High-Speed Rolling Z-Score: Window = 16 samples (~3 minutes)
k_val <- 16
dt[, r_mu := data.table::frollmean(dB, n = k_val, fill = NA, align = "right"), by = uuid]
dt[, r_sq := data.table::frollmean(dB^2, n = k_val, fill = NA, align = "right"), by = uuid]
dt[, r_sd := sqrt(pmax(0, r_sq - r_mu^2)), by = uuid]
dt[, z_rolling := (dB - r_mu) / r_sd]

# Handle NAs from rolling windows or empty data boundaries defensively
dt[is.na(z_global), z_global := 0]
dt[is.na(z_rolling), z_rolling := 0]

# Generate binary threshold flags across all evaluation parameters
dt[, flag_G25 := z_global > 2.5]
dt[, flag_G30 := z_global > 3.0]
dt[, flag_G40 := z_global > 4.0]
dt[, flag_R25 := z_rolling > 2.5]
dt[, flag_R30 := z_rolling > 3.0]
dt[, flag_R40 := z_rolling > 4.0]

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
flag_names <- c("flag_G25", "flag_G30", "flag_G40", "flag_R25", "flag_R30", "flag_R40")

for (fn in flag_names) {
  m <- get_comprehensive_metrics(dt[[fn]], dt$uuid, dt$timestamp_dt)
  meth  <- if (grepl("G", fn)) "Global" else "Rolling"
  thresh <- switch(fn, "flag_G25" = 2.5, "flag_G30" = 3.0, "flag_G40" = 4.0, "flag_R25" = 2.5, "flag_R30" = 3.0, "flag_R40" = 4.0)
  
  results[[fn]] <- data.table(
    Method = meth,
    Threshold = thresh,
    Density_ObsHr = m$dens,
    Freq_EventsHr = m$freq,
    Avg_Dur_Sec = m$dur,
    Obs_Per_Event = m$cnt
  )
}

final_report <- rbindlist(results)
cat("\n--- Z-SCORE SENSITIVITY & CHARACTERIZATION ---\n")
print(final_report[order(Method, Threshold)])

# 5. SAVE MASTER DATASET TO THE SANDBOX PATH
fwrite(dt, here("data", "processed", "step_7_SENSITIVITY_MASTER.csv"))
cat("\nMaster dataset successfully saved with Z-score flags inside data/processed/.\n")

cat("\n--- Step 7 Complete ---\n")