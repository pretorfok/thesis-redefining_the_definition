########################################################################
# SCRIPT SOURCE: Step 14 -- Individual Sensitivity Martrix.R
########################################################################
# --- STEP 14: INDIVIDUAL SENSITIVITY MATRIX (HISTORICAL CALIBRATION) ---

cat("\n--- STEP 14: AGGREGATING SENSITIVITY BY PARTICIPANT (OLD METHOD) ---\n")
library(data.table)
library(here)

# 1. LOAD DATA RELATIVELY FROM STEP 11
dt <- fread(here("data", "processed", "step_11_FINAL_MAPPED_DATA.csv"))

# 2. STEP A: COMPUTE HOURLY SPONTANEOUS BASELINES NIGHT-BY-NIGHT (EXACT OLD LOGIC)
# Grouping by participant and individual session uuid isolates each night cleanly
# We calculate standard row-to-row differences here to match the historical baseline definitions
nightly_baselines <- dt[!is.na(participant_id), .(
  Total_Hrs             = (.N * 11) / 3600,
  Spontaneous_HR_Spikes = sum((heartRate - data.table::shift(heartRate)) >= 5, na.rm = TRUE),
  Spontaneous_Shifts    = sum(stage_shift == TRUE, na.rm = TRUE)
), by = .(participant_id, uuid)]

# Normalize raw counts into exact historical hourly rates per night
nightly_baselines[, `:=`(
  Baseline_HR_Spikes_Hr = Spontaneous_HR_Spikes / Total_Hrs,
  Baseline_Shifts_Hr    = Spontaneous_Shifts / Total_Hrs
)]

# STEP B: AGGREGATE NIGHTS INTO A PARTICIPANT-WIDE AVERAGE (MATCHING THE ORIGINAL AVERAGES)
participant_baselines <- nightly_baselines[, .(
  Mean_Baseline_HR_Hr       = mean(Baseline_HR_Spikes_Hr, na.rm = TRUE),
  Mean_Baseline_Shifts_Hr = mean(Baseline_Shifts_Hr, na.rm = TRUE),
  Baseline_Decoupling_Ratio = round(
    mean(Baseline_HR_Spikes_Hr, na.rm = TRUE) / (mean(Baseline_Shifts_Hr, na.rm = TRUE) + 0.01), 
    2
  )
), by = .(participant_id = as.character(participant_id))]

# 3. CALCULATION FUNCTION FOR OPERATIONAL RISK RATIOS (HOURLY WITH SAFEGUARDS)
get_participant_rr_hourly <- function(p_id, full_dt) {
  sub <- full_dt[participant_id == p_id]
  if (nrow(sub) == 0) return(NULL)
  
  calc_rr_hourly <- function(outcome_col, noise_col) {
    # Isolate Noise vs Quiet epochs safely using native row filters
    noise_epochs <- sub[get(noise_col) == TRUE]
    quiet_epochs <- sub[get(noise_col) == FALSE]
    
    # RESTORED SAFEGUARD: Minimum event threshold to avoid extreme division outliers
    if (nrow(noise_epochs) < 5 | nrow(quiet_epochs) < 5) return(1.00)
    
    # Rate = (Events) / (Total Observation Time in Hours)
    rate_noise <- sum(noise_epochs[[outcome_col]], na.rm = TRUE) / (nrow(noise_epochs) * 11 / 3600)
    rate_quiet <- sum(quiet_epochs[[outcome_col]], na.rm = TRUE) / (nrow(quiet_epochs) * 11 / 3600)
    
    # Handle zeros: if quiet is 0, cap at 25.0 for clustering stability
    if (rate_quiet == 0) return(if (rate_noise > 0) 25.00 else 1.00)
    
    # Risk Ratio (RR) calculation capped defensively at 25.0
    res <- rate_noise / rate_quiet
    return(round(pmin(res, 25.0), 2))
  }
  
  return(data.table(
    participant_id = as.character(p_id),
    HR_RR_R25      = as.numeric(calc_rr_hourly("hr_spike", "flag_R25")),
    Brain_RR_R25   = as.numeric(calc_rr_hourly("stage_shift", "flag_R25"))
  ))
}

# 4. EXECUTE RISK RATIO CALCULATIONS
p_list <- unique(dt[!is.na(participant_id), participant_id])
risk_ratios <- rbindlist(lapply(p_list, get_participant_rr_hourly, full_dt = dt))

# 5. INTEGRATE RISK RATIOS WITH AVERAGED PARTICIPANT BASELINES
final_comparison <- merge(risk_ratios, participant_baselines, by = "participant_id", all.x = TRUE)

# Enforce sorting stability before table display
final_comparison[, participant_id_num := as.numeric(participant_id)]
setorder(final_comparison, participant_id_num)
final_comparison[, participant_id_num := NULL]

# 6. SAVE TO THE RELATIVE PROCESSED DATA PATHWAY 
fwrite(final_comparison, here("data", "processed", "step_14_INDIVIDUAL_PROFILES_HOURLY.csv"))

# 7. CONSOLE VERIFICATION VIEWPORT
print(final_comparison)

cat("\nStep 14 Complete. Historical baseline variables re-aligned successfully.\n")