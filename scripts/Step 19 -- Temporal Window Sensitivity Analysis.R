### --- STEP 19: TEMPORAL WINDOW SENSITIVITY ANALYSIS ---

cat("\n--- STEP 19: TEMPORAL WINDOW SENSITIVITY ANALYSIS ---\n")
library(data.table)
library(here)

# 1. LOAD DATA FROM STEP 11
master_path <- here("data", "processed", "step_11_FINAL_MAPPED_DATA.csv")
cat("Reading data frame from:", master_path, "...\n")
dt <- fread(master_path)
setkey(dt, uuid, timestamp_dt)

# 2. GENERATE PHYSIOLOGICAL OUTCOMES NATIVELY GROUPED BY SESSION
cat("Calculating instantaneous cortical and autonomic events safely...\n")
dt[, stage_shift := (stage_rank != data.table::shift(stage_rank)), by = uuid]
dt[, hr_spike    := (heartRate - data.table::shift(heartRate) >= 5), by = uuid]

# 3. PREP DATA FOR SENSITIVITY FILTER (Exclude Wake)
dt_sens <- dt[stage_rank < 4]

# 4. RUN INDEPENDENT TEMPORAL LOOK-FORWARD LOOPS
steps <- 1:5
results_list <- list()
cat("Executing independent, protocol-aligned sensitivity tests...\n")

for (i in steps) {
  # Cardiovascular windows: 1 to 5 epochs (11s to 55s)
  w_hr   <- i
  hr_sec <- w_hr * 11
  
  # Cortical windows: 3 to 7 epochs (33s to 77s)
  w_stage   <- i + 2
  stage_sec <- w_stage * 11
  
  # Isolate Cortical Leads Grouped by UUID
  temp_sens <- copy(dt_sens)
  for (n in 1:w_stage) {
    temp_sens[, paste0("stage_lead_", n) := data.table::shift(stage_shift, n = n, type = "lead"), by = uuid]
  }
  lead_stage_cols <- paste0("stage_lead_", 1:w_stage)
  temp_sens[, window_shift_stage := do.call(pmax, c(.SD, na.rm = TRUE)), .SDcols = lead_stage_cols]
  
  # Isolate Cardiovascular Leads Grouped by UUID
  for (n in 1:w_hr) {
    temp_sens[, paste0("hr_lead_", n) := data.table::shift(hr_spike, n = n, type = "lead"), by = uuid]
  }
  lead_hr_cols <- paste0("hr_lead_", 1:w_hr)
  temp_sens[, window_shift_hr := do.call(pmax, c(.SD, na.rm = TRUE)), .SDcols = lead_hr_cols]
  
  # --- PART A: CORTICAL SENSITIVITY REGRESSION MODELLING ---
  temp_stage_dt <- temp_sens[, .(outcome = window_shift_stage, noise = flag_R25)]
  temp_stage_dt <- temp_stage_dt[!is.na(outcome) & !is.na(noise)]
  
  or_stage <- NA; ci_stage <- c(NA, NA)
  if (uniqueN(temp_stage_dt$outcome) == 2) {
    mod_stage <- tryCatch({
      suppressWarnings(glm(outcome ~ noise, data = temp_stage_dt, family = binomial))
    }, error = function(e) NULL)
    
    if (!is.null(mod_stage) && mod_stage$converged) {
      or_stage <- exp(coef(mod_stage))[2]
      ci_stage <- tryCatch(exp(confint.default(mod_stage))[2, ], error = function(e) c(NA, NA))
    }
  }
  
  # --- PART B: CARDIOVASCULAR SENSITIVITY REGRESSION MODELLING ---
  temp_hr_dt <- temp_sens[, .(outcome = window_shift_hr, noise = flag_R25)]
  temp_hr_dt <- temp_hr_dt[!is.na(outcome) & !is.na(noise)]
  
  or_hr <- NA; ci_hr <- c(NA, NA)
  if (uniqueN(temp_hr_dt$outcome) == 2) {
    mod_hr <- tryCatch({
      suppressWarnings(glm(outcome ~ noise, data = temp_hr_dt, family = binomial))
    }, error = function(e) NULL)
    
    if (!is.null(mod_hr) && mod_hr$converged) {
      or_hr <- exp(coef(mod_hr))[2]
      ci_hr <- tryCatch(exp(confint.default(mod_hr))[2, ], error = function(e) c(NA, NA))
    }
  }
  
  # --- PART C: CACHE INDEPENDENT ROWS ---
  results_list[[i]] <- data.table(
    Step             = i,
    HR_Window_Sec    = hr_sec,
    HR_Spike_OR      = round(fifelse(is.na(or_hr), as.numeric(NA), or_hr), 3),
    HR_Spike_Lower   = round(ci_hr[1], 3),
    HR_Spike_Upper   = round(ci_hr[2], 3),
    Stage_Window_Sec = stage_sec,
    Stage_OR         = round(fifelse(is.na(or_stage), as.numeric(NA), or_stage), 3),
    Stage_CI_Lower   = round(ci_stage[1], 3),
    Stage_CI_Upper   = round(ci_stage[2], 3)
  )
}

sensitivity_matrix <- rbindlist(results_list)
cat("\n============================================================================\n")
cat("=== PROTOCOL-ALIGNED TEMPORAL SENSITIVITY METRICS ===\n")
cat("============================================================================\n")
print(sensitivity_matrix, row.names = FALSE)
cat("----------------------------------------------------------------------------\n")

# Save sensitivity analysis table dynamically back to the processed directory sandbox
fwrite(sensitivity_matrix, here("data", "processed", "step_19_TEMPORAL_WINDOW_SENSITIVITY_RESULTS.csv"))
cat("Sensitivity metrics exported successfully to data/processed.\n")

cat("\nStep 19 Complete.\n")