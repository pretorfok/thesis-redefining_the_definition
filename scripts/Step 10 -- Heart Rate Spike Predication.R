### --- STEP 10: HR PREDICTIVE VALIDITY COMPARISON ---

cat("--- STEP 10: Running HR Predictive Validity Analysis (with Confidence Intervals)\n")
library(data.table)
library(here)

# 1. LOAD DATA
dt <- fread(here("data", "processed", "step_9_SENSITIVITY_MASTER.csv"))

# 2. DEFINE OUTCOME: Heart Rate Spike (Strictly Grouped by UUID)
cat("Defining Heart Rate Spike window (22 seconds)...\n")
dt[, hr_lead1 := data.table::shift(heartRate, n = 1, type = "lead"), by = uuid]
dt[, hr_lead2 := data.table::shift(heartRate, n = 2, type = "lead"), by = uuid]

# CRITICAL FIX: Grouped evaluation isolates session boundaries to prevent cross-participant data bleeding
dt[, hr_spike := (hr_lead1 - heartRate >= 5) | (hr_lead2 - heartRate >= 5), by = uuid]

# Clean up temporary lag vector columns cleanly
dt[, `:=`(hr_lead1 = NULL, hr_lead2 = NULL)]

# 3. EXCLUDE WAKE EPOCHS
dt_clean <- dt[!is.na(hr_spike) & !is.na(heartRate) & !(sleepStage %in% c(-1, 4))]

# 4. RUN LOGISTIC REGRESSION MODELS
all_flags <- grep("^flag_", names(dt_clean), value = TRUE)
results_list <- list()
cat("Calculating Odds Ratios and CIs for", length(all_flags), "detection thresholds...\n")

for (f in all_flags) {
  # Standardize dynamic column calls using get()
  temp_dt <- dt_clean[!is.na(get(f))]
  
  # Ensure there are sufficient flagged events and variations to prevent model collapse
  if (sum(temp_dt[, get(f)], na.rm = TRUE) > 5 && uniqueN(temp_dt$hr_spike) == 2) {
    
    model <- tryCatch({
      # Wrap in suppressWarnings to keep output clean if extreme thresholds result in poor convergence
      suppressWarnings(glm(as.formula(paste("hr_spike ~", f)), data = temp_dt, family = binomial))
    }, error = function(e) NULL)
    
    if (!is.null(model) && model$converged) {
      # Extract summary coefficients matrix to retrieve exact p-values
      coef_matrix <- summary(model)$coefficients
      
      # Defensive check: verify that the flag predictor was not dropped due to singularity
      if (nrow(coef_matrix) >= 2) {
        or_val  <- exp(coef_matrix[2, 1])
        p_raw   <- coef_matrix[2, 4]
        ci_vals <- tryCatch(exp(confint.default(model))[2, ], error = function(e) c(NA, NA))
        
        # Format the P-Value column cleanly as a character vector matching professional scientific layout standards
        p_char  <- if (p_raw < 0.001) "<.001" else sprintf("%.3f", p_raw)
        
        # Corrected, high-speed hit rate calculation native to data.table evaluation
        true_positives <- nrow(temp_dt[get(f) == TRUE & hr_spike == TRUE])
        total_flagged  <- nrow(temp_dt[get(f) == TRUE])
        hit_rate       <- (true_positives / total_flagged) * 100
        
        results_list[[f]] <- data.table(
          Flag         = f,
          Events       = as.integer(total_flagged),
          Hit_Rate_Pct = round(hit_rate, 1),
          Odds_Ratio   = round(or_val, 2),
          OR_Lower     = round(ci_vals[1], 2),
          OR_Upper     = round(ci_vals[2], 2),
          P_Value      = as.character(p_char)
        )
      }
    }
  }
}

# 5. FINAL TABLE OUTPUT
if (length(results_list) > 0) {
  final_hr_showdown <- rbindlist(results_list)[order(-Odds_Ratio)]
  cat("\n--- HEART RATE VALIDITY SHOWDOWN RESULTS (with CI) ---\n")
  print(final_hr_showdown)
  
  # 6. EXPORT
  fwrite(final_hr_showdown, here("data", "processed", "step_10_HR_VALIDITY_RESULTS.csv"))
} else {
  cat("\nWarning: No logistic regression models converged successfully.\n")
}

fwrite(dt_clean, here("data", "processed", "step_10_MAPPED_DATA.csv"))

cat("\nStep 10 Complete.\n")