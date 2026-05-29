### --- STEP 11: SLEEP STAGE VALIDITY ---

cat("\n--- STEP 11: SLEEP STAGE VALIDITY ---\n")
library(data.table)
library(here)

# 1. LOAD DATA FROM STEP 10
cat("Loading mapped data from Step 10...\n")
dt <- fread(here("data", "processed", "step_10_MAPPED_DATA.csv"))

# 2. MAP SLEEP HIERARCHY
dt[, stage_rank := fcase(
  sleepStage == 2,  1, # Deep (N3)
  sleepStage == 3,  2, # REM
  sleepStage == 1,  3, # Light (N1/N2)
  sleepStage == -1, 4  # Awake
)]

# 3. DEFINE THE OUTCOME: STAGE SHIFT (55-Second Window Grouped by UUID)
cat("Calculating sleep stage transitions (55s window)...\n")
dt[, future_max_rank := pmax(
  data.table::shift(stage_rank, n = 1, type = "lead"),
  data.table::shift(stage_rank, n = 2, type = "lead"),
  data.table::shift(stage_rank, n = 3, type = "lead"),
  data.table::shift(stage_rank, n = 4, type = "lead"),
  data.table::shift(stage_rank, n = 5, type = "lead"),
  na.rm = TRUE
), by = uuid]

# CRITICAL FIX: Grouped evaluation isolates session boundaries to prevent cross-participant data bleeding
dt[, stage_shift := future_max_rank > stage_rank, by = uuid]

# Clean up temporary calculation tracking vector column cleanly
dt[, future_max_rank := NULL]

# 4. RUN VALIDITY MODELS
all_flags <- grep("^flag_", names(dt), value = TRUE)
results_list <- list()
cat("Running Logistic Regression with CIs for Sleep Stage Transitions...\n")

for (f in all_flags) {
  # Cleanly subset and isolate non-wake segments using get()
  temp_dt <- dt[!is.na(stage_shift) & !is.na(get(f)) & stage_rank < 4]
  
  # Ensure sufficient variations and observations exist to protect against model collapse
  if (sum(temp_dt[, get(f)], na.rm = TRUE) > 5 && uniqueN(temp_dt$stage_shift) == 2) {
    
    model <- tryCatch({
      # Suppress optimization warnings if rare thresholds yield sparse matrix cells
      suppressWarnings(glm(as.formula(paste("stage_shift ~", f)), data = temp_dt, family = binomial))
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
        
        # Corrected, robust hit rate calculation native to data.table syntax
        true_positives <- nrow(temp_dt[get(f) == TRUE & stage_shift == TRUE])
        total_flagged  <- nrow(temp_dt[get(f) == TRUE])
        shift_rate     <- (true_positives / total_flagged) * 100
        
        results_list[[f]] <- data.table(
          Flag           = f,
          Events         = as.integer(total_flagged),
          Hit_Rate_Pct   = round(shift_rate, 1),
          Odds_Ratio     = round(or_val, 2),
          OR_Lower       = round(ci_vals[1], 2),
          OR_Upper       = round(ci_vals[2], 2),
          P_Value        = as.character(p_char)
        )
      }
    }
  }
}

# 5. DISPLAY AND SAVE
if (length(results_list) > 0) {
  final_stage_showdown <- rbindlist(results_list)[order(-Odds_Ratio)]
  print(final_stage_showdown)
  
  # Save Results Relatively via pure here() paths
  fwrite(final_stage_showdown, here("data", "processed", "step_11_STAGE_VALIDITY_RESULTS.csv"))
} else {
  cat("Warning: No logistic regression models converged successfully.\n")
}

fwrite(dt, here("data", "processed", "step_11_FINAL_MAPPED_DATA.csv"))
cat("\nStep 11 Complete.\n")