### --- STEP 16: CLINICAL SLEEP QUALITY VS. PHENOTYPE ---

library(data.table)
library(here)

cat("\n--- STEP 16: CLINICAL SLEEP QUALITY VS. PHENOTYPE ---\n")

# 1. LOAD THE PHENOTYPES (From Step 15)
dt_pheno <- fread(here("data", "processed", "step_15_FINAL_PHENOTYPES_STANDARDIZED.csv"))

# 2. LOAD THE MAPPED DATA (From Step 11)
dt_sleep <- fread(here("data", "processed", "step_11_FINAL_MAPPED_DATA.csv"))

# 3. CALCULATE CLINICAL STATS PER PARTICIPANT WITH ACCURATE FRACTIONAL BASES
cat("Aggregating clinical metrics for 16 participants...\n")
global_stats <- dt_sleep[!is.na(participant_id), .(
  Total_Hours          = (.N * 11) / 3600,
  
  # CLINICAL AMENDMENT: Calculate Deep sleep individually alongside your combined macrostructure pool
  Deep_Pct             = (sum(sleepStage == 2, na.rm = TRUE) / .N) * 100,
  Deep_And_REM_Pct     = (sum(sleepStage %in% c(2, 3), na.rm = TRUE) / .N) * 100,
  Shifts_Per_Hr        = sum(stage_shift == TRUE, na.rm = TRUE) / ((.N * 11) / 3600)
), by = .(participant_id = as.character(participant_id))]

# 4. ALIGN AND MERGE
dt_pheno[, participant_id := as.character(participant_id)]
global_stats[, participant_id := as.character(participant_id)]
dt_comparison <- merge(global_stats, dt_pheno, by = "participant_id")

# 5. GENERATE FINAL THESIS SUMMARY TABLE WITH REVISED NOMENCLATURE
table_clinical_impact <- dt_comparison[, .(
  N                           = .N,
  Mean_Deep_Sleep_Pct         = round(mean(Deep_Pct, na.rm = TRUE), 1),
  Mean_Deep_And_REM_Sleep_Pct = round(mean(Deep_And_REM_Pct, na.rm = TRUE), 1),
  Avg_Shifts_Per_Hr           = round(mean(Shifts_Per_Hr, na.rm = TRUE), 2),
  Group_Cardiac_RR            = round(mean(HR_RR_R25, na.rm = TRUE), 2),
  Group_Cortical_RR           = round(mean(Brain_RR_R25, na.rm = TRUE), 2),
  Avg_Baseline_Decoupling     = round(mean(Baseline_Decoupling_Ratio, na.rm = TRUE), 2)
), by = Phenotype][order(Phenotype)]

# 6. SAVE & PRINT VIA NATIVE RELATIVE ENDPOINTS
cat("\n--- CLINICAL IMPACT BY SENSITIVITY PHENOTYPE ---\n")
print(table_clinical_impact)

fwrite(table_clinical_impact, here("data", "processed", "step_16_CLINICAL_IMPACT_SUMMARY.csv"))
fwrite(dt_comparison,        here("data", "processed", "step_16_PARTICIPANT_FULL_PROFILES.csv"))

cat("\nSummaries exported successfully to data/processed/\n")
cat("\nStep 16 Complete.\n")