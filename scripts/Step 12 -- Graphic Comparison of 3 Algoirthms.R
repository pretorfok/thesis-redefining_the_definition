### --- STEP 12: GRAPHIC COMPARISON OF 3 ALGORITHMS ---

library(data.table)
library(ggplot2)
library(here)

cat("\n--- STEP 12: GRAPHIC COMPARISON OF 3 ALGORITHMS ---\n")

# 1. LOAD AND PREP DATA
hr_results <- fread(here("data", "processed", "step_10_HR_VALIDITY_RESULTS.csv"))
ss_results <- fread(here("data", "processed", "step_11_STAGE_VALIDITY_RESULTS.csv"))

# Ensure column names match cleanly across data models
setnames(ss_results, "Shift_Rate_Pct", "Hit_Rate_Pct", skip_absent = TRUE)
hr_results[, Outcome := "Heart Rate Spike (>5 BPM)"]
ss_results[, Outcome := "Downward Sleep Stage Shift"]
plot_dt <- rbind(hr_results, ss_results, fill = TRUE)

# 2. CLEAN DATA FOR DISPLAY & ENFORCE LOGICAL FACTORS
# Clean threshold keys and map algorithm category frameworks
plot_dt[, Threshold_Label := gsub("flag_", "", Flag)]

plot_dt[, Algorithm := fcase(
  grepl("flag_G", Flag), "Global Z-Score (G-Series)",
  grepl("flag_R", Flag), "Rolling Z-Score (R-Series)",
  grepl("flag_D", Flag), "Rise Time (D-Series)",
  grepl("flag_L", Flag), "L90+ Threshold (L-Series)"
)]

# CRITICAL FIX: Enforce a strict chronological/numerical factor order for the X-axis
target_order <- c("G25", "G30", "G40", "R25", "R30", "R40", "L10", "L15", "L20", "D05", "D10", "D15")
plot_dt[, Threshold_Label := factor(Threshold_Label, levels = intersect(target_order, unique(Threshold_Label)))]

# 3. CREATE THE 2x2 ACADEMIC IMPACT PLOT
final_plot <- ggplot(plot_dt, aes(x = Threshold_Label, y = Odds_Ratio, fill = Outcome)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", linewidth = 0.8) +
  facet_wrap(~Algorithm, scales = "free_x", ncol = 2) + 
  scale_fill_manual(values = c("#D95F02", "#7570B3")) +
  theme_minimal() +
  labs(
    title = "Statistical Synchronisation Across Algorithms: Cardiovascular vs. Cortical Events",
    subtitle = "Comparison of Odds Ratios Relative to Baseline Threshold Boundaries",
    y = "Odds Ratio (Likelihood of Response)",
    x = "Detection Paradigm and Threshold",
    fill = "Event Type:"
  ) +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 10, face = "bold"), # Slanted to guarantee zero text overlap
    strip.text      = element_text(face = "bold", size = 11),
    panel.spacing   = unit(1.5, "lines"),
    legend.position = "bottom",
    plot.title      = element_text(face = "bold", size = 14)
  )

# 4. SAVE TO FIGURES SUBDIRECTORY VIA ENHANCED GRAPHIC DEVICES
# Ensuring the destination folder asset structure is fully initialized
figures_dir <- here("outputs", "figures")
if (!dir.exists(figures_dir)) dir.create(figures_dir, recursive = TRUE)

output_file_path <- file.path(figures_dir, "step_12_final_thesis_impact_matrix.png")

# Using explicit width, height, and high resolution (dpi) parameters for print readiness
ggsave(
  filename = output_file_path,
  plot     = final_plot,
  width    = 10,
  height   = 8,
  dpi      = 300
)

# Safe fallback print execution check for interactive console viewports
if (interactive()) {
  print(final_plot)
}

cat("Matrix charts exported safely to:\n =>", output_file_path, "\n")
cat("Step 12 Complete.\n")