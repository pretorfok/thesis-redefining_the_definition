### --- STEP 15: CLUSTERING (HEART VS. BRAIN SENSITIVITY) ---

library(data.table)
library(ggplot2)
library(ggrepel)
library(here)

cat("\n--- STEP 15: CLUSTERING BY STANDARDIZED CARDIOVASCULAR vs. CORTICAL EVENTS ---\n")

# 1. LOAD & PREP (Using the Hourly-Rate Risk Ratios from Step 14)
dt_sens <- fread(here("data", "processed", "step_14_INDIVIDUAL_PROFILES_HOURLY.csv"))

# 2. K-MEANS CLUSTERING (K=3) WITH TIGHT SEED ENCAPSULATION
# Enforcing seed immediately before the call guarantees execution determinism inside run_all.R
set.seed(123) 

km_data <- scale(dt_sens[, .(HR_RR_R25, Brain_RR_R25)])
km_res  <- kmeans(km_data, centers = 3, nstart = 25)
dt_sens[, Cluster := as.integer(km_res$cluster)]

# 3. STABILIZE AND LABEL PHENOTYPES BASED ON COMPOSITE SENSITIVITY
dt_sens[, Composite := (HR_RR_R25 + Brain_RR_R25) / 2]

# Deterministically order clusters by their actual calculated composite means
cluster_order <- dt_sens[, .(m = mean(Composite)), by = Cluster][order(m)]
cluster_order[, Target_Rank := seq_len(.N)]

# Bridge the rank map back to the main data table to prevent flipped labels across runs
dt_sens <- merge(dt_sens, cluster_order[, .(Cluster, Target_Rank)], by = "Cluster", all.x = TRUE)

dt_sens[, Phenotype := fcase(
  Target_Rank == 1, "Type A: Stable Responders",
  Target_Rank == 2, "Type B: Decoupled (Heart Only)",
  Target_Rank == 3, "Type C: Fragile High-Responders"
)]

# Clean up temporary structural sorting rankings
dt_sens[, Target_Rank := NULL]

# 4. GENERATE THE FINAL THESIS FIGURE
thesis_plot <- ggplot(dt_sens, aes(x = HR_RR_R25, y = Brain_RR_R25, color = Phenotype)) +
  # Safe Zone: Below 1.5 RR (Baseline variability threshold)
  annotate("rect", xmin = 0, xmax = 1.5, ymin = 0, ymax = 1.5, fill = "grey", alpha = 0.2) +
  # Decoupling Zone Label
  annotate("text", x = 5.2, y = 0.3, label = "Cardiovascular-Cortical Decoupling", fontface = "italic", alpha = 0.5, size = 4) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", alpha = 0.3) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "red", alpha = 0.3) +
  geom_point(size = 5, alpha = 0.8) +
  geom_text_repel(aes(label = paste("P", participant_id)), fontface = "bold", size = 4.5) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("#2980B9", "#8E44AD", "#C0392B")) +
  scale_x_continuous(limits = c(0, 7), breaks = 0:7) +
  scale_y_continuous(limits = c(0, 4), breaks = 0:4) +
  labs(
    title = "Phenotypic Clustering of Cardiovascular and Cortical Sensitivity",
    subtitle = "Standardized Risk Ratios (RR) Relative to Individual Sessional Baselines",
    x = "Cardiovascular Sensitivity (Heart Rate RR)",
    y = "Cortical Sensitivity (Sleep Stage Shift RR)"
  ) +
  theme(
    legend.position = "bottom",
    legend.title    = element_blank(),
    plot.title      = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle   = element_text(hjust = 0.5)
  )

# 5. SAVE AND SUMMARY (Using safe device configurations)
figures_dir <- here("outputs", "figures")
if (!dir.exists(figures_dir)) dir.create(figures_dir, recursive = TRUE)

ggsave(
  filename = file.path(figures_dir, "step_15_Sensitivity_Map_Standardized.png"),
  plot     = thesis_plot,
  width    = 10,
  height   = 8,
  dpi      = 600
)

group_summary <- dt_sens[, .(
  N                   = .N,
  Avg_Cardiac_RR      = round(mean(HR_RR_R25), 2),
  Avg_Cortical_RR     = round(mean(Brain_RR_R25), 2),
  Baseline_Decoupling = round(mean(Baseline_Decoupling_Ratio), 2)
), by = Phenotype][order(Phenotype)]

print(group_summary)

# Save master clustered dataset to data/processed folder relatively
fwrite(dt_sens, here("data", "processed", "step_15_FINAL_PHENOTYPES_STANDARDIZED.csv"))

cat("\nStep 15 Complete.\n")