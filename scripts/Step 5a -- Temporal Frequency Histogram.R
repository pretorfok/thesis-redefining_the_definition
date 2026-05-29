### --- STEP 5a TEMPORAL FREQUENCY HISTOGRAM ---

library(data.table)
library(ggplot2)
library(here)
library(patchwork) 
library(scales) 

cat("\n--- STEP 5a TEMPORAL FREQUENCY HISTOGRAM ---\n")

# 1. Load Data
dt <- fread(here("data", "processed", "step_5_MAPPED_GOLDEN_DATASET.csv"))

# 2. Calculate Time Gaps (Diffs) per Participant
setorder(dt, uuid, timestamp)
dt[, time_gap_ms := timestamp - shift(timestamp), by = uuid]
dt[, time_gap_s := time_gap_ms / 1000]
plot_data <- dt[!is.na(time_gap_s) & time_gap_s < 60]

# --- AUTOMATED VALIDATION CALCULATION ---
total_obs <- nrow(plot_data)
peak_obs <- nrow(plot_data[time_gap_s >= 10.75 & time_gap_s <= 11.25])
sec_freq_pct <- ((total_obs - peak_obs) / total_obs) * 100

# 3. Create the Main Plot
p_main <- ggplot(plot_data, aes(x = time_gap_s)) + 
  geom_histogram(binwidth = 0.5, fill = "#008080", color = "white") + 
  scale_x_continuous(breaks = seq(0, 60, by = 5)) +
  # Force coordinates so the plot doesn't "pad" the axis
  coord_cartesian(xlim = c(0, 60), ylim = c(0, max(layer_data(last_plot())$y) * 1.05), expand = FALSE) +
  labs(
    title = "Distribution of Temporal Frequency between Observations",
    subtitle = paste0("Analysis of sampling consistency (Gap < 60s) | Secondary Freq: ", round(sec_freq_pct, 2), "%"),
    x = "Time Gap between successive records (Seconds)",
    y = "Frequency (Count)"
  ) +
  theme_minimal() +
  theme(
    axis.line.x = element_line(color = "black", linewidth = 0.8),
    axis.line.y = element_line(color = "black", linewidth = 0.8),
    panel.grid.minor = element_blank()
  )

# 4. Create the Inset Plot
p_inset <- ggplot(plot_data, aes(x = time_gap_s)) + 
  geom_histogram(binwidth = 0.5, fill = "#CC7766", color = "white") +
  coord_cartesian(xlim = c(0, 60), expand = FALSE) +
  scale_x_continuous(breaks = seq(0, 60, by = 15)) +
  scale_y_log10(
    breaks = 10^c(0, 2, 4, 6), 
    limits = c(1, 10^6.2), 
    labels = label_scientific(digits = 0),
    expand = expansion(mult = c(0, 0.05))
  ) + 
  labs(title = "Log-Scale (Count)", x = NULL, y = NULL) +
  theme_minimal(base_size = 9) +
  theme(
    plot.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    axis.line.y = element_line(color = "black", linewidth = 0.5)
  )

# 5. Combine with Patchwork
final_plot <- p_main + inset_element(
  p_inset, 
  left = 0.52, bottom = 0.45, right = 0.98, top = 0.97
)

# 6. Save
ggsave(filename = "step_5a_histogram.png", plot = final_plot, path = here("outputs", "figures"), width = 10, height = 6, dpi = 300)