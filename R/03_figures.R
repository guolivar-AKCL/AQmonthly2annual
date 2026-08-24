# ============================================================================
# 03 - Figures for the technical report
#
# Reads the products of 01 and 02 and writes the report figures. No new
# analysis happens here.
#
# Outputs: output/plots/*.png
# ============================================================================

source("R/00_config.R")
source("R/functions.R")

library(dplyr)
library(ggplot2)

factors <- read.csv(SCALING_FACTORS)
pairs <- read.csv("output/window_year_pairs.csv")

dir.create(PLOT_DIR, showWarnings = FALSE, recursive = TRUE)

# Presentation helpers -------------------------------------------------------

BLUE <- "#2a78d6" # categorical slot 1, also the sequential hue
SEQ_LOW <- "#cde2fb" # sequential step 100
SEQ_HIGH <- "#0d366b" # sequential step 700
GREY <- "#52514e" # secondary ink

theme_report <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey92"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(colour = GREY),
    strip.text = element_text(face = "bold")
  )

label_metric <- function(x) unname(METRIC_LABELS[x])

# Pooled factors, with the window's first month for ordering along the x axis
pooled <- factors |>
  filter(site == "pooled") |>
  mutate(
    metric_label = factor(label_metric(metric), levels = unname(METRIC_LABELS)),
    start_month = match(substr(window, 1, 3), month.abb)
  )

# ============================================================================
# Figure 1: which months to sample
#
# One cell per sampling window: where it starts, how long it runs, and how far
# out the scaled estimate lands. Dark is bad.
# ============================================================================

fig_heatmap <- ggplot(pooled, aes(x = start_month, y = n_months, fill = loo_pct_mae)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(
    aes(label = sprintf("%.0f", loo_pct_mae), colour = loo_pct_mae > 7),
    size = 2.7, show.legend = FALSE
  ) +
  scale_colour_manual(values = c("TRUE" = "white", "FALSE" = "#0b0b0b")) +
  scale_fill_gradient(low = SEQ_LOW, high = SEQ_HIGH, name = "Mean error (%)") +
  scale_x_continuous(breaks = 1:12, labels = month.abb, expand = c(0, 0)) +
  scale_y_continuous(breaks = 1:MAX_WINDOW, expand = c(0, 0)) +
  facet_wrap(~metric_label, ncol = 1) +
  labs(
    title = "Error in the scaled annual mean, by sampling window",
    subtitle = paste(
      "Leave-one-out mean absolute error, pooled across sites,",
      START_YEAR, "-", END_YEAR
    ),
    x = "First month sampled",
    y = "Months sampled"
  ) +
  theme_report +
  theme(panel.grid.major = element_blank())

ggsave(
  file.path(PLOT_DIR, "fig1_window_error_heatmap.png"), fig_heatmap,
  width = 8, height = 8, dpi = 300
)

# ============================================================================
# Figure 2: how much a longer campaign buys you
# ============================================================================

by_length <- factors |>
  filter(site != "pooled") |>
  mutate(metric_label = factor(label_metric(metric), levels = unname(METRIC_LABELS)))

fig_length <- ggplot(by_length, aes(x = factor(n_months), y = loo_pct_mae)) +
  geom_boxplot(outlier.shape = NA, colour = GREY, fill = "grey95", width = 0.6) +
  geom_jitter(width = 0.15, alpha = 0.25, size = 0.9, colour = BLUE) +
  geom_line(
    data = pooled |> group_by(metric_label, n_months) |>
      summarise(loo_pct_mae = min(loo_pct_mae), .groups = "drop"),
    aes(x = factor(n_months), y = loo_pct_mae, group = 1),
    colour = "#eb6834", linewidth = 0.8
  ) +
  facet_wrap(~metric_label) +
  labs(
    title = "Sampling error falls with campaign length, with diminishing returns",
    subtitle = paste(
      "Each point is one site and window; box summarises all windows of that",
      "length.\nOrange line: best available window of each length, pooled"
    ),
    x = "Months sampled",
    y = "Leave-one-out mean absolute error (%)"
  ) +
  theme_report

ggsave(
  file.path(PLOT_DIR, "fig2_error_by_window_length.png"), fig_length,
  width = 9, height = 4.5, dpi = 300
)

# ============================================================================
# Figure 3: the fit behind the recommended four-month factor
# ============================================================================

# Pick the best four-month window for each pollutant
best_four <- pooled |>
  filter(n_months == 4) |>
  group_by(metric) |>
  slice_min(loo_pct_mae, n = 1) |>
  ungroup()

scatter_data <- pairs |>
  inner_join(
    best_four |> select(metric, window, factor),
    by = c("Parameter" = "metric", "window")
  ) |>
  filter(site != CITY_SITE) |>
  mutate(
    metric_label = factor(label_metric(Parameter), levels = unname(METRIC_LABELS)),
    panel = paste0(label_metric(Parameter), " (", window, ")")
  ) |>
  arrange(metric_label) |>
  mutate(panel = factor(panel, levels = unique(panel)))

fig_scatter <- ggplot(scatter_data, aes(x = sampled_mean, y = annual_mean)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey75", linetype = "dashed") +
  geom_abline(
    data = distinct(scatter_data, panel, factor),
    aes(slope = factor, intercept = 0), colour = BLUE, linewidth = 0.8
  ) +
  geom_point(colour = BLUE, alpha = 0.7, size = 1.8) +
  facet_wrap(~panel, scales = "free") +
  labs(
    title = "Sampled mean against annual mean, best four-month window",
    subtitle = paste(
      "One point per site-year. Solid: fitted scaling factor.",
      "Dashed: no correction (1:1)"
    ),
    x = "Mean of the sampled months (µg/m³)",
    y = "Annual mean (µg/m³)"
  ) +
  theme_report

ggsave(
  file.path(PLOT_DIR, "fig3_sampled_vs_annual.png"), fig_scatter,
  width = 9, height = 4, dpi = 300
)

# ============================================================================
# Figure 4: the factors themselves, with confidence intervals
# ============================================================================

four_month <- pooled |>
  filter(n_months == 4) |>
  mutate(window = reorder(window, start_month))

fig_factors <- ggplot(four_month, aes(x = factor, y = window)) +
  geom_vline(xintercept = 1, colour = "grey75", linetype = "dashed") +
  geom_errorbar(
    aes(xmin = factor_lwr, xmax = factor_upr),
    orientation = "y", width = 0, colour = GREY
  ) +
  geom_point(colour = BLUE, size = 2.4) +
  facet_wrap(~metric_label) +
  labs(
    title = "Four-month scaling factors and their 95% confidence intervals",
    subtitle = paste(
      "Annual mean = factor x sampled mean. Pooled across sites,",
      START_YEAR, "-", END_YEAR
    ),
    x = "Scaling factor",
    y = NULL
  ) +
  theme_report

ggsave(
  file.path(PLOT_DIR, "fig4_four_month_factors.png"), fig_factors,
  width = 9, height = 4.5, dpi = 300
)

cat("03_figures: 4 figures written to ", PLOT_DIR, "/\n", sep = "")
