################################################################################
# Plots for the report:
# Ambient Air Quality in Auckland – trend analysis 2015 to 2024
# TR 2025/26
################################################################################
# To use this script, you must have access to the official air quality data
# for this report in RData format
#
# For questions and support gustavo.olivarespino@aucklandcouncil.govt.nz
################################################################################

# Load Libraries
library(dplyr)
library(openair)
library(purrr)
library(ggplot2)
library(lubridate)

# Load data
load("./data/aq_data_SOE2025.RData")
# all.data.long

# Add "ALL AUCKLAND" site
auckland_avg <- all.data.long |>
  group_by(date, Parameter) |>
  summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") |>
  mutate(site = "auckland")

# Bind the new rows to the original dataframe
all.data.long <- bind_rows(all.data.long, auckland_avg)

# Which PM metric are we looking at?
pm <- "pm2.5"
pm_label <- "PM2.5"
# Let's see the relationship between the May-August average and the annual average for pm2.5 at all sites from 2015 onwards
pm_data <- all.data.long |>
  filter(Parameter == pm) |>
  mutate(
    year = year(date),
    month = month(date)
  ) |>
  group_by(site, year) |>
  summarise(
    annual_avg = mean(Value, na.rm = TRUE),
    may_aug_avg = mean(Value[month %in% 5:8], na.rm = TRUE),
    cc_dep_avg = mean(Value[month %in% c(10:12, 1:2)], na.rm = TRUE),
    sep_dec_avg = mean(Value[month %in% 9:12], na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(year >= 2015)
# Scatter plot of may-august avg vs annual avg
# Linear model to quantify the relationship
lm_model <- lm(annual_avg ~ cc_dep_avg, data = pm_data)
summary(lm_model)

lm_model <- lm(annual_avg ~ may_aug_avg, data = pm_data)
summary(lm_model)
# Plot with regression line, confidence interval, and equation in subtitle.
# One plot per site in a single figure
sites <- unique(pm_data$site)
n_sites <- length(sites)
par(mfrow = c(ceiling(n_sites / 3), 3), mar = c(4, 4, 3, 1))
site_name <- "auckland"
for (site_name in sites) {
  site_data <- pm_data %>% filter(site == site_name)
  lm_site <- lm(annual_avg ~ cc_dep_avg + 0, data = site_data)

  plot(
    site_data$cc_dep_avg,
    site_data$annual_avg,
    main = site_name,
    xlab = paste("May-August Average", pm_label, "(µg/m³)"),
    ylab = paste("Annual Average", pm_label, "(µg/m³)"),
    pch = 16,
    col = "steelblue"
  )

  # Add regression line with confidence interval
  x_range <- range(site_data$cc_dep_avg, na.rm = TRUE)
  x_new <- seq(x_range[1], x_range[2], length.out = 100)
  pred <- predict(lm_site, newdata = data.frame(cc_dep_avg = x_new), se.fit = TRUE)

  # Add confidence interval (shaded region)
  polygon(
    c(x_new, rev(x_new)),
    c(pred$fit + 1.96 * pred$se.fit, rev(pred$fit - 1.96 * pred$se.fit)),
    col = rgb(0, 0, 1, 0.1),
    border = NA
  )

  # Add regression line
  lines(x_new, pred$fit, col = "blue", lwd = 2)

  # Extract coefficients for equation
  intercept <- coef(lm_site)[1]
  slope <- coef(lm_site)[2]
  r_squared <- summary(lm_site)$r.squared

  # Add equation in subtitle
  equation <- sprintf(
    "[Annual] = %.3f + %.3f * [May-Aug], R² = %.3f",
    intercept, slope, r_squared
  )
  mtext(equation, side = 3, line = 0.5, cex = 0.85)
}

par(mfrow = c(1, 1))


ggplot(pm_data) +
  geom_point(aes(x = cc_dep_avg, y = annual_avg, colour = "Oct - Feb")) +
  # geom_point(aes(x = may_aug_avg, y = annual_avg, colour = "May - Aug")) +
  # geom_point(aes(x = sep_dec_avg, y = annual_avg, colour = "Sep - Dec")) +
  theme_minimal()
ggsave("./output/pm2.5_may_aug_vs_annual.png", width = 8, height = 6)


# Now let's look at all months individually and their relationship with the annual average
pm_monthly <- all.data.long |>
  filter(Parameter == pm) |>
  mutate(
    year = year(date),
    month = month(date)
  ) |>
  group_by(site, year, month) |>
  summarise(
    monthly_avg = mean(Value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(year >= 2015)

# Add annual average to this dataframe
pm_monthly <- pm_monthly |>
  left_join(
    pm_data |> select(site, year, annual_avg),
    by = c("site", "year")
  )
# Add season labels Jan-Apr, May-Aug, Sep-Dec
pm_monthly <- pm_monthly |>
  mutate(
    season = case_when(
      month %in% 1:4 ~ "Jan-Apr",
      month %in% 5:8 ~ "May-Aug",
      month %in% 9:12 ~ "Sep-Dec"
    )
  )

# Plot with colours for each season
ggplot(pm_monthly, aes(x = monthly_avg, y = annual_avg, colour = season)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    formula = y ~ x + 0, se = FALSE,
    color = "black"
  ) +
  labs(
    title = "Relationship between Monthly Average and Annual Average of PM2.5",
    x = paste("Monthly Average", pm_label, "(µg/m³)"),
    y = paste("Annual Average", pm_label, "(µg/m³)"),
    colour = "Season"
  ) +
  theme_minimal()

# Plot with colours for each month
ggplot(pm_monthly, aes(x = monthly_avg, y = annual_avg, colour = factor(month))) +
  geom_point() +
  geom_smooth(
    method = "lm",
    formula = y ~ x + 0, se = FALSE,
    color = "black"
  ) +
  labs(
    title = "Relationship between Monthly Average and Annual Average of PM2.5",
    x = paste("Monthly Average", pm_label, "(µg/m³)"),
    y = paste("Annual Average", pm_label, "(µg/m³)"),
    colour = "Month"
  ) +
  theme_minimal()


summary(lm(annual_avg ~ jan_apr_avg + 0, data = pm_data))
summary(lm(annual_avg ~ may_aug_avg + 0, data = pm_data))
summary(lm(annual_avg ~ sep_dec_avg + 0, data = pm_data))
