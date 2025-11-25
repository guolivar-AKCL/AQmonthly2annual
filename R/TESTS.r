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

# Let's see the relationship between the May-August average and the annual average for no2 at all sites from 2015 onwards
no2_data <- all.data.long |>
  filter(Parameter == "no2") |>
  mutate(
    year = year(date),
    month = month(date)
  ) |>
  group_by(site, year) |>
  summarise(
    annual_avg = mean(Value, na.rm = TRUE),
    may_aug_avg = mean(Value[month %in% 5:8], na.rm = TRUE),
    jan_apr_avg = mean(Value[month %in% 1:4], na.rm = TRUE),
    sep_dec_avg = mean(Value[month %in% 9:12], na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(year >= 2015)
# Scatter plot of may-august avg vs annual avg
# Linear model to quantify the relationship
lm_model <- lm(annual_avg ~ may_aug_avg + 0, data = no2_data)
summary(lm_model)
ggplot(no2_data, aes(x = may_aug_avg, y = annual_avg, colour = site)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    formula = y ~ x + 0, se = TRUE,
    color = "blue"
  ) +
  labs(
    title = "Relationship between May-August Average and Annual Average of NO2",
    x = "May-August Average NO2 (µg/m³)",
    y = "Annual Average NO2 (µg/m³)",
    subtitle = sprintf("[Annual] = %.3f * [May-August]", coef(lm_model)[1])
  ) +
  theme_minimal()

ggplot(no2_data) +
  geom_point(aes(x = jan_apr_avg, y = annual_avg, colour = "Jan - Apr")) +
  geom_point(aes(x = may_aug_avg, y = annual_avg, colour = "May - Aug")) +
  geom_point(aes(x = sep_dec_avg, y = annual_avg, colour = "Sep - Dec")) +
  theme_minimal()
ggsave("./output/no2_may_aug_vs_annual.png", width = 8, height = 6)


# Now let's look at all months individually and their relationship with the annual average
no2_monthly <- all.data.long |>
  filter(Parameter == "no2") |>
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
no2_monthly <- no2_monthly |>
  left_join(
    no2_data |> select(site, year, annual_avg),
    by = c("site", "year")
  )
# Add season labels Jan-Apr, May-Aug, Sep-Dec
no2_monthly <- no2_monthly |>
  mutate(
    season = case_when(
      month %in% 1:4 ~ "Jan-Apr",
      month %in% 5:8 ~ "May-Aug",
      month %in% 9:12 ~ "Sep-Dec"
    )
  )

# Plot with colours for each season
ggplot(no2_monthly, aes(x = monthly_avg, y = annual_avg, colour = season)) +
  geom_point() +
  geom_smooth(
    method = "lm",
    formula = y ~ x + 0, se = FALSE,
    color = "black"
  ) +
  labs(
    title = "Relationship between Monthly Average and Annual Average of NO2",
    x = "Monthly Average NO2 (µg/m³)",
    y = "Annual Average NO2 (µg/m³)",
    colour = "Season"
  ) +
  theme_minimal()

# Plot with colours for each month
ggplot(no2_monthly, aes(x = monthly_avg, y = annual_avg, colour = factor(month))) +
  geom_point() +
  geom_smooth(
    method = "lm",
    formula = y ~ x + 0, se = FALSE,
    color = "black"
  ) +
  labs(
    title = "Relationship between Monthly Average and Annual Average of NO2",
    x = "Monthly Average NO2 (µg/m³)",
    y = "Annual Average NO2 (µg/m³)",
    colour = "Month"
  ) +
  theme_minimal()


summary(lm(annual_avg ~ jan_apr_avg + 0, data = no2_data))
summary(lm(annual_avg ~ may_aug_avg + 0, data = no2_data))
summary(lm(annual_avg ~ sep_dec_avg + 0, data = no2_data))
