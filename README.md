# AQmonthly2annual

**Scaling factors that convert a short air quality monitoring campaign into an
estimate of the annual mean.**

Campaign monitoring, passive sampling and sensor deployments run for weeks or
months; guidelines and trend statements are annual means. Auckland's pollutants
have a strong seasonal cycle, so a winter campaign reads high and a summer one
reads low. This repository fits, for every pollutant and every window of one to
six consecutive months, the factor `annual mean = f x sampled mean`, and
measures how much error is left after the correction.

The deliverable is [docs/scaling_factors_report.Rmd](docs/scaling_factors_report.Rmd)
(knits to `docs/scaling_factors_report.html`) and the factor table
`output/scaling_factors.csv`.

Supports *Ambient Air Quality in Auckland - trend analysis 2015 to 2024*
(TR 2025/26). Contact: gustavo.olivarespino@aucklandcouncil.govt.nz

## Headline results

Best window of each length, pooled across sites, 2015-2024. "Error" is the
leave-one-out mean absolute error of the corrected estimate.

| Pollutant | 1 month | 4 months | 6 months |
|-----------|---------|----------|----------|
| PM2.5 | Oct, x1.040, 8.7% | Mar-Jun, x1.013, 5.1% | Jul-Dec, x0.949, 4.0% |
| PM10  | Aug, x0.973, 8.0% | Aug-Nov, x1.001, 4.4% | Jul-Dec, x0.976, 3.3% |
| NO2   | Mar, x1.066, 13.9% | Jul-Oct, x0.883, 7.8% | Jun-Nov, x0.897, 6.0% |

Which months you sample matters at least as much as how many. Uncorrected, a
May-August NO2 campaign overstates the annual mean by about 26%.

**Campaigns that cross the year end** (summer campaigns) are fitted against a
rolling twelve-month mean instead of a calendar year — the twelve months
beginning with the campaign. They work, but cost accuracy: Dec-Mar NO2 is
x1.184 with 17% error, against 8% for a well-timed four months. Extending a
summer campaign through to May brings NO2 back to 8.6%.

## Running it

Scripts run in filename order from the project root:

```sh
Rscript R/01_prepare_data.R      # monthly and annual means, completeness rules
Rscript R/02_scaling_factors.R   # fit the factors, score them out of sample
Rscript R/03_figures.R           # report figures
Rscript -e 'rmarkdown::render("docs/scaling_factors_report.Rmd")'
```

| File | What it does |
|------|--------------|
| [R/00_config.R](R/00_config.R) | Pollutants, period, longest window, completeness thresholds, paths. Change settings here. |
| [R/functions.R](R/functions.R) | Shared functions: window enumeration, factor fitting, leave-one-out and leave-one-site-out error, and `get_timeseries_data()` for pulling fresh data from KiWIS. |
| [R/01_prepare_data.R](R/01_prepare_data.R) | Resolves duplicate hours, adds the network-average series, applies completeness rules, writes monthly, annual and rolling twelve-month means. |
| [R/02_scaling_factors.R](R/02_scaling_factors.R) | Fits site-specific and pooled factors for every window, against both the calendar-year and rolling twelve-month targets, and scores them. |
| [R/03_figures.R](R/03_figures.R) | The five report figures. No analysis. |

Packages: **dplyr**, **lubridate**, **purrr**, **ggplot2**, **knitr**,
**rmarkdown**; **rjson** and **curl** for the KiWIS fetch.

## Method in brief

For each pollutant, site and window of consecutive months: average the monthly
means inside the window, fit the through-origin regression
`target ~ sampled + 0` across years, and report the coefficient as the scaling
factor. Two targets are fitted — the **calendar year** the window sits in
(the primary one, for windows inside a single year) and a **rolling twelve-month
mean** beginning with the window (which is what lets a campaign cross the end of
a year). Where both apply they agree to a median of 0.01 in the factor. A single through-origin factor is used rather than a two-parameter
regression because it is what a campaign can actually apply — one multiplier,
no intercept assumed to hold at a different concentration level.

Errors are scored out of sample: **leave-one-out** across years for the site's
own factor, and **leave-one-site-out** for the pooled factors, which is the
error to quote when a factor is applied at a location that contributed nothing
to fitting it.

Completeness rules: a month needs 75% of its hours, a year needs all twelve
months, a factor needs at least five usable years.

## Data

`data/aq_data_SOE2025.RData` holds `all.data.long` — ~12.1M hourly records
(`Parameter`, `Value`, `date`, `site`), 2003-2024, ten sites from rural
background (patumahoe) to city-centre kerbside (queen_st, customs_st), 18
parameters. `data/all_stations_parameters.RData` holds the KiWIS time series
catalogue used by `get_timeseries_data()`.

Both are read-only and neither is committed - you need access to the official
report data to run this.

## Outputs

| File | Contents |
|------|----------|
| `output/monthly_means.csv` | Monthly means with hour counts and completeness flags |
| `output/annual_means.csv` | Annual means for complete calendar site-years |
| `output/rolling_means.csv` | Rolling twelve-month means at every start month |
| `output/window_year_pairs.csv` | Sampled and annual mean for every site, year and window |
| `output/scaling_factors.csv` | **The factors**, with confidence intervals, fitted and out-of-sample errors, and the two-parameter alternative |
| `output/plots/` | The five report figures |
| `docs/scaling_factors_report.html` | The technical report |

Key columns of `scaling_factors.csv`: `metric`, `target` (`calendar` or
`rolling`), `site` (a site name, `auckland` for the network average, or
`pooled`), `window`, `n_months`, `wraps` (window crosses the year end), `factor`
with
`factor_lwr`/`factor_upr`, `loo_pct_mae` (own-site error), `loso_pct_mae`
(transfer error, pooled rows only), and `lm_intercept`/`lm_slope` for the
two-parameter alternative.

## Repository layout

Based on [Matt Dray's analysis template](https://github.com/matt-dray/analysis-template).
Each folder has its own `README.md`.

```
AQmonthly2annual/
|- data/      # raw inputs, read-only, not committed
|- ext/       # external inputs, read-only
|- R/         # the analysis pipeline
|- python/    # unused
|- docs/      # the technical report
|- output/    # generated data, factors and plots
|- Docker/    # one subfolder per image
|- AGENTS.md  # instructions for AI coding agents
`- README.md
```

See [AGENTS.md](AGENTS.md) for conventions, code style and the checks to run
before finishing a change.
