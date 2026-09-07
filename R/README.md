# `R/`

The analysis pipeline scripts, numbered to indicate run order:

- `00_config.R` — Analysis settings (pollutants, period, thresholds)
- `functions.R` — Shared functions used by other scripts
- `01_prepare_data.R` — Data preparation and completeness rules
- `02_scaling_factors.R` — Fit the scaling factors
- `03_figures.R` — Generate report figures
- `04_method_checks.R` — Sensitivity analysis and validation

Run them in filename order. Each script reads the output of the previous step from `output/` and writes its results there. Processed data and figures go to `output/`.
