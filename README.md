# Mixed Effects Models for Retrofit Impact Evaluation

[![License: CC BY-SA 4.0](https://img.shields.io/badge/License-CC%20BY--SA%204.0-lightgrey.svg)](https://creativecommons.org/licenses/by-sa/4.0/)

## Overview

This repository provides a complete, transferable methodological framework for evaluating the impact of energy retrofit interventions on indoor climate parameters using **linear mixed effects (LME) models**. The code and sample data accompany the paper:

> **[AKM]**, "Assessing the Impact of Energy Retrofits on Indoor Climate Conditions Using Mixed Effects Models: Methodology and R Implementation," *[Atmosphere]*, [Year]. DOI: [insert DOI]

The primary contribution is the analytical framework and its open-source implementation — not the findings of the specific case study. Indoor vapour pressure (partial pressure of water vapour, Pw) measured in 23 Irish residential homes across pre- and post-retrofit periods is used as a worked example, but the code is designed to be readily adapted to other indoor climate outcomes, building datasets, and retrofit contexts.

---

## Repository Structure

**The repository houses the following files:**

A License file.

A gitignore file for R repositories.

A read me file (this one).

The analysis code: mixed_effects_model_analysis.R

And sample data provided in two forms: a csv file and an R data file.


---

## Requirements

The analysis was conducted in **R** (version ≥ 4.2.0). The following packages are required:

```r
install.packages(c(
  "lme4",        # Mixed effects model fitting
  "lmerTest",    # p-values for fixed effects
  "emmeans",     # Estimated marginal means and pairwise contrasts
  "sjPlot",      # Coefficient tables and standardised effects
  "performance", # VIF, ICC, model diagnostics
  "effectsize",  # Standardised coefficients with CIs
  "tidyverse",   # Data manipulation and visualisation
  "patchwork",   # Multi-panel figures
  "flextable",   # Publication-ready tables
  "officer",     # Word document export
  "caret",       # Stratified train/test split
  "scales"       # Percentage formatting in plots
))
```

---

## Quick Start

1. Create your R project (in R Studio or otherwise) for the analysis

2. Download the files in this repsoitory to you R project folder. 

3. To adapt the framework to your own dataset, replace `sample_data.csv` with your data structured according to the tidy format described in Section 2.1.2 of the paper (see also Table 1 therein), and update the model formula to reflect your outcome variable and available predictors.

---

## Data

The sample dataset (`data/sample_data.csv`) is a synthetic dataset derived from a monitoring study of 23 Irish residential homes, comprising approximately 28,000 hourly observations (~5% of the full dataset). It includes:

| Variable | Description |
|---|---|
| `HomeID` | Anonymised home identifier |
| `RetrofitStatus` | Pre Retrofit / Post Retrofit |
| `RoomPw` | Indoor partial pressure of water vapour (kPa) |
| `ExternalPw` | External partial pressure of water vapour (kPa) |
| `ExternalT` | External temperature (°C) |
| `RoomT` | Room temperature (°C) |
| `RoomType` | Living Room / Other Rooms |
| `Season` | Heating / Non-Heating |
| `Occupants` | Number of occupants |

External climate data sourced from Met Éireann (Dublin Airport station).

---

## Key Outputs

The framework produces:

- **Model selection table**: LRT comparisons and AIC/BIC for candidate models
- **Fixed effects table**: Raw and standardised coefficients with 95% CIs and p-values
- **Random effects summary**: ICC, variance components, intercept–slope correlations
- **Diagnostic plots**: Residual plots, Q-Q plots, ACF, VIF table
- **Pairwise contrasts**: Tukey-adjusted emmeans comparisons by room type and retrofit status
- **Robustness table**: Coefficient stability under 5th/95th percentile exclusion
- **Validation plot**: Predicted vs. observed on 30% holdout test set (R² = 0.817)

---

## Adapting to Other Outcomes

The framework is outcome-agnostic. To apply it to a different indoor climate parameter (e.g. CO₂ concentration, indoor temperature, PM2.5):

1. Replace `RoomPw` with your outcome variable throughout 
2. Review the choice of fixed effects — predictors relevant to humidity (e.g. `ExternalPw`) may not be appropriate for all outcomes
3. Check the MAR assumption for missing data and re-run diagnostics
`

---

## License

This work is licensed under a [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/).

You are free to share and adapt this material for any purpose, provided you give appropriate credit to the original authors and distribute any adaptations under the same license.





