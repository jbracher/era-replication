# era-replication

Replication materials for the Matters Arising comment

> Bracher, J. and Funk, S. *Information leakage from data revisions in retrospective forecasting studies.*

commenting on

> Aygün, E., Belyaeva, A., Comanici, G. et al. *An AI system to help scientists write expert-level empirical software.* Nature **654**, 909–916 (2026). https://doi.org/10.1038/s41586-026-10658-6

The comment shows that the reported ~11% improvement in mean Weighted Interval Score (WIS) of ERA's *Google Retrospective* over the CDC COVID-19 Forecast Hub ensemble, in the retrospective evaluation of the 2024/25 season, is driven by information leakage: the retrospective pipeline was scored against consolidated end-of-season surveillance data that had been revised after the real-time forecast dates. The advantage is concentrated in the state-weeks whose data were most heavily revised and disappears where the data were stable.

## Contents

- `analysis.R` — main script. Reproduces the paper's WIS numbers, reconstructs the real-time and consolidated data from the weekly snapshots, quantifies the revisions, and generates the figures in `figures/`.
- `google_materials/selected_submissions-june.csv` — forecasts (predictive quantiles and precomputed WIS) for `CovidHub-ensemble` and `Google Retrospective (TS)`, horizons 0–3 weeks, 52 US states and territories, 2024/25 season. Provided by the authors of Aygün et al.
- `snapshots/` — 30 weekly real-time snapshots of the NHSN COVID-19 hospital-admissions data (Wednesdays, 2024-11-20 to 2025-06-18). Used to reconstruct the data available at each forecast date and its revision relative to the final (2025-06-18) values.
- `figures/` — generated figures: `extreme_cases.pdf` (the nine most strongly revised state-weeks, overlaying real-time and consolidated data with both models' forecasts) and `comparison_absolute_revisions.pdf` / `comparison_relative_revisions.pdf` (WIS advantage as a function of the accepted revision threshold).

## Reproducing the analysis

Requires R with base packages only. From the repository root:

```sh
Rscript analysis.R
```

This reproduces the headline WIS values (mean 29.1 for the CDC ensemble vs 25.9 for *Google Retrospective*) and writes the PDFs to `figures/`. The revision-binned summary reported in the comment is derived from the same `model_comparison` and `data_revisions` objects built in the script.

## Data sources and reuse

- Forecasts in `google_materials/` were shared by the authors of Aygün et al. (2026); please cite the original paper when reusing them.
- Surveillance snapshots in `snapshots/` derive from the CDC/NHSN Weekly Hospital Respiratory Data.
