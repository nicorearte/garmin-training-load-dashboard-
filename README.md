# Garmin Training Load Dashboard

An R-based dashboard that estimates training load, fatigue, and workload risk from Garmin Connect CSV exports for runners whose Garmin device does not include advanced load analysis.

## Features

- Imports Garmin Connect CSV activity data
- Filters running activities only
- Computes:
  - ATL (Acute Training Load)
  - CTL (Chronic Training Load)
  - TSB (Training Stress Balance)
  - ACWR (Acute:Chronic Workload Ratio)
- Produces:
  - a KPI / executive dashboard
  - a trends dashboard
- Saves both dashboards as PNG files

## Folder structure

```text
garmin-training-load-dashboard/
├─ README.md
├─ .gitignore
├─ data/
│  └─ activities.csv
├─ output/
├─ R/
│  ├─ functions.R
│  └─ run_dashboard.R
