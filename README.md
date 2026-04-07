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
```

## Required packages

```r
install.packages(c(
  "tidyverse",
  "lubridate",
  "janitor",
  "patchwork",
  "stringr",
  "zoo",
  "scales",
  "readr",
  "tidyr",
  "ggplot2",
  "here"
))
```

## How to use

1. Export your activities from Garmin Connect.
2. Save the file as:

```text
data/activities.csv
```

3. Run:

```r
source("R/run_dashboard.R")
```

4. The generated dashboard images will be saved in:

```text
output/
```

## Notes

- This version is focused on running only.
- If TSS is missing, the script estimates load using session duration and inferred RPE.
- Column names may vary depending on Garmin export language and format.
- The script expects Garmin exports with Spanish column names by default, because it was built from a Spanish-language export.

## License

MIT
