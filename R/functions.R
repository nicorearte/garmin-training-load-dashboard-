library(tidyverse)
library(lubridate)
library(janitor)
library(patchwork)
library(stringr)
library(zoo)
library(scales)
library(readr)
library(tidyr)
library(ggplot2)
library(here)

# ----------------------------
# 1) CONFIG
# ----------------------------
weight_kg <- 78.6
age <- 39
height_cm <- 179
sex <- "male"

tsb_alert_low <- -10
tsb_very_low <- -15

acwr_low <- 0.80
acwr_high <- 1.30
acwr_very_high <- 1.50

# ----------------------------
# 2) FUNCTIONS
# ----------------------------
calc_ewma <- function(x, tau) {
  x <- tidyr::replace_na(as.numeric(x), 0)
  n <- length(x)

  if (n == 0) return(numeric(0))

  out <- numeric(n)

  initial_window <- min(7, n)
  out[1] <- mean(x[1:initial_window], na.rm = TRUE)

  if (n >= 2) {
    alpha <- 1 / tau
    for (i in 2:n) {
      out[i] <- out[i - 1] + alpha * (x[i] - out[i - 1])
    }
  }

  out
}

hms_to_min <- function(x) {
  suppressWarnings(as.numeric(period_to_seconds(hms(as.character(x))) / 60))
}

min_per_km_to_label <- function(x) {
  ifelse(
    is.na(x),
    NA_character_,
    sprintf("%d:%02d/km", floor(x), round((x - floor(x)) * 60))
  )
}

infer_rpe <- function(pace, km) {
  case_when(
    is.na(pace) | is.na(km) ~ NA_real_,
    km >= 24 ~ 7,
    km >= 18 ~ 6,
    pace <= 4.20 ~ 8,
    pace <= 4.45 ~ 7,
    pace <= 5.10 ~ 5,
    TRUE ~ 4
  )
}

classify_acwr_status <- function(x) {
  case_when(
    is.na(x) ~ "No data",
    x < acwr_low ~ "Low",
    x < acwr_high ~ "Optimal",
    x < acwr_very_high ~ "High",
    TRUE ~ "Very high"
  )
}

classify_tsb_status <- function(x) {
  case_when(
    is.na(x) ~ "No data",
    x <= tsb_very_low ~ "Very high fatigue",
    x <= tsb_alert_low ~ "High fatigue",
    x < 0 ~ "Moderate fatigue",
    TRUE ~ "Recovered"
  )
}

recommend_training <- function(acwr, tsb) {
  case_when(
    is.na(acwr) | is.na(tsb) ~ "Not enough data yet to recommend training load.",
    acwr >= acwr_very_high ~ "High risk: rest or very easy running is advisable.",
    acwr >= acwr_high & tsb <= tsb_alert_low ~ "High acute load with accumulated fatigue: reduce volume and intensity.",
    tsb <= tsb_very_low ~ "Very high fatigue: prioritize rest or active recovery.",
    acwr >= acwr_high ~ "High load: keep the session controlled and avoid very demanding quality work.",
    acwr < acwr_low & tsb >= 0 ~ "Low load and good freshness: good day for quality or a controlled long run.",
    tsb <= tsb_alert_low ~ "High fatigue: better to do easy running, mobility, or rest.",
    tsb < 0 ~ "Moderate fatigue: training is possible, but avoid overreaching.",
    TRUE ~ "Stable zone: you can continue with the normal plan."
  )
}

make_card <- function(title, value, subtitle = NULL, fill = "#1E88E5") {
  ggplot() +
    annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = fill, color = NA) +
    annotate("text", x = 0.05, y = 0.80, label = title, hjust = 0, color = "white", size = 5, fontface = "bold") +
    annotate("text", x = 0.05, y = 0.48, label = value, hjust = 0, color = "white", size = 11, fontface = "bold") +
    annotate("text", x = 0.05, y = 0.18, label = subtitle, hjust = 0, color = alpha("white", 0.9), size = 4.2) +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    theme_void()
}

validate_required_columns <- function(df, required_cols) {
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      paste(
        "Missing required columns in Garmin CSV:",
        paste(missing_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}
