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

weight_kg <- 78.6
age <- 39
height_cm <- 179
sex <- "male"

tsb_alert_low <- -10
tsb_very_low <- -15

acwr_low <- 0.80
acwr_high <- 1.30
acwr_very_high <- 1.50

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

find_first_existing <- function(df, candidates, required = FALSE, label = NULL) {
  nm <- names(df)
  hits <- candidates[candidates %in% nm]
  if (length(hits) == 0) {
    if (required) {
      stop(
        paste0(
          "Could not find required column for ",
          ifelse(is.null(label), "field", label),
          ". Checked: ",
          paste(candidates, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    return(NA_character_)
  }
  hits[1]
}

parse_garmin_date <- function(x) {
  x_chr <- as.character(x)
  parsed <- suppressWarnings(lubridate::ymd(x_chr, quiet = TRUE))
  if (all(is.na(parsed))) parsed <- suppressWarnings(lubridate::mdy(x_chr, quiet = TRUE))
  if (all(is.na(parsed))) parsed <- suppressWarnings(lubridate::dmy(x_chr, quiet = TRUE))
  if (all(is.na(parsed))) parsed <- as.Date(x_chr)
  as.Date(parsed)
}

standardize_garmin_columns <- function(df) {
  df <- janitor::clean_names(df)

  date_col <- find_first_existing(df, c("fecha", "date"), TRUE, "date")
  activity_col <- find_first_existing(df, c("tipo_de_actividad", "activity_type", "activity_type_name"), TRUE, "activity type")
  distance_col <- find_first_existing(df, c("distancia", "distance"), TRUE, "distance")
  calories_col <- find_first_existing(df, c("calorias", "calories"), TRUE, "calories")

  moving_time_col <- find_first_existing(df, c("tiempo_en_movimiento", "moving_time"), FALSE, "moving time")
  elapsed_time_col <- find_first_existing(df, c("tiempo_transcurrido", "elapsed_time"), FALSE, "elapsed time")
  time_col <- find_first_existing(df, c("tiempo", "time"), FALSE, "time")
  hr_col <- find_first_existing(df, c("frecuencia_cardiaca_media", "average_heart_rate"), FALSE, "average heart rate")
  tss_col <- find_first_existing(df, c("training_stress_score_r", "training_stress_score"), FALSE, "training stress score")

  df %>%
    mutate(
      fecha = parse_garmin_date(.data[[date_col]]),
      activity_type = str_to_lower(as.character(.data[[activity_col]])),
      distance_raw = suppressWarnings(as.numeric(.data[[distance_col]])),
      calories_raw = suppressWarnings(as.numeric(.data[[calories_col]])),
      avg_hr_raw = if (!is.na(hr_col)) suppressWarnings(as.numeric(.data[[hr_col]])) else NA_real_,
      tss_raw = if (!is.na(tss_col)) suppressWarnings(as.numeric(.data[[tss_col]])) else NA_real_,
      moving_time_min = case_when(
        !is.na(moving_time_col) ~ hms_to_min(.data[[moving_time_col]]),
        !is.na(elapsed_time_col) ~ hms_to_min(.data[[elapsed_time_col]]),
        !is.na(time_col) ~ hms_to_min(.data[[time_col]]),
        TRUE ~ NA_real_
      )
    )
}

is_running_activity <- function(x) {
  str_detect(str_to_lower(as.character(x)), "carrera|running|run|corrida|treadmill|cinta")
}
