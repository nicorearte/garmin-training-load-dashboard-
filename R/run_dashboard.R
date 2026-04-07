source(here::here("R", "functions.R"))

# ----------------------------
# 3) PATHS
# ----------------------------
garmin_csv_path <- here::here("data", "activities.csv")
output_dir <- here::here("output")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# ----------------------------
# 4) IMPORT
# ----------------------------
garmin_raw <- readr::read_csv(garmin_csv_path, show_col_types = FALSE) %>%
  janitor::clean_names()

required_cols <- c(
  "fecha",
  "tipo_de_actividad",
  "distancia",
  "calorias"
)

validate_required_columns(garmin_raw, required_cols)

garmin <- garmin_raw %>%
  mutate(
    fecha = as.Date(fecha),
    activity_type = str_to_lower(tipo_de_actividad),
    distancia = as.numeric(distancia),
    calorias = as.numeric(calorias),
    fc = suppressWarnings(as.numeric(frecuencia_cardiaca_media)),
    tss = suppressWarnings(as.numeric(training_stress_score_r)),
    moving_time_min = case_when(
      "tiempo_en_movimiento" %in% names(.) && !is.na(tiempo_en_movimiento) ~ hms_to_min(tiempo_en_movimiento),
      "tiempo_transcurrido" %in% names(.) && !is.na(tiempo_transcurrido) ~ hms_to_min(tiempo_transcurrido),
      TRUE ~ hms_to_min(tiempo)
    )
  ) %>%
  filter(!is.na(fecha)) %>%
  filter(str_detect(activity_type, "carrera|running|cinta|treadmill"))

# ----------------------------
# 5) UNITS
# ----------------------------
garmin <- garmin %>%
  mutate(
    km = case_when(
      is.na(distancia) ~ 0,
      distancia > 1000 ~ distancia / 1000,
      TRUE ~ distancia
    ),
    km = ifelse(is.na(km) | is.infinite(km), 0, km),
    pace_min_km = ifelse(km > 0, moving_time_min / km, NA_real_)
  )

# ----------------------------
# 6) DAILY DATASET
# ----------------------------
daily <- garmin %>%
  group_by(fecha) %>%
  summarise(
    km = sum(km, na.rm = TRUE),
    minutes = sum(moving_time_min, na.rm = TRUE),
    tss = sum(tss, na.rm = TRUE),
    avg_hr = ifelse(all(is.na(fc)), NA_real_, mean(fc, na.rm = TRUE)),
    kcal = sum(calorias, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(fecha) %>%
  ungroup()

end_date <- Sys.Date()

daily <- daily %>%
  complete(
    fecha = seq.Date(min(fecha), end_date, by = "day")
  ) %>%
  arrange(fecha) %>%
  mutate(
    km = replace_na(km, 0),
    minutes = replace_na(minutes, 0),
    tss = replace_na(tss, 0),
    kcal = replace_na(kcal, 0),
    avg_hr = replace_na(avg_hr, NA_real_),
    pace_min_km = ifelse(km > 0, minutes / km, NA_real_),
    rpe = ifelse(km > 0, infer_rpe(pace_min_km, km), NA_real_),
    load = case_when(
      tss > 0 ~ tss,
      minutes > 0 & !is.na(rpe) ~ minutes * rpe,
      TRUE ~ 0
    ),
    trained = if_else(load > 0, 1, 0),
    day_type = if_else(trained == 1, "Training", "Rest")
  )

# ----------------------------
# 7) LOAD MODEL
# ----------------------------
daily <- daily %>%
  mutate(
    atl = calc_ewma(load, 7),
    ctl = calc_ewma(load, 42),
    tsb = ctl - atl,
    acwr = ifelse(ctl > 10, atl / ctl, NA_real_)
  )

# ----------------------------
# 8) ROLLING AVERAGES
# ----------------------------
daily <- daily %>%
  mutate(
    load_roll = zoo::rollmean(load, 3, fill = NA, align = "right"),
    acwr_roll = zoo::rollmean(acwr, 3, fill = NA, align = "right")
  )

# ----------------------------
# 9) THEME
# ----------------------------
theme_dashboard <- theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10),
    legend.position = "top"
  )

# ----------------------------
# 10) CHARTS
# ----------------------------

g_load <- daily %>%
  pivot_longer(c(atl, ctl), names_to = "metric", values_to = "value") %>%
  ggplot(aes(fecha, value, color = metric)) +
  geom_line(linewidth = 1.1) +
  scale_x_date(date_labels = "%d-%b") +
  labs(
    title = "ATL vs CTL",
    subtitle = "Acute fatigue vs chronic load",
    color = NULL
  ) +
  theme_dashboard

g_tsb <- ggplot(daily, aes(fecha, tsb)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_hline(yintercept = tsb_alert_low, linetype = 3) +
  geom_hline(yintercept = tsb_very_low, linetype = 3) +
  geom_line(linewidth = 1.1) +
  scale_x_date(date_labels = "%d-%b") +
  labs(
    title = "TSB",
    subtitle = "Training stress balance"
  ) +
  theme_dashboard

g_daily_load <- ggplot(daily, aes(fecha, load)) +
  geom_col() +
  geom_line(aes(y = load_roll), linetype = 2, linewidth = 1) +
  scale_x_date(date_labels = "%d-%b") +
  labs(
    title = "Daily Load",
    subtitle = "Daily bars + 3-day rolling average"
  ) +
  theme_dashboard

g_acwr <- ggplot(daily, aes(fecha, acwr)) +
  geom_hline(yintercept = acwr_low, linetype = 2) +
  geom_hline(yintercept = acwr_high, linetype = 2) +
  geom_hline(yintercept = acwr_very_high, linetype = 3) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2, na.rm = TRUE) +
  scale_x_date(date_labels = "%d-%b") +
  labs(
    title = "Acute / Chronic Workload Ratio (ACWR)",
    subtitle = "Main load-risk indicator"
  ) +
  theme_dashboard

# ----------------------------
# 11) MAIN DASHBOARD
# ----------------------------
dashboard_main <- (g_load | g_tsb) / (g_daily_load | g_acwr)

print(dashboard_main)

# ----------------------------
# 12) EXECUTIVE DASHBOARD
# ----------------------------
latest <- daily %>%
  filter(fecha == max(fecha, na.rm = TRUE)) %>%
  mutate(
    acwr_status = classify_acwr_status(acwr),
    tsb_status = classify_tsb_status(tsb),
    recommendation = recommend_training(acwr, tsb),
    acwr_label = ifelse(is.na(acwr), "No data", scales::number(acwr, accuracy = 0.01, decimal.mark = ".")),
    tsb_label = ifelse(is.na(tsb), "No data", scales::number(tsb, accuracy = 0.1, decimal.mark = ".")),
    atl_label = ifelse(is.na(atl), "No data", scales::number(atl, accuracy = 0.1, decimal.mark = ".")),
    ctl_label = ifelse(is.na(ctl), "No data", scales::number(ctl, accuracy = 0.1, decimal.mark = "."))
  )

color_acwr <- case_when(
  latest$acwr_status == "Optimal" ~ "#2E7D32",
  latest$acwr_status == "Low" ~ "#1565C0",
  latest$acwr_status == "High" ~ "#EF6C00",
  latest$acwr_status == "Very high" ~ "#C62828",
  TRUE ~ "#616161"
)

color_tsb <- case_when(
  latest$tsb_status == "Recovered" ~ "#2E7D32",
  latest$tsb_status == "Moderate fatigue" ~ "#EF6C00",
  latest$tsb_status == "High fatigue" ~ "#C62828",
  latest$tsb_status == "Very high fatigue" ~ "#8E0000",
  TRUE ~ "#616161"
)

color_recommendation <- case_when(
  str_detect(latest$recommendation, "High risk|rest") ~ "#C62828",
  str_detect(latest$recommendation, "reduce volume|High load|High fatigue|Very high fatigue") ~ "#EF6C00",
  str_detect(latest$recommendation, "good day for quality") ~ "#2E7D32",
  TRUE ~ "#455A64"
)

card_acwr <- make_card(
  "Current ACWR",
  latest$acwr_label,
  paste("Status:", latest$acwr_status),
  fill = color_acwr
)

card_tsb <- make_card(
  "Current TSB",
  latest$tsb_label,
  paste("Status:", latest$tsb_status),
  fill = color_tsb
)

card_fitness <- make_card(
  "ATL / CTL",
  paste0(latest$atl_label, " / ", latest$ctl_label),
  "Acute fatigue / chronic load",
  fill = "#5E35B1"
)

panel_recommendation <- ggplot() +
  annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = color_recommendation, color = NA) +
  annotate("text", x = 0.03, y = 0.82, label = "Automatic recommendation", hjust = 0, color = "white", size = 6, fontface = "bold") +
  annotate("text", x = 0.03, y = 0.48, label = str_wrap(latest$recommendation, width = 55), hjust = 0, color = "white", size = 5) +
  annotate("text", x = 0.03, y = 0.15, label = paste("Analysis date:", format(latest$fecha, "%d-%m-%Y")), hjust = 0, color = alpha("white", 0.9), size = 4) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
  theme_void()

gauge_acwr <- ggplot() +
  geom_rect(aes(xmin = 0, xmax = acwr_low, ymin = 0, ymax = 1), fill = "#1565C0") +
  geom_rect(aes(xmin = acwr_low, xmax = acwr_high, ymin = 0, ymax = 1), fill = "#2E7D32") +
  geom_rect(aes(xmin = acwr_high, xmax = acwr_very_high, ymin = 0, ymax = 1), fill = "#EF6C00") +
  geom_rect(aes(xmin = acwr_very_high, xmax = 2.0, ymin = 0, ymax = 1), fill = "#C62828") +
  annotate("text", x = 1.0, y = 1.18, label = "ACWR Gauge", fontface = "bold", size = 5) +
  annotate("text", x = 0.40, y = -0.15, label = "Low", size = 3.5) +
  annotate("text", x = 1.05, y = -0.15, label = "Optimal", size = 3.5) +
  annotate("text", x = 1.40, y = -0.15, label = "High", size = 3.5) +
  annotate("text", x = 1.75, y = -0.15, label = "Very high", size = 3.5) +
  scale_x_continuous(limits = c(0, 2), expand = c(0, 0)) +
  coord_cartesian(ylim = c(-0.2, 1.3), clip = "off") +
  theme_void()

if (!is.na(latest$acwr)) {
  gauge_acwr <- gauge_acwr +
    geom_vline(xintercept = latest$acwr, linewidth = 2, color = "black")
}

last_21 <- daily %>%
  filter(fecha >= max(fecha, na.rm = TRUE) - 20)

g_recent_acwr <- ggplot(last_21, aes(fecha, acwr)) +
  geom_hline(yintercept = acwr_low, linetype = 2) +
  geom_hline(yintercept = acwr_high, linetype = 2) +
  geom_hline(yintercept = acwr_very_high, linetype = 3) +
  geom_line(linewidth = 1.2) +
  scale_x_date(date_labels = "%d-%b") +
  labs(
    title = "Recent ACWR Trend",
    subtitle = "Last 21 days"
  ) +
  theme_dashboard

g_recent_tsb <- ggplot(last_21, aes(fecha, tsb)) +
  geom_hline(yintercept = 0, linetype = 2) +
  geom_hline(yintercept = tsb_alert_low, linetype = 3) +
  geom_hline(yintercept = tsb_very_low, linetype = 3) +
  geom_line(linewidth = 1.2) +
  scale_x_date(date_labels = "%d-%b") +
  labs(
    title = "Recent TSB Trend",
    subtitle = "Fatigue and recovery"
  ) +
  theme_dashboard

# ----------------------------
# 13) KPI / TRAFFIC LIGHT DASHBOARD
# ----------------------------
dashboard_kpis <- (card_acwr | card_tsb | card_fitness) /
  (panel_recommendation | gauge_acwr) +
  plot_annotation(
    title = "Executive Dashboard",
    subtitle = "Load, fatigue and automatic recommendation",
    theme = theme(
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 11)
    )
  )

# ----------------------------
# 14) TREND DASHBOARD
# ----------------------------
dashboard_risk <- (g_recent_acwr | g_recent_tsb)

dashboard_trends <- dashboard_main / dashboard_risk +
  plot_annotation(
    title = "Trend Dashboard",
    subtitle = "Load, fatigue and recent evolution",
    theme = theme(
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 11)
    )
  )

print(dashboard_kpis)
print(dashboard_trends)

# ----------------------------
# 15) OPTIONAL: SAVE DASHBOARDS AS PNG
# ----------------------------
ggsave(
  filename = file.path(output_dir, "dashboard_kpis.png"),
  plot = dashboard_kpis,
  width = 16,
  height = 10,
  dpi = 300
)

ggsave(
  filename = file.path(output_dir, "dashboard_trends.png"),
  plot = dashboard_trends,
  width = 16,
  height = 14,
  dpi = 300
)
