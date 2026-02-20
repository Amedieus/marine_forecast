##' Make Example Visualizations (Coastal Targets)
##' @param target_long_daily long daily data from process_targets()$target_long_daily
##' @param out_dir output directory for figures (default outputs/figures)
##' @param max_sites number of sites to include in example plots (default 6)
##' @return character vector of written file paths
make_example_visualizations <- function(target_long_daily,
                                        out_dir = "outputs/figures",
                                        max_sites = 6) {
  
  req <- c("datetime", "site_id", "variable", "observation")
  if (!all(req %in% names(target_long_daily))) {
    stop(
      "target_long_daily is missing required columns: ",
      paste(setdiff(req, names(target_long_daily)), collapse = ", ")
    )
  }
  
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Pick a small number of sites so plots remain readable
  sites <- sort(unique(target_long_daily$site_id))
  site_show <- head(sites, max_sites)
  
  df <- target_long_daily %>%
    dplyr::filter(site_id %in% site_show)
  
  # -------------------------
  # 1) Full time series
  # -------------------------
  p_ts <- ggplot2::ggplot(df, ggplot2::aes(x = datetime, y = observation)) +
    ggplot2::geom_line() +
    ggplot2::facet_grid(variable ~ site_id, scales = "free_y") +
    ggplot2::labs(
      title = "Coastal targets: time series (example sites)",
      x = "Date",
      y = "Observation"
    )
  
  f1 <- file.path(out_dir, "time_series_example_sites.png")
  ggplot2::ggsave(f1, p_ts, width = 14, height = 8, dpi = 150)
  
  # -------------------------
  # 2) Last 60 days
  # -------------------------
  max_date <- max(df$datetime, na.rm = TRUE)
  df_recent <- df %>% dplyr::filter(datetime >= (max_date - 60))
  
  p_recent <- ggplot2::ggplot(df_recent, ggplot2::aes(x = datetime, y = observation)) +
    ggplot2::geom_line() +
    ggplot2::facet_grid(variable ~ site_id, scales = "free_y") +
    ggplot2::labs(
      title = "Coastal targets: last 60 days (example sites)",
      x = "Date",
      y = "Observation"
    )
  
  f2 <- file.path(out_dir, "last_60_days_example_sites.png")
  ggplot2::ggsave(f2, p_recent, width = 14, height = 8, dpi = 150)
  
  # -------------------------
  # 3) Seasonal pattern (DOY median + IQR)
  # -------------------------
  df_season <- df %>%
    dplyr::mutate(doy = lubridate::yday(datetime)) %>%
    dplyr::group_by(site_id, variable, doy) %>%
    dplyr::summarise(
      p25 = stats::quantile(observation, 0.25, na.rm = TRUE),
      p50 = stats::quantile(observation, 0.50, na.rm = TRUE),
      p75 = stats::quantile(observation, 0.75, na.rm = TRUE),
      .groups = "drop"
    )
  
  p_season <- ggplot2::ggplot(df_season, ggplot2::aes(x = doy, y = p50)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = p25, ymax = p75), alpha = 0.2) +
    ggplot2::geom_line() +
    ggplot2::facet_grid(variable ~ site_id, scales = "free_y") +
    ggplot2::labs(
      title = "Coastal targets: seasonal pattern (median with IQR)",
      x = "Day of year",
      y = "Observation"
    )
  
  f3 <- file.path(out_dir, "seasonal_pattern_example_sites.png")
  ggplot2::ggsave(f3, p_season, width = 14, height = 8, dpi = 150)
  
  # Return written files
  c(f1, f2, f3)
}
