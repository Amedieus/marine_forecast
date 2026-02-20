library(tidyverse)
library(lubridate)

source("01_download_data.R")
source("02_process_data.R")
source("03_make_visuals.R")

target <- download_targets()
processed <- process_targets(target)

files <- make_example_visualizations(processed$target_long_daily)

dir.create("logs", showWarnings = FALSE, recursive = TRUE)
writeLines(c(
  "RUN SUMMARY",
  paste("Raw rows:", nrow(target)),
  paste("Daily rows:", nrow(processed$target_long_daily)),
  paste("Sites:", length(unique(processed$target_long_daily$site_id))),
  paste("Variables:", paste(sort(unique(processed$target_long_daily$variable)), collapse = ", ")),
  paste("Figures:", paste(basename(files), collapse = ", "))
), "logs/run_summary.txt")
