library(googlesheets4)

gantt_dat <-
  read_sheet(
    pandan_tracker("gantt", launch_browser = FALSE),
    "gantt"
  ) %>%
  janitor::clean_names()

readr::write_csv(gantt_dat, "data-raw/gantt-gs.csv")


