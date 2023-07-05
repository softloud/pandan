#' Report
#'
#' @inheritParams pandan_dat
#' @param update Update progress log.
#'
#' @export

pandan_report <- function(
  project,
  gs_url = Sys.getenv("PANDAN_TRACKER"),
  update = FALSE
  ) {
  dat <- suppressMessages(
    pandan_dat(project, gs_url)
  )

  report_dat <-
  tibble::tibble(
    date = lubridate::now(),
    project = project,
    writing = dat$components$completed %>% sum(),
    editing = dat$editing$completed %>% sum(),
    components = dat$sections %>% length(),
    levels = dat$levels %>% length(),
    edit_n = dat$edit_n
  )

  if (isTRUE(update)) {
    googlesheets4::sheet_append(
      ss = Sys.getenv("PANDAN_PROGRESS"),
      data = report_dat
    )
  }

  return(report_dat)
}
