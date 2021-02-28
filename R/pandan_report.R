#' Report
#'
#' @inheritParams pandan_dat
#' @param update Update progress log.
#'
#' @export

pandan_report <- function(
  project,
  gs_url = Sys.getenv("PANDAN_MS"),
  update = FALSE
  ) {
  dat <- suppressMessages(
    pandan_dat(project, gs_url)
  )

  report_dat <-
  tibble::tibble(
    project = project,
    writing = dat$components$completed %>% sum(),
    editing = dat$editing$completed %>% sum(),
    components = dat$sections %>% length(),
    levels = dat$levels %>% length()
  )

  if (isTRUE(update)) {
    googlesheets4::sheet_append(
      ss = "https://docs.google.com/spreadsheets/d/1G_35kjbqgnl6L983TR6ccGiikqk9vOmB7yptD5z3yyM/edit#gid=0",
      data = report_dat
    )
  }

  return(report_dat)
}
