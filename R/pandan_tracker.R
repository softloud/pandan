#' Open a pandan sheet
#'
#' @param tracker Specify tracker, progress, or projects to open sheet.
#'
#'
#' @export

pandan_tracker <- function(
  tracker = "project"
  ) {

  gs_url <- dplyr::case_when(
    tracker == "tracker" ~ "PANDAN_TRACKER",
    tracker == "progress" ~ "PANDAN_PROGRESS",
    tracker == "projects" ~ "PANDAN_PROJECTS"
  ) %>% Sys.getenv()

  googlesheets4::gs4_browse(gs_url)
}
