#' Open a pandan sheet
#'
#' @param tracker Specify tracker, progress, or projects to open sheet.
#' @param launch_browser Toggle url return.
#'
#' @export

pandan_tracker <- function(tracker = "tracker",
                           launch_browser = TRUE) {
  gs_url <- dplyr::case_when(
    tracker == "tracker" ~ "PANDAN_TRACKER",
    tracker == "progress" ~ "PANDAN_PROGRESS",
    tracker == "projects" ~ "PANDAN_PROJECTS",
    tracker == "gantt" ~ "PANDAN_GANTT"
  ) %>% Sys.getenv()

  googlesheets4::gs4_browse(gs_url)
}
