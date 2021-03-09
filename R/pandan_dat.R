#' Get project data
#'
#' Import project-level data from googlesheets.
#'
#' @param project Project tracking sheet. First column for
#' components, second to some number of columns for project level progress,
#' and any column name containing `read_edit` will be classified as reading over
#' and editing, not project progress.
#' @param gs_url Url for googlesheet. Defaults to system environment variable
#' `PANDAN`. `usethis::edit_r_environ()` to set this.
#'
#' @return List of object of project metadata useful for visualisation, e.g.,
#' current levels and current sections.
#'
#' @examples
#' # runs because PANDAN url environment variable set and
#' # pandan googlesheet at url
#' pandan_dat("waystation")
#'
#' @export

pandan_dat <- function(
  project,
  gs_url = Sys.getenv("PANDAN_TRACKER")
) {

  # read data
  alldat <-
    googlesheets4::read_sheet(
    ss = gs_url,
    sheet = project
    )

  # divide into project and reading
  component_progress <-
    alldat %>%
    dplyr::select(1, !dplyr::starts_with("read_edit")) %>%
    dplyr::mutate(components = as.character(components))

  # levels
  levels <- component_progress %>%
    dplyr::select(-components) %>%
    names

  edting_progress <-
    alldat %>%
    dplyr::select(1, dplyr::starts_with("read_edit"))

  pandan <-
  list(
  components =
    component_progress %>%
    dplyr::select(components) %>%
    dplyr::mutate(
      completed = component_progress %>%
        dplyr::select(-components) %>%
        apply(MARGIN = 1, FUN = sum)
    ),

  editing =
    edting_progress %>%
    dplyr::select(components) %>%
    dplyr::mutate(
      completed = edting_progress %>%
        dplyr::select(-components) %>%
        apply(MARGIN = 1, FUN = sum)
    ),
  sections = component_progress %>%
    dplyr::pull(components),
  levels = levels,
  edit_n = alldat %>%
    dplyr::select(dplyr::starts_with("read_edit"))
  %>% ncol()
  )

  class(pandan) <- "pandan_project"

  return(pandan)


}
