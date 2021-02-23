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
#' project_dat("pandan")
#'
#' @export

pandan_dat <- function(
  project,
  gs_url = Sys.getenv("PANDAN_MS")
) {

  # read data
  alldat <-
    googlesheets4::read_sheet(
    ss = gs_url,
    sheet = project
    )

  # metadata
  # complete this as I build the visualisation

  # divide into project and reading
  pdat <-
    alldat %>%
    dplyr::select(1, !contains("read_edit"))

  edat <-
    alldat %>%
    dplyr::select(1, contains("read_edit"))

  pandan <-
  list(
  components =
    pdat %>%
    dplyr::select(components) %>%
    dplyr::mutate(
      completed = pdat %>%
        dplyr::select(-components) %>%
        apply(MARGIN = 1, FUN = sum)
    ),

  editing =
    edat %>%
    dplyr::select(components) %>%
    dplyr::mutate(
      completed = edat %>%
        dplyr::select(-components) %>%
        apply(MARGIN = 1, FUN = sum)
    ),

  levels = pdat %>% dplyr::select(-components) %>% names,
  components = pdat %>% dplyr::pull(components)

  )

  class(pandan) <- "pandan_project"

  return(pandan)


}
