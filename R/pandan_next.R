#' Get next actions
#'
#' Next three things to write and where to read from.
#'
#' @inheritParams pandan_dat
#' @param gs_projects Url for projects `pandan_tracker("projects)`.
#'
#' @export

pandan_next <- function(project,
                        gs_url = Sys.getenv("PANDAN_TRACKER"),
                        gs_projects = Sys.getenv("PANDAN_PROJECTS")) {

  # get data
  dat <- suppressMessages(pandan_dat(project, gs_url))

  # project data
  project_meta <- googlesheets4::read_sheet(gs_projects) %>%
    dplyr::filter(project == !!project)

  project_category <- project_meta %>% dplyr::pull(category)
  project_desc <- project_meta %>% dplyr::pull(description)

  # assign tasks to components
  next_actions <-
    dat %>%
    purrr::pluck("components") %>%
    dplyr::mutate(
      next_action = dplyr::if_else(
        completed <= length(components),
        dat$levels[completed + 1],
        "completed"
      )
    ) %>%
    dplyr::arrange(completed) %>%
    head(1)

  read_from <-
    dat %>%
    purrr::pluck("editing") %>%
    dplyr::arrange(completed) %>%
    dplyr::slice(1) %>%
    dplyr::pull(components)

  next_actions %>%
    dplyr::select(
      Section = components,
      Next = next_action
    ) %>%
    gt::gt() %>%
    gt::tab_header(
      title = glue::glue("Next actions in {project} {project_category}"),
      subtitle = project_desc
    ) %>%
    gt::tab_source_note(
      glue::glue("Then read from Section {read_from}.")
    )

}
