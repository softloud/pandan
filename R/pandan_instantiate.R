#' Instantiate a project
#'
#' Copy the project template to sheet.
#'
#' Add project name, description, and url to [pandan projects](https://docs.google.com/spreadsheets/d/1q1dJMjJRVgH6nFW_59RngF4YBkHX1RW2ZgekZVIE_zY/edit#gid=0).
#'
#' Add 0 progress to pandan progress log.
#'
#' @inheritParams pandan_report
#' @param objective A string describing what finished looks like.
#' @param category Specify talk or manuscript.
#' @param group Specify meta category
#'
#' @export

pandan_instantiate <- function (project,
                                objective,
                                category,
                                group,
                                gs_url = Sys.getenv("PANDAN_TRACKER")) {

  # check inputs
  assertthat::assert_that(is.character(project) && !stringr::str_detect(project, "\\s"), msg = "Single-word project signfier")
  assertthat::assert_that(is.character(objective), msg = "What would signify this project as completed?")
  assertthat::assert_that(is.character(category), msg = "Specify if manuscript, coursework, site, etc. This is the facet label in the view.")


  # create project tracker
  googlesheets4::sheet_copy(
    from_ss = Sys.getenv("PANDAN_PROJECTS"),
    from_sheet = "template",
    to_sheet = project,
    .after = "template"
  )

  # add to project list
  googlesheets4::sheet_append(
    ss = Sys.getenv("PANDAN_PROJECTS"),
    data = tibble::tibble(
      project = project,
      description = objective,
      status = "active",
      category = category,
      group = group
    )
  )

  # update progress log
  pandan_report(project,
                gs_url,
                update = TRUE)

  print("Check project description in gs; opening sheet in browser.")
  pandan_tracker("projects")
}
