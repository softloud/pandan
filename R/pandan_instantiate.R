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
#'
#' @export

pandan_instantiate <- function (project,
                                objective,
                                category,
                                gs_url = Sys.getenv("PANDAN_TRACKER")) {

  # check inputs
  assertthat::assert_that(is.character(project) && !stringr::str_detect(project, "\\s"), msg = "Single-word project signfier")
  assertthat::assert_that(is.character(objective), msg = "What would signify this project as completed?")
  assertthat::assert_that(is.character(category), msg = "Specify if manuscript, coursework, site, etc. This is the facet label in the view.")


  # create project tracker


  googlesheets4::sheet_copy(
    from_ss = "https://docs.google.com/spreadsheets/d/1UXHQZcMa-6J_UkS-LO7bvX7lLVT7zB3kbROAxBf2Gpk/edit#gid=1712702589",
    from_sheet = "template",
    to_sheet = project,
    .after = "template"
  )

  # add to project list
  googlesheets4::sheet_append(
    ss = "https://docs.google.com/spreadsheets/d/1q1dJMjJRVgH6nFW_59RngF4YBkHX1RW2ZgekZVIE_zY/edit#gid=0",
    data = tibble::tibble(
      project = project,
      description = objective,
      status = "active",
      category = category
    )
  )

  # update progress log
  pandan_report(project,
                gs_url,
                update = TRUE)
}
