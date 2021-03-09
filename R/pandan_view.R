#' See progress over time
#'
#' @export

pandan_view <- function(project = "all"){
  progress <- googlesheets4::read_sheet(Sys.getenv("PANDAN_PROGRESS"))
  projects <- googlesheets4::read_sheet(Sys.getenv("PANDAN_PROJECTS"))

  dat <-   progress %>%
    dplyr::left_join(projects, by = "project")

  if (project != "all") {
    dat <- progress %>%
      dplyr::filter(project == !!project)
  }

  completed <- projects %>%
    dplyr::filter(status == "completed") %>%
    dplyr::pull(project) %>%
    paste0(collapse = ", ") %>% {
    glue::glue("well done! you've completed: {.}.")
    }

  dat %>%
    # create label for legend
    dplyr::mutate(
      project_name = project,
      project = stringr::str_c(project, description, sep = " | ")) %>%
    dplyr::group_by(project) %>%
    dplyr::mutate(total = components * (levels + edit_n),
                  total_current = dplyr::last(total)) %>%
    dplyr::filter(status == "active") %>%
    ggplot2::ggplot(
      ggplot2::aes(x = date, y = writing/total_current,
                   group = project, colour = project)) +
    ggplot2::geom_line(alpha = 0.6) +
    ggplot2::geom_point(size = 4) +
    ggplot2::facet_grid(category ~ .) +
    ggthemes::theme_solarized() +
    rockthemes::scale_color_melloncollie(
    ) +
    ggplot2::labs(
      title = "pandan progress",
      subtitle = completed
    ) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.direction = "vertical",
      legend.title = ggplot2::element_text("project")
    ) +
    ggplot2::ylim(0,1)

}
