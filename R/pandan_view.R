#' See progress over time
#'
#' @export

pandan_view <- function(){
  progress <- googlesheets4::read_sheet(Sys.getenv("PANDAN_PROGRESS"))
  projects <- googlesheets4::read_sheet(Sys.getenv("PANDAN_PROJECTS"))

  dat <-   progress %>%
    dplyr::left_join(projects, by = "project")

  dat %>%
    dplyr::group_by(project) %>%
    dplyr::mutate(total = components * levels,
                  total_max = max(total)) %>%
    dplyr::filter(status == "active") %>%
    ggplot2::ggplot(
      ggplot2::aes(x = date, y = writing/total_max,
                   group = project, colour = project)) +
    ggplot2::geom_line(alpha = 0.6) +
    ggplot2::geom_point(size = 4) +
    ggplot2::facet_grid(category ~ .) +
    ggthemes::theme_solarized() +
    rockthemes::scale_color_melloncollie() +
    ggplot2::labs(
      title = "pandan progress"
    ) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank()
    ) +
    ggplot2::ylim(0,1)

}
