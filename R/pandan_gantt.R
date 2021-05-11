#' Gantt chart
#'
#' A gantt chart is a type of bar chart with tasks performed in the y axis
#' and **time** in the x axis. The horizontal width of the bar represents
#' a duration of time spent on a task within a **project**.
#'
#' The standard structure (according to wikipedia) is a data table of the form
#'
#' task | predecessors |optimistic (O)| normal (M) | pessimistic (P) | expected_time (E)
#'
#' Predecessors can can contain multiple values, so will need an argument for
#' separator.
#'
#' The expected time is estimated using a Beta distribution statistic,
#'
#' $$
#' (O + 4M + P) / 6
#' $$
#'
#' @param gantt_url gs with sheets gantt and project key and meta.
#' gantt has columns: predecessors	optimistic	normal	pessimistic
#' status added
#'
#' where time is measured in days.
#' @param predecessor_sep Separator for predecessors, defaults to semicolon with any amount of white space around.

pandan_gantt <- function(gantt_url = Sys.getenv("PANDAN_GANTT"),
                         predecessor_sep = "\\s*;\\s*") {


  gantt_df <- googlesheets4::read_sheet(gantt_url, "gantt")
  project_key <- googlesheets4::read_sheet(gantt_url, "project_key")
  meta_df <- googlesheets4::read_sheet(gantt_url, "meta")

  wrangle_plotdat <-
    gantt_df %>%
    dplyr::mutate(
      expected = (optimistic + 4 * normal + pessimistic) / 6,
      predecessors = dplyr::if_else(is.na(predecessors),
                                    max(gantt_df$row) + 1,
                                    predecessors)
    ) %>%
    dplyr::arrange(project, desc(predecessors)) %>%
    dplyr::select(project, row, task, status, predecessors, expected) %>%
    dplyr::group_by(project) %>%
    dplyr::mutate(
      end = cumsum(expected),
      start = end - expected,
      row = as.character(row)
    ) %>%
    dplyr::select(-predecessors) %>%
    tidyr::pivot_longer(
      cols = c(start, end),
      names_to = "terminal",
      values_to = "x"
    ) %>%
    dplyr::mutate(x = lubridate::ymd(meta_df$started) + x)


  gantt_plot <-
    wrangle_plotdat %>%
    ggplot2::ggplot(ggplot2::aes(
      x = x,
      y = row,
      group = task,
      colour = status
    )) +
    ggplot2::geom_line(# ggplot2::aes(alpha = status),
      alpha = 0.5,
      size = 5)  +
    ggplot2::facet_grid(project ~ .,
                        scales = "free_y") +
    ggplot2::scale_color_grey(labels = NULL) +
    ggplot2::labs(
      title = meta_df$title,
      # subtitle = meta_df$subtitle,
      subtitle = stringr::str_wrap(
        "This Gantt chart shows the expected value,
        estimated using a beta distribution,
        (optimistic + 4 x normal + pessimistic) / 6.

        The horizontal components show different projects.
        "
      )
    )

  colors <- tibble::deframe(ggthemes::ggthemes_data[["fivethirtyeight"]])

  with_theme <-
    gantt_plot +
    ggplot2::theme(
      line = ggplot2::element_line(colour = "black"),
      rect = ggplot2::element_rect(
        fill = colors["Light Gray"],
        linetype = 0,
        colour = NA
      ),
      text = ggplot2::element_text(colour = colors["Dark Gray"]),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      legend.background = ggplot2::element_rect(),
      legend.position = "bottom",
      legend.direction = "horizontal",
      # legend.box = "vertical",
      panel.grid = ggplot2::element_line(colour = NULL),
      panel.grid.major = ggplot2::element_line(colour = colors["Medium Gray"]),
      # panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(
        hjust = 0,
        size = ggplot2::rel(1.5),
        face = "bold"
      ),
      plot.margin = ggplot2::unit(c(1,
                           1, 1, 1), "lines"),
      strip.background = ggplot2::element_rect()
    )

plotly::ggplotly(p = gantt_plot,
                 tooltip = c("task", "status", "outlook"))


}
