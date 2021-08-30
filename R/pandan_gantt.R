#' Wrangle data for Gantt plot
#'
#' @export

wrangle_gantt_dat <- function(
  gantt_df = googlesheets4::read_sheet(Sys.getenv("PANDAN_GANTT"), "minigantt")
) {

    gantt_df %>%
    janitor::clean_names() %>%
    dplyr::mutate(
      start = lubridate::ymd(start),
      end = start + (optimistic + 4 * normal + pessimistic) / 6
    ) %>%
    dplyr::select(
      project, start, end, dplyr::everything()
    )

}

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
#' Trackers only for reading and writing, gantts for everything else.
#'
#' @export

pandan_gantt <- function() {

  gantt_plot <-
  wrangle_gantt_dat() %>%
    ggplot2::ggplot() +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = start,
        xend = end,
        y = project,
        yend = project
      )
    )

      plotly::ggplotly(p = gantt_plot,
                       tooltip = c("objective", "priorities"))


}
