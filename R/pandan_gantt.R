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

  gantt_dat <-
    wrangle_gantt_dat()

  events <-
    googlesheets4::read_sheet(Sys.getenv("PANDAN_GANTT"), "events") %>%
    dplyr::mutate(
      date = lubridate::ymd(date)
    ) %>%
    dplyr::bind_rows(
      tibble::tibble(
      date = lubridate::today(),
      event = "today")
    ) %>%
    dplyr::mutate(
      id = dplyr::row_number()
    ) %>%
    dplyr::arrange(desc(id)) %>%
    dplyr::select(-id)

  #gantt_plot <-
    gantt_dat %>%
    dplyr::mutate(
      month = lubridate::month(start, label = TRUE, abbr = FALSE)
    ) %>%
    ggplot2::ggplot() +
    ggplot2::geom_vline(
      data = events,
      ggplot2::aes(
        xintercept = date,
        linetype = event
      ),
      alpha = 0.4
    ) +
      ggplot2::geom_vline(
        xintercept = lubridate::today(),
        colour = "darkgreen",
        size = 1.5,
        alpha = 0.2
      ) +
    ggplot2::geom_segment(
      ggplot2::aes(
        x = start,
        xend = end,
        y = project,
        yend = project
      ),
      size = 10,
      alpha = 0.3
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        x = start,
        y = project,
        label = stringr::str_wrap(objective, 80)
      ),
      size = 3.5,
      nudge_y = 0.3,
      hjust = 0,
      alpha = 0.8
    ) +
    ggthemes::theme_solarized_2() +
    ggplot2::labs(
      title = "Stuff Charles is working on",
      x = "date"
    ) +
      ggplot2::theme(
        legend.position = "bottom"
      ) +
      ggplot2::facet_wrap(~ month, scales = "free")

  # gantt_gt <-
  #   gantt_dat %>%
  #   dplyr::select(project, objective)



      # plotly::ggplotly(p = gantt_plot,
      #                  tooltip = c("objective", "priorities"))


}
