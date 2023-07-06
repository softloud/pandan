gantt_pal <- list(
  murky_gold = "darkgoldenrod4",
  murky_blue = "cadetblue4",
  murky_grey = "azure4",
  murky_green = "darkolivegreen"
)


status_levels <- tibble::tribble(
  ~status, ~colour,
  "completed", gantt_pal$murky_green,
  "overdue", gantt_pal$murky_gold,
  "due", gantt_pal$murky_gold,
  "in progress", gantt_pal$murky_blue,
  "scheduled", gantt_pal$murky_grey,
  "sometime", gantt_pal$murky_grey,
  "project", gantt_pal$murky_grey
)

#' Wrangle data for Gantt plot
#'
#' @export

wrangle_gantt_dat <- function() {
  googlesheets4::gs4_deauth()
  gantt_df <- googlesheets4::read_sheet(Sys.getenv("PANDAN_GANTT"),
                                        col_types = c("iccccDciDDcc"))

  gantt_df %>%
    janitor::clean_names() %>%
    # convert to lubridate
    mutate(across(where(is.Date), lubridate::ymd)) %>%
    mutate(
      status = case_when(
        is.na(task) ~ "project",!is.na(status) ~ status,
          !is.na(end) ~ "completed",!is.na(due) &
          due < today() ~ "overdue",
        due == today() ~ "due",
        start > today() ~ "scheduled",!is.na(start) ~ "in progress",
        is.na(start) ~ "sometime",
        .default = "this is an error"
      )
    ) %>%
    dplyr::filter(!str_detect(status, "dropped")) %>%
    mutate(
      # calculate predicted end - must happen after completed
      end = if_else(
        is.na(end) & !is.na(predicted_days),
        start +
          days(predicted_days - 1),
        end
      ),
      # update overdue
      status = case_when(
        status == "in progress" & end < today() ~ "overdue",
        status == "in progress" & end == today() ~ "due",
        .default = status
      ),
      # add levels to status
      status = as_factor(status) %>% fct_relevel(status_levels$status)
    ) %>%
    mutate(# set levels for fokus
      fokus = as_factor(fokus) %>%
        fct_relevel("phi", "theta", "psi", "pi"),) %>%
    arrange(fokus, due, project) %>%
    dplyr::ungroup()
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

pandan_gantt <- function(font_size = 20) {
  gantt_dat <-
    wrangle_gantt_dat()

  gantt_min_df <- gantt_dat %>%
    dplyr::filter(status %in% c("overdue", "due", "in progress")) %>%
    dplyr::filter(start == min(start, na.rm = TRUE))

  gantt_min <-
    if (nrow(gantt_min_df) == 0)
      return(today())
  else
    pluck(gantt_min_df, "start", 1)

  # set to today?

  gantt_max <- gantt_dat %>%
    dplyr::filter(end == max(end, na.rm = TRUE)) %>%
    pluck("end", 1)


  gantt_dat %>%
    ggplot() +
    theme_minimal(base_family = "serif", base_size = font_size) +
    # today
    geom_rect(
      data = NULL,
      aes(
        xmin = today(),
        xmax = today() + 1,
        ymin = -Inf,
        ymax = Inf
      ),
      # linetype = "dotted",
      alpha = 0.1,
      fill = gantt_pal$murky_blue
    ) +
    # planned work
    geom_segment(
      aes(
        x = start,
        xend = end + 1,
        y = task,
        yend = task,
        colour = status
      ),
      alpha = 0.3,
      linewidth = 6
    ) +
    # due dates
    geom_segment(
      aes(
        x = due,
        xend = due + 1,
        y = task,
        yend = task,
        colour = status
      ),
      alpha = 0.3,
      size = 12
    ) +
    # planned work
    geom_text(
      aes(x = start,
          label = task,
          y = task),
      size = font_size * 0.3,
      alpha = 0.5,
      family = "serif",
      # nudge_y = 0.2,
      hjust = 0
    ) +
    scale_color_manual(values = status_levels$colour) +
    theme(
      axis.text.y = element_blank(),
      axis.text.x = element_text(angle = 30),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position = "bottom",
      strip.text.y.left = element_text(angle = 0)
    ) +
    scale_x_date(breaks = scales::breaks_pretty(nrow(gantt_dat))) +
    facet_grid(project ~ .,
               scales = "free_y",
               switch = "y") +
    labs(x = "", y = "") +
    xlim(gantt_min, gantt_max)

}

#' Overdue
#'
#' @export

pandan_due <- function() {
  df <- pandan::wrangle_gantt_dat()

  df %>%
    dplyr::filter(stringr::str_detect(status, "due")) %>%
    gt::gt(groupname_col = "fokus")
}
