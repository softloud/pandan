#' See progress over time
#'
#' @param project Choose from one of the projects in [pandan_view].
#' @param distill Plot theme for matching distill blog.
#' @param update Run [project_report] with update for all `status == "active"`
#' projects, unless project is not "all".
#'
#' @export

pandan_view <-
  function(project = "all",
           distill = FALSE,
           update = TRUE) {
    projects <- googlesheets4::read_sheet(Sys.getenv("PANDAN_PROJECTS"))

    # update
    if (isTRUE(update)) {
      if (project != "all") {
        pandan_report(!!project, update = TRUE)
      } else {
        projects %>%
          dplyr::filter(status == "active") %>%
          dplyr::pull(project) %>%
          purrr::map(
            .f = function(x) {
              pandan_report(x, update = TRUE)
            }
          )
      }
    }

    progress <-
      googlesheets4::read_sheet(Sys.getenv("PANDAN_PROGRESS")) %>%
      # filter to last record on a given date
      dplyr::mutate(day = lubridate::date(date)) %>%
      dplyr::group_by(day) %>%
      dplyr::mutate(last_time = max(date)) %>%
      dplyr::filter(date == last_time) %>%
      dplyr::select(-last_time)



    dat <-   progress %>%
      dplyr::left_join(projects, by = "project")

    if (project != "all") {
      dat <- dat %>%
        dplyr::filter(project == !!project)
    }

    completed <- projects %>%
      dplyr::filter(status == "completed") %>%
      dplyr::pull(project) %>%
      paste0(collapse = ", ") %>% {
        glue::glue("well done! you've completed: {.}.")
      }

    plotdat <-
      dat %>%
      # create label for legend
      dplyr::mutate(
        project_name = project,
        project = stringr::str_c(project, description, sep = " | ") %>%
          stringr::str_wrap()
      ) %>%
      dplyr::group_by(project) %>%
      dplyr::mutate(total = components * (levels + edit_n),
                    total_current = dplyr::last(total)) %>%
      dplyr::filter(status == "active")

    project_plot <-
      plotdat %>%
      ggplot2::ggplot(
        ggplot2::aes(
          x = day,
          y = writing / total_current,
          group = project,
          colour = project,
          shape = category
        )
      )  +
      ggplot2::geom_hline(
        data = tibble::tibble(
          progress = c("sweet fuck all", "halfway!", "completed"),
          value = c(0, 0.5, 1)
        ),
        alpha = 0.2,
        ggplot2::aes(yintercept = value, linetype = progress)
      ) +
      ggplot2::geom_line(alpha = 0.3) +
      ggplot2::geom_point(size = 6, alpha = 0.6) +
      ggplot2::facet_grid(group ~ ., labeller = ggplot2::label_parsed) + #,
      # once I figure out how to switch the y axis to the right
      # switch="y") +
      # ggplot2::scale_y_continuous(position = "right") +
      rockthemes::scale_color_melloncollie() +
      ggplot2::ylim(-0.2, 1.2)


    themed_plot <-
      if (isTRUE(distill)) {
        project_plot + ggthemes::theme_fivethirtyeight()
      } else {
        project_plot + ggthemes::theme_solarized_2()
      }

    # final tweaks
    themed_plot +
      ggplot2::labs(
        title = glue::glue("pandan progress to {dontpanic::title_date(Sys.Date())}"),
        subtitle = completed
      ) +
      ggplot2::theme(
        strip.text.y = ggplot2::element_text(angle = 0),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        legend.position = "bottom",
        # c(0, 1),
        legend.direction = "vertical",
        legend.title = ggplot2::element_text("project")
      )
  }
