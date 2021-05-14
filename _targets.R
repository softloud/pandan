library(targets)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
summ <- function(dataset) {
  summarize(dataset, mean_x = mean(x))
}

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse"))

conflicted::conflict_prefer("filter", "dplyr")

# End this file with a list of target objects.
list(
  tar_target(set_n,
             4),

  tar_target(
    set_obs,
    tibble(
      project = c(rep("a", set_n - 1), "b"),
      component = c("1", "1.1", "1.2", "1"),
      pred = c(rep(NA, set_n - 1), "a_1"),
      time = sample(1:3, size = set_n, replace = TRUE)
    ) %>%
      mutate(component = str_c(project, component, sep = "_"))
  )

  ,


  tar_target(
    wide_trans,
    set_obs %>%
      mutate(pred_com = map2(
        component,
        pred,
        .f = function(c, p) {
          set_obs %>%
            filter(str_detect(component, c)
                   # ,
                   # component != c
                   ) %>%
            pull(component) %>%
            c(p) %>%
            discard(~ is.na(.x))
        }
      )
  )),

  tar_target(
    max_length,
    wide_trans %>%
      unnest(pred_com) %>%
      arrange(desc(pred_com), desc(time)) %>%
      group_by(project, pred_com) %>%
      mutate(
        # define end point
        # this is the trickiest bit

        # set default to time
        end = time,

        # add time for predecessor component
        pred_time = map_int(pred_com, .f = function(pc){
          wide_trans %>%
            filter(component == pc) %>%
            pull(time)
        }),

        # update end
        end = if_else(
          component == pred_com,
          end,
          end + pred_time
        )
          ) %>%
      group_by(component) %>%
      filter(
        end == max(end)
      ) %>%
      mutate(
        start = end - time
      )

  ),

  tar_target(
    gantt_plot,
    max_length %>%
    ggplot() +
      geom_segment(
        aes(
          x = start,
          xend = end,
          y = component,
          yend = component
        ),
        size = 6,
        alpha = 0.3
      ) +
      facet_grid(project ~ ., scales = "free_y") +
      ggthemes::theme_wsj()
  ),

  NULL

)
