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
tar_option_set(packages =
                 c("tidyverse",
                   "glue",
                   "lubridate"))

# End this file with a list of target objects.
list(
  tar_target(
    raw_obs,
    tibble(component = c("1", "1.1", "1.2", "1.2.1", "1"),) %>%
      mutate(
        project = c(rep("a", length(component) - 1), "b"),

        pred = case_when(
          component == "1.1" & project == "a" ~ "a_1.2.1",
          component == "1" &
            project == "b" ~ "a_1.1"
        ),
        time = sample(1:3, size = length(component), replace = TRUE)
      ) %>%
      select(project, everything())
  ),

  tar_target(raw_proj,
             tibble(
               project = c("a", "b"),
               start_inst = c("2021-05-05", "2021-05-06")
             ) %>%
             mutate(
               start_inst = ymd(start_inst)
             )
             ),

  tar_target(
    obs,
    raw_obs %>%
      mutate(
        component = str_c(project, component, sep = "_"),
        pred_c = map_chr(
          component,
          .f = function(c) {
            str_match(c, "(.*)\\.\\d*") %>% pluck(2)
          }
        ),
        pred =
          case_when(
            !is.na(pred_c) & !is.na(pred) ~ glue("{pred_c};{pred}"),
            is.na(pred_c) & !is.na(pred) ~ glue("{pred}"),
            !is.na(pred_c) & is.na(pred) ~ glue("{pred_c}")
          ) %>% str_split(pattern = ";")
      ) %>%
      select(project, component, time, everything(),-pred_c)

  ),

  tar_target(obs_proj,
             obs %>%
               left_join(raw_proj, by = "project") %>%
               mutate(
                 end_inst = start_inst + time,
                 pred_na = is.na(pred)
               )
               ),

  tar_target(
    find_max,
    obs_proj %>%
      filter(component != "a_1") %>%
      mutate(
        start = map(pred, .f = function(proj, end_points){
          tibble(
            component = pred
          )
          # %>%
          #   left_join(obs_proj) %>%
          #   arrange(desc(end_inst)) %>%
          #     select(component, end_inst)
        })
      )
  ),

  NULL

)
