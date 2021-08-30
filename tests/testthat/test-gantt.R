library(googlesheets4)

test_that("get a dataframe", {
  expect_type(wrangle_gantt_dat(), "list")
})

test_that("get a plot", {
  expect_s3_class(pandan_gantt(gantt_df), "ggplot")
})
