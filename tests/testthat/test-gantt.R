library(googlesheets4)

gantt_df <-
  read_sheet("https://docs.google.com/spreadsheets/d/15joXuw0BBtU5tSbK7QIGWInhbYJ8XUh8bVW9HgCuhhE/edit#gid=0")


test_that("get a dataframe", {
  expect_equal("data.frame" %in% class(gantt_df), TRUE)
})

test_that("get a plot", {
  expect_s3_class(pandan_gantt(gantt_df), "ggplot")
})
