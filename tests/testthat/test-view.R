test_that("neet", {
  expect_s3_class(pandan_view(update = FALSE), "ggplot")
})

test_that("distill option", {
  expect_s3_class(pandan_view(distill = TRUE, update = FALSE), "ggplot")
})

test_that("single project view", {
  expect_s3_class(pandan_view("hpp", update = FALSE), "ggplot")
})
