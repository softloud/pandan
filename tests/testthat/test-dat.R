test_that("neet", {
  dat <- pandan_dat("waystation")

  expect_type(dat, "list")

  expect_gt(length(dat), 0)
})
