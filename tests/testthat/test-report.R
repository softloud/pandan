test_that("neet", {
  report <- pandan_report("waystation")

  expect_type(report, "list")
  expect_equal(nrow(report), 1)
})
