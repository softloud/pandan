test_that("neet", {
  expect_s3_class(pandan_next("waystation"), "gt_tbl")
})
