test_that("assess_output_structure", {
  expected <- mortality_model("SA_mortality.csv")
  expect_s3_class(expected, "data.frame")
})
