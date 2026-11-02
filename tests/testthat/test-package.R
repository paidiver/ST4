test_that("vegan is available", {
  expect_true(requireNamespace("vegan", quietly = TRUE))
})
