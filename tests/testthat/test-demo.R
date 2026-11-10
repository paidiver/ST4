test_that("the demo script runs from start to finish", {
  script <- system.file("scripts", "demo.R", package = "ST4")
  expect_true(file.exists(script))

  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())

  # Skip library(ST4): during tests the package is already loaded, and may
  # not be installed
  exprs <- parse(script)
  is_library <- vapply(exprs, function(e) {
    is.call(e) && identical(e[[1]], as.name("library"))
  }, logical(1))
  env <- new.env(parent = environment())

  expect_no_error(utils::capture.output(
    for (e in exprs[!is_library]) eval(e, env)
  ))
  expect_s3_class(env$survey, "su_data")
  expect_equal(env$one_stratum$settings$stratum, "sand")
  expect_s3_class(env$needed, "sample_size_table")
  expect_equal(names(env$tile_metrics),
               c("FileName", "richness", "density", "shannon"))
})
