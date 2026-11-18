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

  # The sand tiles include some with no individuals, so the only warnings
  # expected are the ones saying so (from the metrics and the curves)
  warnings <- character(0)
  expect_no_error(utils::capture.output(withCallingHandlers(
    for (e in exprs[!is_library]) eval(e, env),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )))
  expect_true(all(grepl("no individuals", warnings)))
  expect_s3_class(env$survey, "su_data")
  expect_equal(env$one_stratum$settings$stratum, "sand")
  expect_s3_class(env$needed, "sample_size_table")
  expect_equal(names(env$tile_metrics),
               c("FileName", "richness", "density", "shannon"))
  expect_equal(env$sus$settings$unit, "SUs")
  expect_s3_class(env$curves, "su_curve")
  # The estimates table tracks the guess, single tiles and SUs
  expect_equal(env$estimates$stage, c("Guess", "Single tiles", "Composite SUs"))
  expect_equal(env$estimates$cv[1], env$guessed_cv)
  expect_equal(env$n_needed, env$estimates$n_needed[3])
})
