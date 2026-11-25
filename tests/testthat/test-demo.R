# Runs a demo script from inst/scripts and returns its objects and warnings
run_demo <- function(name) {
  script <- system.file("scripts", name, package = "ST4")
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

  warnings <- character(0)
  expect_no_error(utils::capture.output(withCallingHandlers(
    for (e in exprs[!is_library]) eval(e, env),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )))
  env$warnings <- warnings
  env
}

test_that("the demo script runs from start to finish", {
  env <- run_demo("demo.R")
  # The sand images include some with no individuals, so the only warnings
  # expected are the ones saying so (from the metrics and the curves)
  expect_true(all(grepl("no individuals", env$warnings)))
  expect_s3_class(env$survey, "su_images")
  expect_equal(env$one_stratum$settings$stratum, "sand")
  expect_s3_class(env$needed, "sample_size_table")
  expect_equal(names(env$image_metrics),
               c("FileName", "richness", "density", "shannon"))
  expect_s3_class(env$sus, "su_composite")
  expect_s3_class(env$curves, "su_curve")
  # The estimates table tracks the guess, single images and SUs
  expect_equal(env$estimates$stage, c("Guess", "Single images", "Composite SUs"))
  expect_equal(env$estimates$cv[1], env$guessed_cv)
  expect_equal(env$n_needed, env$estimates$n_needed[3])
})

test_that("the BODC demo script runs from start to finish", {
  env <- run_demo("demo_bodc.R")
  expect_true(all(grepl("no individuals", env$warnings)))
  expect_equal(env$one_stratum$settings$stratum, "KM1808")
  expect_s3_class(env$sus, "su_composite")
  expect_equal(env$sus$settings$sample_units$type, "images")
  expect_equal(env$estimates$stage, c("Guess", "Single images", "Composite SUs"))
  expect_equal(env$n_needed, env$estimates$n_needed[3])
  expect_match(env$survey$settings$data_citation, "doi:10.5285/5f9b8ce1")
})
