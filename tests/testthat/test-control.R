test_that("has the defaults", {
  ctl <- sample_size_control()
  expect_s3_class(ctl, "sample_size_control")
  expect_equal(unclass(ctl)[c("alpha", "power", "n_strata", "max_n")],
               list(alpha = 0.05, power = 0.9, n_strata = 2, max_n = Inf))
  expect_length(attr(ctl, "changed"), 0)
})

test_that("records which settings were changed", {
  ctl <- sample_size_control(alpha = 0.1, max_n = 50)
  expect_equal(ctl$alpha, 0.1)
  expect_equal(ctl$power, 0.9)
  expect_setequal(attr(ctl, "changed"), c("alpha", "max_n"))
})

test_that("the three ways to change a setting give the same result", {
  by_helper <- sample_size(20, 40, control = sample_size_control(alpha = 0.1))
  by_list <- sample_size(20, 40, control = list(alpha = 0.1))
  by_arg <- sample_size(20, 40, alpha = 0.1)

  expect_equal(by_list, by_helper)
  expect_equal(by_arg, by_helper)
  expect_false(by_helper == sample_size(20, 40))
})

test_that("settings given straight to the function override control", {
  ctl <- sample_size_control(alpha = 0.1, power = 0.8)
  expect_equal(sample_size(20, 40, control = ctl, alpha = 0.2),
               sample_size(20, 40, alpha = 0.2, power = 0.8))
})

test_that("a misspelt setting stops and suggests the right name", {
  expect_error(sample_size_control(alpah = 0.1), "Did you mean `alpha`")
  expect_error(sample_size(20, 40, powr = 0.8), "Did you mean `power`")
  expect_error(sample_size(20, 40, control = list(nstrata = 3)),
               "Did you mean `n_strata`")
  expect_error(sample_size_control(zzzzzz = 1), "Unknown setting `zzzzzz`")
})

test_that("bad values stop when the settings are made", {
  expect_error(sample_size_control(alpha = 5), "alpha")
  expect_error(sample_size_control(power = 0), "power")
  expect_error(sample_size_control(n_strata = 1), "n_strata")
  expect_error(sample_size_control(max_n = 2.5), "max_n")
  expect_error(sample_size(20, 40, control = "fast"), "control")
  expect_error(sample_size(20, 40, 0.1), "control")
  expect_error(sample_size(20, 40, list(), 0.1), "named")
})

test_that("print marks the changed settings", {
  out <- utils::capture.output(print(sample_size_control(alpha = 0.1)))
  expect_match(out[grepl("alpha", out)], "0.1.*\\(changed\\)")
  expect_false(any(grepl("power.*changed", out)))
})
