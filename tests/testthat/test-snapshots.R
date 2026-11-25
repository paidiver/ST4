# Fixed-seed snapshots of the main results on the synthetic data. The first
# run saves the output to _snaps/snapshots.md. Later runs fail if the output
# changes, which catches silent changes to the numbers. If a change is
# intended, check the new output and accept it with
# testthat::snapshot_accept("snapshots").

metrics <- c("richness", "density", "shannon")

test_that("data and stratum printouts and summaries", {
  sand <- synthetic_sand()
  expect_snapshot({
    synthetic_survey()
    sand
    summary(sand)
  })
})

test_that("metrics for single images", {
  sand <- synthetic_sand()
  # The empty sand images give warnings, kept in the snapshot
  expect_snapshot({
    m <- su_metrics(sand, metrics)
    head(m)
    summary(m)
  })
})

test_that("number of SUs needed", {
  sand <- synthetic_sand()
  expect_snapshot({
    sample_size(cv = 25, delta = 40)
    sample_size_table()
    sample_size_data(sand, delta = 40, metrics = metrics)
  })
})

test_that("composite SUs with a fixed seed", {
  sus <- build_sample_units(synthetic_sand(), seed = 1)
  expect_snapshot({
    sus
    summary(sus)
    sus$meta
    head(sus$members)
    summary(su_metrics(sus, metrics))
    sample_size_data(sus, delta = 40, metrics = metrics)
  })
})

test_that("SU size curves with a fixed seed", {
  sand <- synthetic_sand()
  expect_snapshot({
    su_curve(sand, metrics, times = 100, steps = c(1, 5, 10, 20, 50, 100),
             seed = 1)
  })
})
