# Small made-up data where the right answer is known: 6 sand tiles of 5 m²
# and 1 hard tile
units_data <- function() {
  counts <- data.frame(
    tile = paste0("t", 1:7),
    asterias = c(2, 0, 1, 4, 0, 3, 9),
    cerianthid = c(0, 3, 1, 0, 0, 2, 9)
  )
  metadata <- data.frame(
    FileName = paste0("t", 1:7),
    Area_seabed_m2 = c(5, 5, 5, 5, 5, 5, 5),
    Substratum = c(rep("sand", 6), "hard")
  )
  select_stratum(read_counts(counts, metadata), "sand")
}

test_that("group_tiles adds tiles until the target is reached", {
  # Target 10. Running totals: 4, 4 + 7 = 11 >= 10 closes SU 1.
  # Then 3, 3 + 3 = 6, 6 + 5 = 11 >= 10 closes SU 2.
  # Then 2 never reaches 10, so that tile is dropped.
  expect_equal(group_tiles(c(4, 7, 3, 3, 5, 2), 10),
               c(1L, 1L, 2L, 2L, 2L, NA))
  # A tile exactly at the target closes the SU on its own
  expect_equal(group_tiles(c(10, 10), 10), c(1L, 2L))
  # Tiles with nothing in them after the last full SU are dropped too
  expect_equal(group_tiles(c(10, 0, 0), 10), c(1L, NA, NA))
  expect_equal(group_tiles(c(1, 2), 10), c(NA_integer_, NA_integer_))
})

test_that("groups a set number of tiles and drops the leftover", {
  # 6 tiles in SUs of 4 tiles: 1 SU, 2 tiles dropped
  y <- build_sample_units(units_data(), type = "tiles", size = 4, seed = 1)
  expect_s3_class(y, "su_data")
  expect_equal(nrow(y$comm), 1)
  expect_equal(y$meta$n_tiles, 4)
  expect_equal(y$settings$n_dropped, 2)
  # 4 tiles of 5 m² each: 4 * 5 = 20 m²
  expect_equal(y$meta$Area_seabed_m2, 20)
  expect_equal(y$settings$unit, "SUs")
})

test_that("groups by area", {
  # Tiles of 5 m² with a target of 12 m²: 5, 10, 15 >= 12, so 3 tiles and
  # 15 m² per SU. 6 tiles make 2 SUs with none left over.
  y <- build_sample_units(units_data(), type = "area", size = 12, seed = 1)
  expect_equal(y$meta$n_tiles, c(3, 3))
  expect_equal(y$meta$Area_seabed_m2, c(15, 15))
  expect_equal(y$settings$n_dropped, 0)
})

test_that("groups by individuals, each SU at least the target", {
  y <- build_sample_units(units_data(), type = "individuals", size = 4,
                          seed = 1)
  expect_true(all(rowSums(y$comm) >= 4))
})

test_that("SU counts are the sums of their tiles", {
  x <- units_data()
  y <- build_sample_units(x, type = "tiles", size = 2, seed = 3)
  for (su in rownames(y$comm)) {
    tiles <- y$members$FileName[y$members$SU == su]
    expect_equal(y$comm[su, ], colSums(x$comm[tiles, , drop = FALSE]))
  }
  # Sand individuals: asterias 2 + 0 + 1 + 4 + 0 + 3 = 10 and cerianthid
  # 0 + 3 + 1 + 0 + 0 + 2 = 6, total 16. SUs of 2 tiles use all 6 tiles.
  expect_equal(sum(y$comm), 16)
  # Each tile is in at most one SU
  expect_false(anyDuplicated(y$members$FileName) > 0)
  expect_setequal(y$members$FileName, rownames(x$comm))
})

test_that("the same seed gives the same SUs and leaves the session alone", {
  x <- units_data()
  set.seed(42)
  before <- .Random.seed
  a <- build_sample_units(x, type = "tiles", size = 2, seed = 7)
  expect_equal(.Random.seed, before)
  b <- build_sample_units(x, type = "tiles", size = 2, seed = 7)
  expect_equal(a$members, b$members)
})

test_that("without a seed it uses the session's random numbers", {
  x <- units_data()
  set.seed(5)
  a <- build_sample_units(x, type = "tiles", size = 2)
  set.seed(5)
  b <- build_sample_units(x, type = "tiles", size = 2)
  expect_equal(a$members, b$members)
})

test_that("later steps work on SUs", {
  y <- build_sample_units(units_data(), type = "tiles", size = 2, seed = 1)
  m <- su_metrics(y, "abundance")
  expect_equal(names(m), c("SU", "abundance"))
  expect_equal(m$SU, rownames(y$comm))
  expect_output(print(metric_cv(y, "abundance")), "3 SUs")
  expect_output(print(y), "SUs built: by tiles, at least 2 tiles each")
  expect_output(print(y), "6 tiles used, 0 left over")
})

test_that("keeps the stratum and data citation", {
  y <- build_sample_units(units_data(), type = "tiles", size = 2, seed = 1)
  expect_equal(y$settings$stratum, "sand")
  expect_equal(y$settings$by, "Substratum")
  expect_equal(y$settings$data_citation, "No Data Citation")
})

test_that("warns when no stratum is selected", {
  counts <- data.frame(tile = c("a", "b"), sp = c(1, 2))
  meta <- data.frame(FileName = c("a", "b"), Area_seabed_m2 = c(5, 5))
  x <- read_counts(counts, meta)
  expect_warning(build_sample_units(x, type = "tiles", size = 1, seed = 1),
                 "No stratum selected")
})

test_that("stops on bad input", {
  x <- units_data()
  expect_error(build_sample_units(x$comm), "su_data")
  y <- build_sample_units(x, type = "tiles", size = 2, seed = 1)
  expect_error(build_sample_units(y), "already holds SUs")
  # 6 tiles of 5 m² is 30 m², less than one 1000 m² SU
  expect_error(build_sample_units(x, size = 1000), "Not enough tiles")
  expect_error(build_sample_units(x, tpye = "tiles"), "Did you mean `type`")
})

test_that("control has defaults for each type", {
  expect_equal(sample_unit_control()$type, "area")
  expect_equal(sample_unit_control()$size, 150)
  expect_equal(sample_unit_control(type = "individuals")$size, 150)
  expect_equal(sample_unit_control(type = "tiles")$size, 20)
  expect_error(sample_unit_control(type = "volume"), "should be one of")
  expect_error(sample_unit_control(type = "tiles", size = 2.5), "whole number")
  expect_error(sample_unit_control(size = -1), "positive")
  expect_output(print(sample_unit_control(size = 200)), "\\(changed\\)")
})
