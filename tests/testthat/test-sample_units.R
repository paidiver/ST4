# Small made-up data where the right answer is known: 6 sand images of 5 m²
# and 1 hard image
units_data <- function() {
  counts <- data.frame(
    image = paste0("t", 1:7),
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

test_that("group_images adds images until the target is reached", {
  # Target 10. Running totals: 4, 4 + 7 = 11 >= 10 closes SU 1.
  # Then 3, 3 + 3 = 6, 6 + 5 = 11 >= 10 closes SU 2.
  # Then 2 never reaches 10, so that image is dropped.
  expect_equal(group_images(c(4, 7, 3, 3, 5, 2), 10),
               c(1L, 1L, 2L, 2L, 2L, NA))
  # An image exactly at the target closes the SU on its own
  expect_equal(group_images(c(10, 10), 10), c(1L, 2L))
  # Images with nothing in them after the last full SU are dropped too
  expect_equal(group_images(c(10, 0, 0), 10), c(1L, NA, NA))
  expect_equal(group_images(c(1, 2), 10), c(NA_integer_, NA_integer_))
})

test_that("groups a set number of images and drops the leftover", {
  # 6 images in SUs of 4 images: 1 SU, 2 images dropped
  y <- build_sample_units(units_data(), type = "images", size = 4, seed = 1)
  expect_s3_class(y, c("su_composite", "su_data"), exact = TRUE)
  expect_equal(nrow(y$comm), 1)
  expect_equal(y$meta$n_images, 4)
  expect_equal(y$settings$n_dropped, 2)
  # 4 images of 5 m² each: 4 * 5 = 20 m²
  expect_equal(y$meta$Area_seabed_m2, 20)
  expect_equal(y$settings$unit, "composite SUs")
})

test_that("groups by area", {
  # Images of 5 m² with a target of 12 m²: 5, 10, 15 >= 12, so 3 images and
  # 15 m² per SU. 6 images make 2 SUs with none left over.
  y <- build_sample_units(units_data(), type = "area", size = 12, seed = 1)
  expect_equal(y$meta$n_images, c(3, 3))
  expect_equal(y$meta$Area_seabed_m2, c(15, 15))
  expect_equal(y$settings$n_dropped, 0)
})

test_that("groups by individuals, each SU at least the target", {
  y <- build_sample_units(units_data(), type = "individuals", size = 4,
                          seed = 1)
  expect_true(all(rowSums(y$comm) >= 4))
})

test_that("SU counts are the sums of their images", {
  x <- units_data()
  y <- build_sample_units(x, type = "images", size = 2, seed = 3)
  for (su in rownames(y$comm)) {
    images <- y$members$FileName[y$members$SU == su]
    expect_equal(y$comm[su, ], colSums(x$comm[images, , drop = FALSE]))
  }
  # Sand individuals: asterias 2 + 0 + 1 + 4 + 0 + 3 = 10 and cerianthid
  # 0 + 3 + 1 + 0 + 0 + 2 = 6, total 16. SUs of 2 images use all 6 images.
  expect_equal(sum(y$comm), 16)
  # Each image is in at most one SU
  expect_false(anyDuplicated(y$members$FileName) > 0)
  expect_setequal(y$members$FileName, rownames(x$comm))
})

test_that("the same seed gives the same SUs and leaves the session alone", {
  x <- units_data()
  set.seed(42)
  before <- .Random.seed
  a <- build_sample_units(x, type = "images", size = 2, seed = 7)
  expect_equal(.Random.seed, before)
  b <- build_sample_units(x, type = "images", size = 2, seed = 7)
  expect_equal(a$members, b$members)
})

test_that("without a seed it uses the session's random numbers", {
  x <- units_data()
  set.seed(5)
  a <- build_sample_units(x, type = "images", size = 2)
  set.seed(5)
  b <- build_sample_units(x, type = "images", size = 2)
  expect_equal(a$members, b$members)
})

test_that("later steps work on SUs", {
  y <- build_sample_units(units_data(), type = "images", size = 2, seed = 1)
  m <- su_metrics(y, "abundance")
  expect_equal(names(m), c("SU", "abundance"))
  expect_equal(m$SU, rownames(y$comm))
  expect_output(print(metric_cv(y, "abundance")), "3 composite SUs")
  expect_output(print(y), "^su_composite")
  expect_output(print(y), "Composite SUs: 3")
  expect_output(print(y), "Built by: images, at least 2 images each")
  expect_output(print(y), "Images used: 6, 0 left over")
  # 6 images in SUs of 2: every SU has 2
  expect_output(print(y), "Images per SU: 2\n")
})

test_that("keeps the stratum and data citation", {
  y <- build_sample_units(units_data(), type = "images", size = 2, seed = 1)
  expect_equal(y$settings$stratum, "sand")
  expect_equal(y$settings$by, "Substratum")
  expect_equal(y$settings$data_citation, "No Data Citation")
})

test_that("warns when no stratum is selected", {
  counts <- data.frame(image = c("a", "b"), sp = c(1, 2))
  meta <- data.frame(FileName = c("a", "b"), Area_seabed_m2 = c(5, 5))
  x <- read_counts(counts, meta)
  expect_warning(build_sample_units(x, type = "images", size = 1, seed = 1),
                 "No stratum selected")
})

test_that("stops on bad input", {
  x <- units_data()
  expect_error(build_sample_units(x$comm), "su_data")
  y <- build_sample_units(x, type = "images", size = 2, seed = 1)
  expect_error(build_sample_units(y),
               "must hold single images.*holds composite SUs \\(su_composite\\)")
  # 6 images of 5 m² is 30 m², less than one 1000 m² SU
  expect_error(build_sample_units(x, size = 1000), "Not enough images")
  expect_error(build_sample_units(x, tpye = "images"), "Did you mean `type`")
})

test_that("control has defaults for each type", {
  expect_equal(sample_unit_control()$type, "area")
  expect_equal(sample_unit_control()$size, 150)
  expect_equal(sample_unit_control(type = "individuals")$size, 150)
  expect_equal(sample_unit_control(type = "images")$size, 20)
  expect_error(sample_unit_control(type = "volume"), "should be one of")
  expect_error(sample_unit_control(type = "images", size = 2.5), "whole number")
  expect_error(sample_unit_control(size = -1), "positive")
  expect_output(print(sample_unit_control(size = 200)), "\\(changed\\)")
})

test_that("composite SUs keep the seed and show a range of images per SU", {
  y <- build_sample_units(units_data(), type = "images", size = 2, seed = 5)
  expect_equal(y$settings$seed, 5)
  # SUs of at least 6 individuals, seed 2 order: t5, t1, t6, t4, t2, t3
  # SU_1: t5 + t1 + t6 = 0 + 2 + 5 = 7 individuals, 3 images
  # SU_2: t4 + t2 = 4 + 3 = 7 individuals, 2 images
  # t3 (2 individuals) is left over
  z <- build_sample_units(units_data(), type = "individuals", size = 6,
                          seed = 2)
  expect_equal(z$meta$n_images, c(3, 2))
  expect_output(print(z), "Images per SU: 2 to 3")
  expect_output(print(z), "Images used: 5, 1 left over")
  # n_images is not listed as other metadata
  expect_no_match(capture.output(print(z)), "Other metadata")
})
