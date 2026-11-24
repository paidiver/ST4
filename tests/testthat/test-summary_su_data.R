# Small made-up data where the answers can be worked out by hand.
#
#   image  a  b  c  d  e   total  area (m²)  Substratum
#   t1     3  1  0  0  0   4      2          sand
#   t2     0  0  0  0  0   0      5          sand
#   t3     1  1  1  1  0   4      8          sand
#   t4     5  0  0  0  0   5      10         sand
#   t5     0  0  0  0  4   4      4          hard
summary_data <- function() {
  counts <- data.frame(
    image = c("t1", "t2", "t3", "t4", "t5"),
    a = c(3, 0, 1, 5, 0),
    b = c(1, 0, 1, 0, 0),
    c = c(0, 0, 1, 0, 0),
    d = c(0, 0, 1, 0, 0),
    e = c(0, 0, 0, 0, 4)
  )
  metadata <- data.frame(
    FileName = c("t1", "t2", "t3", "t4", "t5"),
    Area_seabed_m2 = c(2, 5, 8, 10, 4),
    Substratum = c("sand", "sand", "sand", "sand", "hard")
  )
  select_stratum(read_counts(counts, metadata), "sand")
}

test_that("summary of images gives the data check values", {
  s <- summary(summary_data())
  expect_s3_class(s, "summary.su_data")
  expect_equal(s$kind, "su_images")
  expect_equal(s$n_units, 4)
  # t2 has no individuals
  expect_equal(s$empty, 1)
  # Individuals per image: 4, 0, 4, 5. Mean 13 / 4 = 3.25, range 0 to 5
  expect_equal(s$individuals, c(mean = 3.25, min = 0, max = 5))
  # Area per image: 2, 5, 8, 10. Mean 25 / 4 = 6.25, range 2 to 10
  expect_equal(s$area, c(mean = 6.25, min = 2, max = 10))
  # Taxa totals in sand: a 9, b 2, c 1, d 1, e 0
  # 4 seen of 5. Singletons c and d. Doubleton b
  expect_equal(s$n_taxa, 5)
  expect_equal(s$taxa_seen, 4)
  expect_equal(s$singletons, 2)
  expect_equal(s$doubletons, 1)
  # Most common: a with 9 of 13 individuals = 69.2%
  expect_equal(s$common$taxon[1], "a")
  expect_equal(s$common$individuals[1], 9)
  expect_equal(s$common$share[1], 100 * 9 / 13)
  expect_null(s$images_per_su)
})

test_that("top sets how many taxa are listed", {
  s <- summary(summary_data(), top = 2)
  # a 9, then b 2
  expect_equal(s$common$taxon, c("a", "b"))
  expect_error(summary(summary_data(), top = 0), "`top`")
})

test_that("summary of images prints a short report", {
  out <- capture.output(print(summary(summary_data())))
  expect_match(out[1], "Summary of the data")
  expect_match(out, "Data: 4 images, stratum Substratum = sand", all = FALSE)
  # 1 of 4 empty = 25%
  expect_match(out, "No individuals: 1 \\(25%\\)", all = FALSE)
  expect_match(out, "Individuals per image: mean 3.25, 0 to 5", all = FALSE)
  expect_match(out, "Seabed area per image: mean 6.25 m², 2 to 10 m²",
               all = FALSE)
  expect_match(out, "Taxa: 4 seen, of 5 in the data", all = FALSE)
  expect_match(out, "69.2%", all = FALSE)
  expect_match(out, "as vegan gives them", all = FALSE)
})

test_that("summary of composite SUs adds images per SU", {
  # SUs of 2 images each: 4 images make 2 SUs, none dropped
  sus <- build_sample_units(summary_data(), type = "images", size = 2,
                            seed = 1)
  s <- summary(sus)
  expect_equal(s$kind, "su_composite")
  expect_equal(s$images_per_su, c(mean = 2, min = 2, max = 2))
  expect_equal(s$images_used, 4)
  expect_equal(s$n_dropped, 0)
  # Pooled SUs hold the same individuals: 13 in total
  expect_equal(s$individuals[["mean"]], 13 / 2)
  out <- capture.output(print(s))
  expect_match(out, "Units: 2 composite SUs", all = FALSE)
  # Same no. of images in every SU, so just the value
  expect_match(out, "Images per SU: 2$", all = FALSE)
  expect_match(out, "Images used: 4, 0 left over and dropped", all = FALSE)
  expect_match(out, "Individuals per SU", all = FALSE)
})

test_that("no empty-unit note when every unit has individuals", {
  # Every SU pools at least 4 individuals, so none is empty
  sus <- build_sample_units(summary_data(), type = "individuals", size = 4,
                            seed = 1)
  out <- capture.output(print(summary(sus)))
  expect_false(any(grepl("as vegan gives them", out)))
})
