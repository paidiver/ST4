synthetic_files <- function() {
  c(counts = system.file("extdata", "synthetic_counts.csv", package = "ST4"),
    metadata = system.file("extdata", "synthetic_metadata.csv", package = "ST4"))
}

test_that("the shipped files can be rebuilt exactly", {
  dir <- withr::local_tempdir()
  write_synthetic_data(make_synthetic_data(), dir)
  files <- synthetic_files()

  expect_identical(readLines(file.path(dir, "synthetic_counts.csv")),
                   readLines(files[["counts"]]))
  expect_identical(readLines(file.path(dir, "synthetic_metadata.csv")),
                   readLines(files[["metadata"]]))
})

test_that("the files load with read_counts()", {
  files <- synthetic_files()
  x <- read_counts(files[["counts"]], files[["metadata"]])

  expect_s3_class(x, "su_data")
  expect_equal(dim(x$comm), c(600, 40))
  expect_true(all(colSums(x$comm) > 0))
})

test_that("the files have the same layout as the real files", {
  files <- synthetic_files()
  counts <- utils::read.csv(files[["counts"]], check.names = FALSE)
  metadata <- utils::read.csv(files[["metadata"]], check.names = FALSE)

  # Blank-named first column, as written by utils::write.csv()
  expect_equal(names(counts)[1], "")
  expect_equal(names(metadata),
               c("", "FileName", "Substratum", "Habitat", "Latitude",
                 "Longitude", "Area_seabed_m2"))
  # Rows are not in the same order in the two files
  expect_false(identical(counts[[1]], metadata$FileName))
})

test_that("hard substrata have more individuals per tile than sand", {
  files <- synthetic_files()
  x <- read_counts(files[["counts"]], files[["metadata"]])
  per_tile <- function(s) mean(rowSums(select_stratum(x, s)$comm))
  expect_gt(per_tile("hard"), 5 * per_tile("sand"))
})

test_that("making the data does not change the user's random numbers", {
  set.seed(1)
  expected <- stats::runif(1)
  set.seed(1)
  make_synthetic_data()
  expect_equal(stats::runif(1), expected)
})
