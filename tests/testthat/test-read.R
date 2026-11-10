# Small made-up tables where the right answer is known

counts_df <- function() {
  data.frame(
    tile = c("t1", "t2", "t3"),
    asterias = c(2, 0, 1),
    cerianthid = c(0, 3, 1)
  )
}

metadata_df <- function() {
  data.frame(
    FileName = c("t3", "t1", "t2"),
    Area_seabed_m2 = c(7.1, 7.4, 6.9),
    Substratum = c("sand", "hard", "sand")
  )
}

test_that("joins by tile id, not by row order", {
  x <- read_counts(counts_df(), metadata_df())

  expect_s3_class(x, "su_data")
  expect_equal(rownames(x$comm), c("t1", "t2", "t3"))
  expect_equal(x$meta$FileName, c("t1", "t2", "t3"))
  # metadata lists t3, t1, t2, so after the join the areas move to
  # t1 = 7.4, t2 = 6.9, t3 = 7.1
  expect_equal(x$meta$Area_seabed_m2, c(7.4, 6.9, 7.1))
  expect_equal(x$meta$Substratum, c("hard", "sand", "sand"))
  expect_equal(unname(x$comm[, "cerianthid"]), c(0, 3, 1))
})

test_that("keeps the totals", {
  x <- read_counts(counts_df(), metadata_df())
  # Counts: asterias 2 + 0 + 1 = 3, cerianthid 0 + 3 + 1 = 4, total 7
  expect_equal(sum(x$comm), 7)
  # Area: 7.1 + 7.4 + 6.9 = 21.4
  expect_equal(sum(x$meta$Area_seabed_m2), 21.4)
  expect_equal(dim(x$comm), c(3, 2))
})

test_that("reads CSV files and drops blank-named columns", {
  counts_file <- withr::local_tempfile(fileext = ".csv")
  meta_file <- withr::local_tempfile(fileext = ".csv")
  # write.csv adds a blank-named row number column, like Jen's files
  c_df <- counts_df()
  rownames(c_df) <- c_df$tile
  utils::write.csv(c_df[-1], counts_file)
  utils::write.csv(metadata_df(), meta_file)

  x <- read_counts(counts_file, meta_file)
  expect_equal(rownames(x$comm), c("t1", "t2", "t3"))
  expect_equal(names(x$meta), c("FileName", "Area_seabed_m2", "Substratum"))
})

test_that("uses other column names when given", {
  m <- metadata_df()
  names(m)[1:2] <- c("tile_id", "area")
  x <- read_counts(counts_df(), m, id = "tile_id", area = "area")
  expect_equal(x$settings$id, "tile_id")
  expect_equal(x$settings$area, "area")
})

test_that("stops when tile ids do not match", {
  m <- metadata_df()
  m$FileName[1] <- "t9"
  expect_error(read_counts(counts_df(), m), "do not match")
  expect_error(read_counts(counts_df(), m), "t3")
  expect_error(read_counts(counts_df(), m), "t9")
})

test_that("stops on repeated tile ids", {
  m <- rbind(metadata_df(), metadata_df()[1, ])
  expect_error(read_counts(counts_df(), m), "repeated in `metadata`")
})

test_that("stops on bad counts", {
  bad <- counts_df()
  bad$asterias[1] <- -1
  expect_error(read_counts(bad, metadata_df()), "whole numbers")
  bad$asterias[1] <- 1.5
  expect_error(read_counts(bad, metadata_df()), "whole numbers")
  bad$asterias[1] <- NA
  expect_error(read_counts(bad, metadata_df()), "whole numbers")
  expect_error(read_counts(counts_df()[1], metadata_df()), "taxon column")
})

test_that("stops on bad metadata", {
  m <- metadata_df()
  m$Area_seabed_m2[2] <- 0
  expect_error(read_counts(counts_df(), m), "Area_seabed_m2")
  expect_error(read_counts(counts_df(), metadata_df(), area = "nope"),
               "`nope` not found")
  expect_error(read_counts(counts_df(), "no_such_file.csv"), "not found")
  expect_error(read_counts(counts_df(), 42), "file path or a data frame")
})

test_that("print gives a summary", {
  x <- read_counts(counts_df(), metadata_df())
  expect_output(print(x), "Stratum: all \\(none selected\\)")
  expect_output(print(x), "Units: 3 tiles")
  expect_output(print(x), "Individuals: 7")
  expect_output(print(x), "Other metadata: Substratum")
})

test_that("has a data citation, by default 'No Data Citation'", {
  x <- read_counts(counts_df(), metadata_df())
  expect_equal(x$settings$data_citation, "No Data Citation")
  expect_output(print(x), "Data citation: No Data Citation")

  y <- read_counts(counts_df(), metadata_df(),
                   data_citation = "  Made-up example tiles  ")
  # Spaces at either end are trimmed
  expect_equal(y$settings$data_citation, "Made-up example tiles")
})

test_that("the data citation cannot be removed or left empty", {
  for (bad in list("", "   ", NA_character_, NULL, character(0),
                   c("a", "b"), 42)) {
    expect_error(read_counts(counts_df(), metadata_df(), data_citation = bad),
                 "data_citation")
  }
})

test_that("the data citation is carried through later steps", {
  x <- read_counts(counts_df(), metadata_df(), data_citation = "Survey X")
  y <- select_stratum(x, "sand")
  expect_equal(y$settings$data_citation, "Survey X")
  expect_output(print(metric_cv(y)), "Data citation: Survey X")
  expect_output(print(sample_size_data(y, delta = 50)),
                "Data citation: Survey X")
})
