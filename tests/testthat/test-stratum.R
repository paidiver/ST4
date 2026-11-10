# Small made-up data where the right answer is known
strata_data <- function() {
  counts <- data.frame(
    tile = c("t1", "t2", "t3", "t4"),
    asterias = c(2, 0, 1, 4),
    cerianthid = c(0, 3, 1, 0)
  )
  metadata <- data.frame(
    FileName = c("t1", "t2", "t3", "t4"),
    Area_seabed_m2 = c(7.4, 6.9, 7.1, 7.0),
    Substratum = c("hard", "sand", "sand", "hard + sand"),
    Habitat = c("hard", "sand", "sand", "intermediate")
  )
  read_counts(counts, metadata)
}

test_that("keeps only the tiles in the stratum", {
  y <- select_stratum(strata_data(), "sand")

  expect_s3_class(y, "su_data")
  expect_equal(rownames(y$comm), c("t2", "t3"))
  expect_equal(y$meta$FileName, c("t2", "t3"))
  expect_true(all(y$meta$Substratum == "sand"))
})

test_that("matches the stratum exactly, not partly", {
  # "hard" must not pick up "hard + sand"
  y <- select_stratum(strata_data(), "hard")
  expect_equal(rownames(y$comm), "t1")
})

test_that("keeps all taxa and the counts for kept tiles", {
  x <- strata_data()
  y <- select_stratum(x, "sand")
  expect_equal(colnames(y$comm), colnames(x$comm))
  expect_equal(y$comm, x$comm[c("t2", "t3"), ])
  # Sand tiles t2 and t3: asterias 0 + 1 = 1, cerianthid 3 + 1 = 4, total 5
  expect_equal(sum(y$comm), 5)
})

test_that("uses another column when given", {
  y <- select_stratum(strata_data(), "intermediate", by = "Habitat")
  expect_equal(rownames(y$comm), "t4")
  expect_equal(y$settings$by, "Habitat")
  expect_equal(y$settings$stratum, "intermediate")
})

test_that("strata split the data with nothing lost", {
  x <- strata_data()
  parts <- lapply(unique(x$meta$Substratum),
                  function(s) select_stratum(x, s))
  expect_equal(sum(vapply(parts, function(p) nrow(p$comm), numeric(1))),
               nrow(x$comm))
  expect_equal(sum(vapply(parts, function(p) sum(p$comm), numeric(1))),
               sum(x$comm))
})

test_that("print shows the stratum", {
  expect_output(print(select_stratum(strata_data(), "sand")),
                "Stratum: Substratum = sand")
})

test_that("stops on bad input", {
  x <- strata_data()
  expect_error(select_stratum(x, "mud"), "No tiles with Substratum = \"mud\"")
  expect_error(select_stratum(x, "mud"), "hard, hard \\+ sand, sand")
  expect_error(select_stratum(x, c("hard", "sand")), "single value")
  expect_error(select_stratum(x, "sand", by = "Depth"), "`Depth` not found")
  expect_error(select_stratum(data.frame(), "sand"), "su_data")
})
