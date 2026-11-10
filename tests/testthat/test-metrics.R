# Small made-up data where the answers can be worked out by hand.
#
#   tile  a  b  c  d   area (m²)
#   t1    3  1  0  0   2
#   t2    0  0  0  0   5
#   t3    1  1  1  1   8
#   t4    5  0  0  0   10
#
# p_i is the proportion of a tile's individuals in taxon i.
# Shannon H' = -sum(p_i * ln p_i). Simpson D = sum(p_i^2).
# Simpson index = 1 - D. Inverse Simpson = 1 / D. Pielou J' = H' / ln(richness).
metrics_data <- function() {
  counts <- data.frame(
    tile = c("t1", "t2", "t3", "t4"),
    a = c(3, 0, 1, 5),
    b = c(1, 0, 1, 0),
    c = c(0, 0, 1, 0),
    d = c(0, 0, 1, 0)
  )
  metadata <- data.frame(
    FileName = c("t1", "t2", "t3", "t4"),
    Area_seabed_m2 = c(2, 5, 8, 10)
  )
  read_counts(counts, metadata)
}

test_that("richness is the default", {
  out <- su_metrics(metrics_data())
  expect_equal(names(out), c("FileName", "richness"))
  expect_equal(out$FileName, c("t1", "t2", "t3", "t4"))
  # Taxa with a count above 0: t1 has a and b, t2 none, t3 all four, t4 only a
  expect_equal(out$richness, c(2, 0, 4, 1))
})

test_that("gives the right answers on known data", {
  out <- su_metrics(metrics_data(), c("abundance", "density", "shannon",
                                      "simpson", "invsimpson", "pielou"))
  # Abundance is the row total: t1 3 + 1 = 4, t2 0, t3 1 + 1 + 1 + 1 = 4, t4 5
  expect_equal(out$abundance, c(4, 0, 4, 5))
  # Density is abundance / area: 4 / 2 = 2, 0 / 5 = 0, 4 / 8 = 0.5, 5 / 10 = 0.5
  expect_equal(out$density, c(2, 0, 0.5, 0.5))

  # t3 has four taxa with 1 each, so every p_i = 1/4
  #   H' = -4 * (1/4 * ln(1/4)) = ln 4 = 1.386
  #   D  = 4 * (1/4)^2 = 0.25, so 1 - D = 0.75 and 1 / D = 4
  #   J' = ln 4 / ln 4 = 1
  expect_equal(out$shannon[3], log(4))
  expect_equal(out$simpson[3], 0.75)
  expect_equal(out$invsimpson[3], 4)
  expect_equal(out$pielou[3], 1)

  # t1 has 3 of a and 1 of b, so p_a = 3/4 and p_b = 1/4
  #   H' = -(3/4 * ln(3/4) + 1/4 * ln(1/4)) = 0.2158 + 0.3466 = 0.5623
  #   D  = (3/4)^2 + (1/4)^2 = 0.5625 + 0.0625 = 0.625
  #   1 - D = 0.375 and 1 / D = 1.6
  #   J' = 0.5623 / ln 2 = 0.8113
  expect_equal(out$shannon[1], -(0.75 * log(0.75) + 0.25 * log(0.25)))
  expect_equal(out$simpson[1], 1 - 0.625)
  expect_equal(out$invsimpson[1], 1 / 0.625)
  expect_equal(out$pielou[1], out$shannon[1] / log(2))
})

test_that("matches calling vegan directly", {
  x <- metrics_data()
  keep <- rowSums(x$comm) > 0
  out <- su_metrics(x, c("richness", "shannon", "simpson", "invsimpson"))

  expect_equal(out$richness, unname(vegan::specnumber(x$comm)))
  for (index in c("shannon", "simpson", "invsimpson")) {
    expect_equal(out[[index]][keep],
                 unname(vegan::diversity(x$comm, index)[keep]))
  }
})

test_that("diversity indices are NA where they are not defined", {
  out <- su_metrics(metrics_data(), c("shannon", "simpson", "invsimpson",
                                      "pielou"))
  # t2 is empty, so every p_i = 0 / 0 and none of the indices can be
  # calculated
  expect_true(all(is.na(unlist(out[2, -1]))))
  # t4 has one taxon, so p_a = 5/5 = 1
  #   H' = -(1 * ln 1) = 0
  #   J' = H' / ln(richness) = 0 / ln 1 = 0 / 0, which is not defined
  expect_equal(out$shannon[4], 0)
  expect_true(is.na(out$pielou[4]))
})

test_that("passes extra settings on to vegan", {
  out <- su_metrics(metrics_data(), "shannon", base = 2)
  # t3 with log base 2: H' = -4 * (1/4 * log2(1/4)) = log2 4 = 2
  expect_equal(out$shannon[3], 2)
})

test_that("takes any metric as a named function", {
  x <- metrics_data()
  out <- su_metrics(x, list("richness", max_count = function(m) {
    apply(m, 1, max)
  }))
  expect_equal(names(out), c("FileName", "richness", "max_count"))
  # Largest count in each row: t1 3, t2 0, t3 1, t4 5
  expect_equal(out$max_count, c(3, 0, 1, 5))
})

test_that("works on a selected stratum and the synthetic data", {
  files <- c(system.file("extdata", "synthetic_counts.csv", package = "ST4"),
             system.file("extdata", "synthetic_metadata.csv", package = "ST4"))
  x <- select_stratum(read_counts(files[1], files[2]), "sand")
  out <- su_metrics(x, c("richness", "density"))
  expect_equal(nrow(out), nrow(x$comm))
  expect_true(all(out$richness >= 0))
})

test_that("stops on bad metrics", {
  x <- metrics_data()
  expect_error(su_metrics(x, "shanon"), "Did you mean `shannon`")
  expect_error(su_metrics(x, "biomass"), "Unknown metric `biomass`")
  expect_error(su_metrics(x, list(function(m) rowSums(m))), "name")
  expect_error(su_metrics(x, list(bad = function(m) 1:2)), "one value per row")
  expect_error(su_metrics(x, c("richness", "richness")), "unique")
  expect_error(su_metrics(x, character(0)), "at least one")
  expect_error(su_metrics(data.frame()), "su_data")
})
