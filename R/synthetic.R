#' Synthetic example data
#'
#' Two made-up CSV files in the same format as real seabed survey data, for
#' trying out the package. None of the values come from a real survey.
#'
#' * `synthetic_counts.csv`: one row per tile and one column per taxon. The
#'   first column holds the tile ids.
#' * `synthetic_metadata.csv`: one row per tile, with columns `FileName`
#'   (tile id), `Substratum`, `Habitat`, `Latitude`, `Longitude` and
#'   `Area_seabed_m2`.
#'
#' There are 600 tiles of about 7 m² each and 40 taxa. The tiles are spread
#' over 7 substrata grouped into 4 habitats. Hard substrata have many more
#' individuals per tile than sand and coarse ones, and each taxon is more
#' common in one habitat. A few taxa are common and most are rare. The
#' numbers of individuals per tile in each substratum are set to roughly
#' match those published in Table 1 of Benoist et al. (2019). Coordinates are
#' placed near 0° N 0° E so they cannot be mistaken for a real site.
#'
#' The files follow the same layout as real data, so real files can be used
#' in their place by changing the file paths. They are made by
#' `data-raw/synthetic_data.R`.
#'
#' @references
#' Benoist, N.M.A., Morris, K.J., Bett, B.J., et al. (2019). Monitoring mosaic
#' biotopes in a marine conservation zone by autonomous underwater vehicle.
#' *Conservation Biology*, 33(5), 1174-1186. \doi{10.1111/cobi.13312}
#'
#' @examples
#' counts_file <- system.file("extdata", "synthetic_counts.csv", package = "ST4")
#' metadata_file <- system.file("extdata", "synthetic_metadata.csv",
#'                              package = "ST4")
#' x <- read_counts(counts_file, metadata_file)
#' x
#' @name synthetic_data
NULL

# Builds the synthetic data. Called by data-raw/synthetic_data.R, and by the
# tests to check the shipped files can be rebuilt exactly.
make_synthetic_data <- function(seed = 2026) {
  withr::with_seed(seed, {
    # Substrata, their habitat, number of tiles and mean individuals per tile
    strata <- data.frame(
      substratum = c("hard", "hard + coarse", "hard + sand", "coarse + hard",
                     "sand + hard", "coarse", "sand"),
      habitat = c("hard", "hard", "hard", "intermediate", "intermediate",
                  "coarse", "sand"),
      n_tiles = c(30, 50, 50, 130, 30, 150, 160),
      mean_individuals = c(22, 17, 19, 2.5, 3.3, 0.7, 1.3)
    )
    habitats <- c("hard", "intermediate", "coarse", "sand")
    n_taxa <- 40

    # A few common taxa and many rare ones
    weight <- sort(stats::rlnorm(n_taxa, sdlog = 1.5), decreasing = TRUE)
    # Each taxon is 4 times as common in its preferred habitat
    preferred <- sample(habitats, n_taxa, replace = TRUE,
                        prob = c(0.5, 0.2, 0.15, 0.15))

    tile_stratum <- rep(seq_len(nrow(strata)), strata$n_tiles)
    n_tiles <- length(tile_stratum)
    tile_habitat <- strata$habitat[tile_stratum]

    pref <- outer(tile_habitat, preferred, function(h, p) ifelse(h == p, 4, 1))
    share <- sweep(pref, 2, weight, "*")
    share <- share / rowSums(share)
    mu <- share * strata$mean_individuals[tile_stratum]

    # Clumped counts: negative binomial, like real seabed fauna
    counts <- matrix(stats::rnbinom(length(mu), mu = mu, size = 0.5),
                     nrow = n_tiles)
    # Real files only list taxa that were seen, so every taxon gets at least
    # one individual, in a tile of its preferred habitat
    for (j in which(colSums(counts) == 0)) {
      tiles <- which(tile_habitat == preferred[j])
      i <- tiles[sample.int(length(tiles), 1)]
      counts[i, j] <- 1
    }

    ids <- sprintf("tile_%04d", seq_len(n_tiles))
    dimnames(counts) <- list(ids, sprintf("taxon_%02d", seq_len(n_taxa)))

    area <- 7.3 * exp(stats::rnorm(n_tiles, sd = 0.08))
    area <- pmin(pmax(area, 4.6), 10.6)

    metadata <- data.frame(
      FileName = ids,
      Substratum = strata$substratum[tile_stratum],
      Habitat = tile_habitat,
      Latitude = round(stats::runif(n_tiles, 0, 0.05), 6),
      Longitude = round(stats::runif(n_tiles, 0, 0.05), 6),
      Area_seabed_m2 = round(area, 6)
    )
    # Real metadata files are not in the same row order as the counts
    metadata <- metadata[sample(n_tiles), ]

    list(counts = as.data.frame(counts), metadata = metadata)
  })
}

# Writes the two CSVs like utils::write.csv() with row names, as in the real
# files: a blank-named first column of tile ids (counts) or row numbers
# (metadata)
write_synthetic_data <- function(data, dir) {
  utils::write.csv(data$counts, file.path(dir, "synthetic_counts.csv"))
  utils::write.csv(data$metadata, file.path(dir, "synthetic_metadata.csv"))
}
