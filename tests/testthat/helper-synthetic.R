# The synthetic survey shipped in inst/extdata, and its sand images
synthetic_survey <- function() {
  read_counts(system.file("extdata", "synthetic_counts.csv", package = "ST4"),
              system.file("extdata", "synthetic_metadata.csv", package = "ST4"))
}

synthetic_sand <- function() {
  select_stratum(synthetic_survey(), "sand")
}
