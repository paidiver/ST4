bodc_file <- function(name) {
  system.file("extdata", paste0("bodc_", name, ".csv"), package = "ST4")
}

test_that("the BODC files read and join", {
  x <- read_counts(bodc_file("counts"), bodc_file("metadata"))
  # Reviewer labels for Observer 1's finds: 83 annotations on 67 images.
  # 140 labels in the label set plus 4 used but not in it: 144.
  expect_equal(nrow(x$comm), 67)
  expect_equal(sum(x$comm), 83)
  expect_equal(ncol(x$comm), 144)
  # Only images with annotations are in the data
  expect_true(all(rowSums(x$comm) >= 1))
  # No image areas are published, so each image is 1 m²
  expect_true(all(x$meta$Area_seabed_m2 == 1))
  expect_equal(unique(x$meta$Survey), "KM1808")
})

test_that("the labels file lists every count column", {
  x <- read_counts(bodc_file("counts"), bodc_file("metadata"))
  labels <- utils::read.csv(bodc_file("labels"), na.strings = "")
  expect_equal(labels$label, colnames(x$comm))
  expect_equal(sum(labels$in_label_set), 140)
  # Labels not in the published set have no taxonomic name
  expect_true(all(is.na(labels$lowest_taxonomic_name[!labels$in_label_set])))
})

test_that("image ids are the date and time the image was taken", {
  meta <- utils::read.csv(bodc_file("metadata"))
  taken <- as.POSIXct(as.character(meta$FileName), format = "%Y%m%d%H%M%S",
                      tz = "UTC")
  expect_equal(format(taken, "%Y-%m-%d %H:%M:%S"), meta$Datetime)
  # Cruise KM1808 ran from 14 May to 16 June 2018
  expect_true(all(taken >= as.POSIXct("2018-05-14", tz = "UTC") &
                    taken < as.POSIXct("2018-06-17", tz = "UTC")))
})
