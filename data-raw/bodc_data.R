# Builds the BODC example data in inst/extdata/ from the BODC download.
# See ?bodc_data for the source, licence and what was changed.
#
# Download both data sets from BODC and unzip them into one folder:
#   https://doi.org/10.5285/5f9b8ce1-0b4d-4061-e063-7086abc0b131 (annotations)
#   https://doi.org/10.5285/5f9ba3ef-e59d-446d-e063-7086abc00439 (labels)
# Then, from the package root:
#   bodc_dir <- "path/to/folder"
#   source("data-raw/bodc_data.R")
#
# The steps follow the iFDO conversion on issue #34.

stopifnot(exists("bodc_dir"), dir.exists(bodc_dir))

# BODC files have two rows under the header (allowed values and a
# description), and non-breaking spaces in the names and values
read_bodc <- function(file) {
  x <- utils::read.csv(file.path(bodc_dir, file), check.names = FALSE,
                       colClasses = "character")[-(1:2), ]
  names(x) <- trimws(gsub("\u00a0", " ", names(x)))
  x[] <- lapply(x, function(v) trimws(gsub("\u00a0", " ", v)))
  x
}
annotations <- read_bodc(paste0("annotation_submission_deepccz_annotator_",
                                "comparison_annotation_attributes.csv"))
label_set <- read_bodc(paste0("label_submission_deepccz_annotator_",
                              "comparison_label_attributes.csv"))

# Observer 1's finds, with the labels given by the independent reviewer
creator <- "Reviewer identification (found by Observer 1)"
annotations <- annotations[annotations[["annotation-human-creator"]] == creator &
                             annotations[["image-filename"]] != "" &
                             annotations[["annotation-label-name"]] != "", ]

# Every label in the label set, plus labels used in the annotations that are
# not in it, so that no annotation is lost
in_set <- unique(label_set[["annotation-label-name"]])
in_set <- in_set[in_set != ""]
used <- unique(annotations[["annotation-label-name"]])
all_labels <- c(in_set, sort(setdiff(used, in_set)))

# Counts: each annotation is one individual. One row per image, one column
# per label, with 0 for labels not seen in an image
tab <- table(annotations[["image-filename"]],
             factor(annotations[["annotation-label-name"]], all_labels))
counts <- data.frame(image = rownames(tab), as.data.frame.matrix(tab),
                     check.names = FALSE)

# Metadata. Image file names are the date and time the image was taken. No
# image areas are published, so each image is given 1 m².
taken <- as.POSIXct(counts$image, format = "%Y%m%d%H%M%S", tz = "UTC")
stopifnot(!anyNA(taken))
metadata <- data.frame(FileName = counts$image, Area_seabed_m2 = 1,
                       Survey = "KM1808",
                       Datetime = format(taken, "%Y-%m-%d %H:%M:%S"))

# What each label is, and whether it is in the published label set
labels <- data.frame(
  label = all_labels,
  lowest_taxonomic_name = label_set[["lowest-taxonomic-name"]][
    match(all_labels, label_set[["annotation-label-name"]])],
  in_label_set = all_labels %in% in_set
)

dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)
utils::write.csv(counts, "inst/extdata/bodc_counts.csv", row.names = FALSE)
utils::write.csv(metadata, "inst/extdata/bodc_metadata.csv", row.names = FALSE)
utils::write.csv(labels, "inst/extdata/bodc_labels.csv", row.names = FALSE,
                 na = "")
