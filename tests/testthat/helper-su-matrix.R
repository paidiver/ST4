# Durden et al. (2016) Table 1 gives the SUs required per stratum for 2 strata
# with alpha 0.05 and power 0.9. Rows are CV % and columns are delta %.
# NA marks the grey cells in the paper, where more than 10 are needed.
durden_table1 <- matrix(c(
   7,  4,  3,  3,  2,  2,  2,  2,  2,  2,
  NA, NA,  7,  4,  3,  3,  2,  2,  2,  2,
  NA, NA, NA,  8,  5,  3,  3,  2,  2,  2,
  NA, NA, NA, NA,  7,  4,  3,  3,  2,  2,
  NA, NA, NA, NA, 10,  6,  4,  3,  2,  2,
  NA, NA, NA, NA, NA,  8,  5,  3,  3,  2,
  NA, NA, NA, NA, NA, 10,  6,  4,  3,  2,
  NA, NA, NA, NA, NA, NA,  7,  4,  3,  3,
  NA, NA, NA, NA, NA, NA,  8,  5,  4,  3,
  NA, NA, NA, NA, NA, NA, 10,  6,  4,  3
), nrow = 10, byrow = TRUE)
table1_cv <- seq(5, 50, 5)
table1_delta <- c(10, 14, 20, 28, 40, 56, 79, 112, 158, 224)
