# Testing approach

There is no published raw data with known results to test against, and
random SUs never match exactly. So the tests check the method in four ways.

1. **Known answers on tiny made-up data.** A few images where the right
   answer can be worked out by hand. The steps are written in the test
   comments. For example, three identical images give the same curve for
   every random order, and Durden et al. (2016) Eqn 1 matches all 59 cells
   of their Table 1.
2. **Properties that always hold**, checked on the synthetic data over
   several seeds:
   - each image is in at most one SU
   - each SU reaches the target and goes over by less than one image
   - SUs plus dropped images give back the stratum's counts and area
   - richness, area and individuals never drop along an SU size curve
3. **Same result as calling vegan directly.** Metrics and curves are
   compared with `vegan::diversity()` and `vegan::specnumber()`, including
   the slow way of re-summing the images at every step.
4. **Fixed-seed snapshots.** `test-snapshots.R` saves the main printouts on
   the synthetic data to `testthat/_snaps/snapshots.md`. A test fails if the
   output changes. If a change is intended, check the new output and accept
   it with `testthat::snapshot_accept("snapshots")`.

Only synthetic data is used. Snapshot tests are skipped on CRAN, and run
locally and on CI (where `NOT_CRAN` is `"true"`).

Run all the tests with `devtools::test()`.
