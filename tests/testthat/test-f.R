
# Unit tests file is a good place to make sense of what functions do. Sometimes
# it's more helpful to read tests instead of the actual code to understand the
# functions properly.

testthat::test_that("fv produces correct results", {

  res <- fv(a = 4, s = 2)
  testthat::expect_equal(
    object = res,
    expected = 2
  )

  res <- fv(a = 9, s = 2)
  testthat::expect_equal(
    object = res,
    expected = 3
  )

  res <- fv(a = 16, s = 2)
  testthat::expect_equal(res, 4)

  res <- fv(2, 2)
  testthat::expect_equal(res, 1.41421, tolerance = 0.01)

  res <- fv(a = 120, s = 6)
  testthat::expect_equal(res, 2.23, tolerance = 0.1)

})

# Test behavior, not implementation. Check that the results are as expected not
# the approach in coding itself or the algorithm used is always the same.

# test each new feature you add. Having all features and corner cases covered by
# tests helps add new features.
# Possible approaches:
# add feature and write tests,
# write tests, capture the results you want to get and write code to achieve
# this. You can do it with new functions too - write requirements of a function
# as tests and work towards passing all of them
#
# You write tests for future you and everyone else that wants to use your code.
#
# Less bugs after updating the existing codebase after weeks.
#
#
#
testthat::test_that("fv works for numeric vectors", {

  a <- c(1:4)
  res <- fv(a, s = 2)

  # fv() produces vector of the exact same length as input vector
  testthat::expect_length(
    object = res,
    n = length(a)
  )

  # check that results are correct for when using a vector
  reference_vec <- c(1, 1.414214, 1.732051, 2)
  testthat::expect_equal(
    res,
    reference_vec,
    tolerance = 0.1
  )

})



# How to test a complex function? One function wraps a lot of functionalities -
# e.g. read data, transforms it, calculates new metrics and finally creates a
# plot (and possibly save to the disk). Consider breaking up into smaller
# functions and carefully test each of them separately.

testthat::test_that("fv_strict accepts only numeric vectors", {

  test_vec <- c("A")
  testthat::expect_error(
    object = fv_strict(test_vec),
    regexp = "parameter a should be numeric vector"
  )

  # prevent logical vectors being coerced to integer vector
  test_vec <- c(T, F)
  testthat::expect_error(
    object = fv_strict(test_vec),
    regexp = "parameter a should be numeric vector"
  )

})



# Check if object is equal to reference file. This can be .csv, .Rds, ...
testthat::test_that("results are equal to reference file", {

  df <- data.frame(
    gene_id = paste0("ENSG0", 1:5),
    v1 = c(235, 426, 864, 145, 306.25)
  )

  df[["v2"]] <- fv_strict(df[["v1"]], s = 2)

  ref <- readRDS(system.file("test_data", "ref.Rds", package = "RpkgCheck"))

  testthat::expect_equal(
    df,
    ref
  )

})
