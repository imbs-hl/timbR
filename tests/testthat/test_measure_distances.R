## Load libraries
library(timbR)
library(ranger)
library(testthat)

## train initial random forest on iris
rf <- ranger(Species ~ ., data = iris, num.trees = 10)

## define correct input variables
metric    <- "splitting variables"
test_data <- iris

dist <- measure_distances(rf = rf, metric = metric, test_data = test_data)
dist1 <- measure_distances(rf = rf, metric = "splitting variables")
dist2 <- measure_distances(rf = rf, metric = "weighted splitting variables")
dist3 <- measure_distances(rf = rf, metric = "terminal nodes", test_data = test_data)
dist4 <- measure_distances(rf = rf, metric = "prediction", test_data = test_data)
dist5 <-  measure_distances(rf = rf, metric = "combined", test_data = test_data)

## test input
test_that("Test input", {
  ## missing input
  expect_error(measure_distances(rf = NULL, metric = metric))
  expect_error(measure_distances(rf = rf, metric = NULL))
  expect_error(measure_distances(rf = rf, metric = "terminal nodes", test_data = NULL))
  expect_error(measure_distances(rf = rf, metric = "prediction", test_data = NULL))
  expect_error(measure_distances(rf = rf, metric = "combined", test_data = NULL))

  ## wrong input
  expect_error(measure_distances(rf = 12345, metric = metric))
  expect_error(measure_distances(rf = ranger(Species ~., data = iris, write.forest = FALSE, num.trees = 10), metric = metric))
  expect_error(measure_distances(rf = rf, metric = 12345))
  expect_error(measure_distances(rf = rf, metric = "prediction", test_data = "abc"))
  expect_error(measure_distances(rf = rf, metric = "prediction", test_data = test_data[1,]))
  expect_error(measure_distances(rf = rf, metric = "combined", test_data = "abc"))
  expect_error(measure_distances(rf = rf, metric = "combined", test_data = test_data[1,]))
  ## unnecassary input
  expect_message(measure_distances(rf = rf, metric = "splitting variables", test_data = test_data))
  expect_message(measure_distances(rf = rf, metric = "weighted splitting variables", test_data = test_data))
})

## test output
test_that("Test output", {
  ## Dimension of output
  expect_equal(dim(measure_distances(rf = rf, metric = metric)), c(rf$num.trees, rf$num.trees))

  ## Distances are not NA for all measures
  expect_equal(sum(is.na(dist1)), 0)
  expect_equal(sum(is.na(dist2)), 0)
  expect_equal(sum(is.na(dist3)), 0)
  expect_equal(sum(is.na(dist4)), 0)
  expect_equal(sum(is.na(dist5)), 0)
})
