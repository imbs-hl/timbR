## Load libraries
library(timbR)
library(ranger)
library(testthat)

## train initial random forest on iris
rf <- ranger(Species ~ ., data = iris, num.trees = 10, keep.inbag = TRUE)

## define correct input variables
metric    <- "weighted splitting variables"
dist <- measure_distances(rf = rf, metric = metric)
num_trees <- 2

rf_rep <- select_trees(rf = rf, num.trees = num_trees, distance.matrix = dist)

## test input
test_that("Test input", {
  ## missing input
  expect_error(select_trees(rf = NULL, num.trees = num_trees, distance.matrix = dist))
  expect_error(select_trees(rf = rf, num.trees = NULL, distance.matrix = dist))
  expect_error(select_trees(rf = rf, num.trees = num_trees, distance.matrix = NULL))

  ## wrong input
  expect_error(select_trees(rf = 123, num.trees = num_trees, distance.matrix = dist))
  expect_error(select_trees(rf = ranger(Species ~ ., data = iris, write.forest = FALSE), num.trees = num_trees, distance.matrix = NULL))
  expect_error(select_trees(rf = rf, num.trees = "abc", distance.matrix = dist))
  expect_error(select_trees(rf = rf, num.trees = 20, distance.matrix = dist))
  expect_error(select_trees(rf = rf, num.trees = num_trees, distance.matrix = "abc"))
  expect_error(select_trees(rf = rf, num.trees = num_trees, distance.matrix = matrix(0, nrow = 20, ncol = 20)))
})

## test output
test_that("Test output", {
  expect_equal(class(rf_rep), "ranger")
  expect_equal(rf_rep$num.trees, num_trees)
  expect_equal(length(rf_rep$inbag.counts), num_trees)
})
