## Load libraries
library(timbR)
library(ranger)
library(testthat)

## train initial random forest on iris
rf <- ranger(Species ~ ., data = iris, num.trees = 10, keep.inbag = TRUE)

## define correct input variables
metric    <- "weighted splitting variables"
dist <- measure_distances(rf = rf, metric = metric)
cluster <- cluster_trees(rf = rf, num.clusters = 2, distance.matrix = dist())
num_trees <- 2

expect_silent(select_trees(rf = rf, num.trees = num_trees, distance.matrix = dist))
rf_rep <- select_trees(rf = rf, num.trees = num_trees, distance.matrix = dist)

## test input
test_that("Test input", {
  ## missing input
  # ranger object is missing
  expect_error(select_trees(rf = NULL, num.trees = num_trees, distance.matrix = dist))
  # number of trees to select is missing
  expect_error(select_trees(rf = rf, num.trees = NULL, distance.matrix = dist))
  # distance matrix is missing
  expect_error(select_trees(rf = rf, num.trees = num_trees, distance.matrix = NULL))

  ## invalid input
  # ranger object is not a ranger object
  expect_error(select_trees(rf = 123, num.trees = num_trees, distance.matrix = dist))
  # ranger object is trained without writing forest
  expect_error(select_trees(rf = ranger(Species ~ ., data = iris, write.forest = FALSE), num.trees = num_trees, distance.matrix = NULL))
  # number of trees to select is not a number
  expect_error(select_trees(rf = rf, num.trees = "abc", distance.matrix = dist))
  # number of trees to select is higher than number of trees in the forest
  expect_error(select_trees(rf = rf, num.trees = 20, distance.matrix = dist))
  # distance matrix is not a matrix
  expect_error(select_trees(rf = rf, num.trees = num_trees, distance.matrix = "abc"))
  # distance matrix has wrong size
  expect_error(select_trees(rf = rf, num.trees = num_trees, distance.matrix = matrix(0, nrow = 20, ncol = 20)))

  ## clustering
  expect_silent(select_trees(rf = rf, num.trees = num_trees, distance.matrix = dist, clustering = cluster))
  # requests more trees than clusters
  expect_error(select_trees(rf = rf, num.trees = 3, distance.matrix = dist, clustering = cluster))
  # clustering has wrong format
  expect_error(select_trees(rf = rf, num.trees = num_trees, distance.matrix = dist, clustering = "abc"))
  expect_error(select_trees(rf = rf, num.trees = num_trees, distance.matrix = dist, clustering = rep(1, num_trees)))
})

## test output
test_that("Test output", {
  expect_equal(class(rf_rep), "ranger")
  expect_equal(rf_rep$num.trees, num_trees)
  expect_equal(length(rf_rep$inbag.counts), num_trees)
})
