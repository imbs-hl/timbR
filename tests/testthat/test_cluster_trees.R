## Load libraries
library(timbR)
library(ranger)
library(testthat)

## train initial random forest on iris
rf <- ranger(Species ~ ., data = iris, num.trees = 10, keep.inbag = TRUE)

## define correct input variables
metric    <- "weighted splitting variables"
dist <- measure_distances(rf = rf, metric = metric)
num_clusters <- 2

cluster_ids <- cluster_trees(rf = rf, num.clusters = num_clusters, distance.matrix = dist)

## test input
test_that("Test input", {
  ## missing input
  expect_error(cluster_trees(rf = NULL, num.clusters = num_clusters, distance.matrix = dist))
  expect_error(cluster_trees(rf = rf, num.clusters = NULL, distance.matrix = dist))
  expect_error(cluster_trees(rf = rf, num.clusters = num_clusters, distance.matrix = NULL))

  ## wrong input
  # rf is not  ranger object
  expect_error(cluster_trees(rf = 123, num.clusters = num_clusters, distance.matrix = dist))
  # num.clusters is nut a number or AUTO
  expect_error(cluster_trees(rf = rf, num.clusters = "abc", distance.matrix = dist))
  # num.clusters is greater than the number of trees
  expect_error(cluster_trees(rf = rf, num.clusters = 20, distance.matrix = dist))
  # distance.matrix is nota matrix
  expect_error(cluster_trees(rf = rf, num.clusters = num_clusters, distance.matrix = "abc"))
  # distance.matrix has wrong dimensions
  expect_error(cluster_trees(rf = rf, num.clusters = num_clusters, distance.matrix = matrix(0, nrow = 20, ncol = 20)))

  ## correct input
  # defined k
  expect_silent(cluster_trees(rf = rf, num.clusters = num_clusters, distance.matrix = dist))
  # auto detection of k
  expect_silent(cluster_trees(rf = rf, num.clusters = "AUTO", distance.matrix = dist))
})

## test output
test_that("Test output", {
  # output has same length as defined clusters
  expect_equal(max(cluster_trees(rf = rf, num.clusters = num_clusters, distance.matrix = dist)), num_clusters)
  # number of auto detected clusters is greater than zero
  # expect_condition(cluster_trees(rf = rf, num.clusters = "AUTO", distance.matrix = dist) >= 1)
  # number of auto detected clusters is smaller than number of trees +1
  # expect_condition(cluster_trees(rf = rf, num.clusters = "AUTO", distance.matrix = dist) <= (rf$num.trees + 1))
  # output has three columns and num.trees rows
  expect_equal(ncol(cluster_trees(rf = rf, num.clusters = num_clusters, distance.matrix = dist)), 3)
  expect_equal(nrow(cluster_trees(rf = rf, num.clusters = num_clusters, distance.matrix = dist)), rf$num.trees)

})
