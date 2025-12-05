## Load libraries
library(timbR)
library(ranger)
library(testthat)

## Prepare inputs
rf <- ranger(Species ~ ., data = iris, num.trees = 10, importance = "permutation")
rf2 <- ranger(Species ~ ., data = iris, num.trees = 10, write.forest = FALSE, importance = "impurity_corrected")
rf3 <- ranger(Species ~ ., data = iris, num.trees = 10, importance = "permutation", probability = TRUE)
rf_rep <- generate_tree(rf = rf, train_data = iris, dependent_varname = "Species")

## Test correct input ----
test_that("Test correct input", {
  # classification
  expect_silent(generate_tree(rf = rf, train_data = iris, dependent_varname = "Species"))
  expect_silent(generate_tree(rf = rf, metric = "splitting variables", train_data = iris, dependent_varname = "Species"))
  expect_silent(generate_tree(rf = rf, metric = "weighted splitting variables", train_data = iris, dependent_varname = "Species"))
  expect_silent(generate_tree(rf = rf, metric = "terminal nodes", train_data = iris, test_data = iris, dependent_varname = "Species"))
  expect_silent(generate_tree(rf = rf, metric = "prediction", train_data = iris, test_data = iris, dependent_varname = "Species"))
  expect_silent(generate_tree(rf = rf, metric = "prediction", train_data = iris, test_data = iris, dependent_varname = "Species", importance.mode = TRUE, imp.num.var = 3))
  expect_silent(generate_tree(rf = rf, metric = "prediction", train_data = iris, test_data = iris, dependent_varname = "Species", importance.mode = TRUE, imp.num.var = "automatic"))
  expect_silent(generate_tree(rf = rf, metric = "prediction", train_data = iris, test_data = iris, dependent_varname = "Species", probs_quantiles = c(0.25,0.75)))
  expect_silent(generate_tree(rf = rf, metric = "prediction", train_data = iris, test_data = iris, dependent_varname = "Species", epsilon = 0.5))
  expect_silent(generate_tree(rf = rf, metric = "prediction", train_data = iris, test_data = iris, dependent_varname = "Species", min.bucket = 40))
  expect_silent(generate_tree(rf = rf, metric = "prediction", train_data = iris, test_data = iris, dependent_varname = "Species", num.splits = 2))
  # probability
  expect_silent(generate_tree(rf = rf3, train_data = iris, dependent_varname = "Species"))
  expect_silent(generate_tree(rf = rf3, metric = "splitting variables", train_data = iris, dependent_varname = "Species"))
  expect_silent(generate_tree(rf = rf3, metric = "weighted splitting variables", train_data = iris, dependent_varname = "Species"))
  expect_silent(generate_tree(rf = rf3, metric = "terminal nodes", train_data = iris, test_data = iris, dependent_varname = "Species"))
  expect_silent(generate_tree(rf = rf3, metric = "prediction", train_data = iris, test_data = iris, dependent_varname = "Species"))
  expect_silent(generate_tree(rf = rf3, metric = "prediction", train_data = iris, test_data = iris, dependent_varname = "Species", importance.mode = TRUE, imp.num.var = 3))
  expect_silent(generate_tree(rf = rf3, metric = "prediction", train_data = iris, test_data = iris, dependent_varname = "Species", importance.mode = TRUE, imp.num.var = "automatic"))
  expect_silent(generate_tree(rf = rf3, metric = "prediction", train_data = iris, test_data = iris, dependent_varname = "Species", probs_quantiles = c(0.25,0.75)))
  expect_silent(generate_tree(rf = rf3, metric = "prediction", train_data = iris, test_data = iris, dependent_varname = "Species", epsilon = 0.5))
  expect_silent(generate_tree(rf = rf3, metric = "prediction", train_data = iris, test_data = iris, dependent_varname = "Species", min.bucket = 40))
  expect_silent(generate_tree(rf = rf3, metric = "prediction", train_data = iris, test_data = iris, dependent_varname = "Species", num.splits = 2))

  })

## Test messages ----
test_that("Test messages", {
  expect_message(generate_tree(rf = rf, metric = "splitting variables", train_data = iris, test_data = iris, dependent_varname = "Species"))
  expect_message(generate_tree(rf = rf, metric = "weighted splitting variables", train_data = iris, test_data = iris, dependent_varname = "Species"))
})

## Test missing inputs ----
test_that("Test missing inputs", {
  expect_error(generate_tree())
  expect_error(generate_tree(rf = NULL, metric = "splitting variables", train_data = iris, dependent_varname = "Species"))
  expect_error(generate_tree(rf = rf, metric = "splitting variables", train_data = NULL, dependent_varname = "Species"))
  expect_error(generate_tree(rf = rf, metric = "predictions", train_data = iris, dependent_varname = "Species"))
  expect_error(generate_tree(rf = rf, metric = "terminal nodes", train_data = iris, dependent_varname = "Species"))
})

## Test incorrect inputs ----
test_that("Test incorrect inputs", {
  expect_error(generate_tree(rf = 1234, metric = "splitting variables", train_data = iris, dependent_varname = "Species"))
  expect_error(generate_tree(rf = rf2, metric = "splitting variables", train_data = iris, dependent_varname = "Species"))
  expect_error(generate_tree(rf = rf, metric = "abc", train_data = iris, dependent_varname = "Species"))
  expect_error(generate_tree(rf = rf, metric = "splitting variables", train_data = 123, dependent_varname = "Species"))
  expect_error(generate_tree(rf = rf, metric = "prediction", train_data = iris, test_data = 123, dependent_varname = "Species"))
  expect_error(generate_tree(rf = rf, metric = "splitting variables", train_data = iris, dependent_varname = "Species", importance.mode = TRUE))
  expect_error(generate_tree(rf = rf, metric = "splitting variables", train_data = iris, dependent_varname = "Species", importance.mode = TRUE, imp.num.var = "abc"))
  expect_error(generate_tree(rf = rf, metric = "splitting variables", train_data = iris, dependent_varname = "Species", importance.mode = TRUE, imp.num.var = -1))
  expect_error(generate_tree(rf = rf, metric = "splitting variables", train_data = iris, dependent_varname = "Species", importance.mode = TRUE, imp.num.var = 123))
  expect_error(generate_tree(rf = rf, metric = "splitting variables", train_data = iris, dependent_varname = "Species", probs_quantiles = 123))
  expect_error(generate_tree(rf = rf, metric = "splitting variables", train_data = iris, dependent_varname = "Species", probs_quantiles = "123"))
  expect_error(generate_tree(rf = rf, metric = "splitting variables", train_data = iris, dependent_varname = "Species", probs_quantiles = c(0,1,1.2)))
  expect_error(generate_tree(rf = rf, metric = "splitting variables", train_data = iris, dependent_varname = "Species", epsilon = 123))
  expect_error(generate_tree(rf = rf, metric = "splitting variables", train_data = iris, dependent_varname = "Species", epsilon = "123"))
  expect_error(generate_tree(rf = rf, metric = "prediction", train_data = iris, test_data = iris, dependent_varname = "Species", min.bucket = -1))
  expect_error(generate_tree(rf = rf, metric = "prediction", train_data = iris, test_data = iris, dependent_varname = "Species", min.bucket = 1000))
  expect_error(generate_tree(rf = rf, metric = "prediction", train_data = iris, test_data = iris, dependent_varname = "Species", min.bucket = "1000"))
  expect_error(generate_tree(rf = rf, metric = "prediction", train_data = iris, test_data = iris, dependent_varname = "Species", min.bucket = TRUE))
  expect_error(generate_tree(rf = rf, metric = "prediction", train_data = iris, test_data = iris, dependent_varname = "Species", num.splits = TRUE))
  expect_error(generate_tree(rf = rf, metric = "prediction", train_data = iris, test_data = iris, dependent_varname = "Species", num.splits = 0))
  expect_error(generate_tree(rf = rf, metric = "prediction", train_data = iris, test_data = iris, dependent_varname = "Species", num.splits = Inf))
})

## Test output
test_that("Test output", {
  expect_equal(class(rf_rep), "ranger")
  expect_equal(rf_rep$num.trees, 1)
})
