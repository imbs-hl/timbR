## Load libraries
library(timbR)
library(ranger)
library(testthat)

## Prepare inputs
regr_data <- longley %>% data.frame()
# Train random forest with ranger
rf <- ranger(Employed ~ ., data = regr_data, num.trees = 10, importance = "permutation")
# Calculate pair-wise distances for all trees
rep_tree <- generate_tree_reimplementation(rf = rf, metric = "splitting variables", train_data = regr_data, dependent_varname = "Employed", importance.mode = TRUE, imp.num.var = 2)
# Get predictions
rep_tree_predictions <- predict(rep_tree, regr_data)$predictions
# Calibrated predictions
rep_tree_calibrated_predictions <- get_calibrated_prediction_regression(y_cal_pred = rep_tree_predictions, y_cal = regr_data$Employed, y_test_pred = rep_tree_predictions, significance_level = 0.05)


## Test correct input ----
test_that("Test correct input", {
  expect_silent(get_calibrated_prediction_regression(y_cal_pred = rep_tree_predictions, y_cal = regr_data$Employed, y_test_pred = rep_tree_predictions, significance_level = 0.05))
  expect_silent(get_calibrated_prediction_regression(y_cal_pred = rep_tree_predictions, y_cal = regr_data$Employed, y_test_pred = rep_tree_predictions, significance_level = 0.5))
  })


## Test incorrect inputs ----
test_that("Test incorrect inputs", {
  expect_error(get_calibrated_prediction_regression(y_cal_pred = "abc", y_cal = regr_data$Employed, y_test_pred = rep_tree_predictions, significance_level = 0.05))
  expect_error(get_calibrated_prediction_regression(y_cal_pred = rep_tree_predictions, y_cal = "abc", y_test_pred = rep_tree_predictions, significance_level = 0.05))
  expect_error(get_calibrated_prediction_regression(y_cal_pred = rep_tree_predictions, y_cal = regr_data$Employed, y_test_pred = "abc", significance_level = 0.05))
  expect_error(get_calibrated_prediction_regression(y_cal_pred = rep_tree_predictions, y_cal = regr_data$Employed, y_test_pred = rep_tree_predictions, significance_level = "abc"))
})

## Test output
test_that("Test output", {
  expect_equal(class(rep_tree_calibrated_predictions), "data.frame")
  expect_equal(ncol(rep_tree_calibrated_predictions), 3)
})
