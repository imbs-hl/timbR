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
rep_tree_calibrated_predictions <- get_calibrated_prediction_regression_mondrian(y_cal_pred = rep_tree_predictions, y_cal = regr_data$Employed, y_test_pred = rep_tree_predictions, significance_level = 0.05,
                                                                                 tree = rep_tree, cal_data = regr_data, test_data = regr_data, dependent_varname = "Employed", show_node_id = FALSE)


## Test correct input ----
test_that("Test correct input", {
  expect_silent(get_calibrated_prediction_regression_mondrian(y_cal_pred = rep_tree_predictions, y_cal = regr_data$Employed, y_test_pred = rep_tree_predictions, significance_level = 0.05,
                                                              tree = rep_tree, cal_data = regr_data, test_data = regr_data, dependent_varname = "Employed", show_node_id = FALSE))
  expect_silent(get_calibrated_prediction_regression_mondrian(y_cal_pred = rep_tree_predictions, y_cal = regr_data$Employed, y_test_pred = rep_tree_predictions, significance_level = 0.05,
                                                              tree = rep_tree, cal_data = regr_data, test_data = regr_data, dependent_varname = "Employed", show_node_id = TRUE))
  })


## Test incorrect inputs ----
test_that("Test incorrect inputs", {
  expect_error(get_calibrated_prediction_regression_mondrian(y_cal_pred = "abc", y_cal = regr_data$Employed, y_test_pred = rep_tree_predictions, significance_level = 0.05,
                                                             tree = rep_tree, cal_data = regr_data, test_data = regr_data, dependent_varname = "Employed", show_node_id = FALSE))
  expect_error(get_calibrated_prediction_regression_mondrian(y_cal_pred = rep_tree_predictions, y_cal = "abc", y_test_pred = rep_tree_predictions, significance_level = 0.05,
                                                             tree = rep_tree, cal_data = regr_data, test_data = regr_data, dependent_varname = "Employed", show_node_id = FALSE))
  expect_error(get_calibrated_prediction_regression_mondrian(y_cal_pred = rep_tree_predictions, y_cal = regr_data$Employed, y_test_pred = "abc", significance_level = 0.05,
                                                             tree = rep_tree, cal_data = regr_data, test_data = regr_data, dependent_varname = "Employed", show_node_id = FALSE))
  expect_error(get_calibrated_prediction_regression_mondrian(y_cal_pred = rep_tree_predictions, y_cal = regr_data$Employed, y_test_pred = rep_tree_predictions, significance_level = "abc",
                                                             tree = rep_tree, cal_data = regr_data, test_data = regr_data, dependent_varname = "Employed", show_node_id = FALSE))
  expect_error(get_calibrated_prediction_regression_mondrian(y_cal_pred = rep_tree_predictions, y_cal = regr_data$Employed, y_test_pred = rep_tree_predictions, significance_level = 0.05,
                                                             tree = "abc", cal_data = regr_data, test_data = regr_data, dependent_varname = "Employed", show_node_id = FALSE))
  expect_error(get_calibrated_prediction_regression_mondrian(y_cal_pred = rep_tree_predictions, y_cal = regr_data$Employed, y_test_pred = rep_tree_predictions, significance_level = 0.05,
                                                             tree = rep_tree, cal_data = "abc", test_data = regr_data, dependent_varname = "Employed", show_node_id = FALSE))
  expect_error(get_calibrated_prediction_regression_mondrian(y_cal_pred = rep_tree_predictions, y_cal = regr_data$Employed, y_test_pred = rep_tree_predictions, significance_level = 0.05,
                                                             tree = rep_tree, cal_data = regr_data, test_data = "abc", dependent_varname = "Employed", show_node_id = FALSE))
  expect_error(get_calibrated_prediction_regression_mondrian(y_cal_pred = rep_tree_predictions, y_cal = regr_data$Employed, y_test_pred = rep_tree_predictions, significance_level = 0.05,
                                                             tree = rep_tree, cal_data = regr_data, test_data = regr_data, dependent_varname = "abc", show_node_id = FALSE))
  expect_error(get_calibrated_prediction_regression_mondrian(y_cal_pred = rep_tree_predictions, y_cal = regr_data$Employed, y_test_pred = rep_tree_predictions, significance_level = 0.05,
                                                             tree = rep_tree, cal_data = regr_data, test_data = regr_data, dependent_varname = "Employed", show_node_id = "abc"))
})

## Test output
test_that("Test output", {
  expect_equal(class(rep_tree_calibrated_predictions), "data.frame")
})
