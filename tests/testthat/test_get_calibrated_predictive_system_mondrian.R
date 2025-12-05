## Load libraries
library(timbR)
library(ranger)
library(testthat)
library(ggplot2)

## Prepare inputs
# Get train, test and calibration data
regr_data <- diamonds %>% data.frame()
train_data <- regr_data[1:100,]
test_data <- regr_data[17981:17900,]
cal_data <- regr_data[35961:53940,]

# Train random forest with ranger
rf <- ranger(carat ~ ., data = train_data, num.trees = 10, importance = "permutation")

# Calculate pair-wise distances for all trees
rep_tree <- generate_tree(rf = rf, metric = "splitting variables", train_data = train_data, dependent_varname = "carat", importance.mode = TRUE, imp.num.var = 2, probs_quantiles = c(0.25,0.5,0.75), num.splits = 3)

# Get predictions
y_test_pred <- predict(rep_tree, test_data)$predictions
y_cal_pred <- predict(rep_tree, cal_data)$predictions
y_cal <- cal_data$carat

# Calibrated predictions
calibrated_predictions <- get_calibrated_predictive_system_mondrian(y_cal_pred = y_cal_pred, y_cal = y_cal, y_test_pred = y_test_pred[1:10],
                                                           significance_level = 0.05, interval_type = "two-tailed",
                                                           tree = rep_tree, cal_data=cal_data, test_data = test_data, dependent_varname="carat")



## Test correct input ----
test_that("Test correct input", {
  expect_silent(get_calibrated_predictive_system_mondrian(y_cal_pred = y_cal_pred, y_cal = y_cal, y_test_pred = y_test_pred[1:10], significance_level = 0.05, interval_type = "two-tailed", tree = rep_tree, cal_data=cal_data, test_data = test_data, dependent_varname="carat"))
  expect_silent(get_calibrated_predictive_system_mondrian(y_cal_pred = y_cal_pred, y_cal = y_cal, y_test_pred = y_test_pred[1:10], significance_level = 0.05, interval_type = "one-tailed", direction = "left-tailed", tree = rep_tree, cal_data=cal_data, test_data = test_data, dependent_varname="carat"))
  expect_silent(get_calibrated_predictive_system_mondrian(y_cal_pred = y_cal_pred, y_cal = y_cal, y_test_pred = y_test_pred[1:10], significance_level = 0.05, interval_type = "one-tailed", direction = "right-tailed", tree = rep_tree, cal_data=cal_data, test_data = test_data, dependent_varname="carat"))
  })


## Test incorrect inputs ----
test_that("Test incorrect inputs", {
  expect_error(get_calibrated_predictive_system_mondrian(y_cal_pred = "abc", y_cal = y_cal, y_test_pred = y_test_pred[1:10], interval_type = "two-tailed", tree = rep_tree, cal_data=cal_data, test_data = test_data, dependent_varname="carat"))
  expect_error(get_calibrated_predictive_system_mondrian(y_cal_pred = y_cal_pred, y_cal = "abc", y_test_pred = y_test_pred[1:10], interval_type = "two-tailed", tree = rep_tree, cal_data=cal_data, test_data = test_data, dependent_varname="carat"))
  expect_error(get_calibrated_predictive_system_mondrian(y_cal_pred = y_cal_pred, y_cal = y_cal, y_test_pred = "abc", interval_type = "two-tailed", tree = rep_tree, cal_data=cal_data, test_data = test_data, dependent_varname="carat"))
  expect_error(get_calibrated_predictive_system_mondrian(y_cal_pred = y_cal_pred, y_cal = y_cal, y_test_pred = y_test_pred[1:10], significance_level = "abc", interval_type = "two-tailed", tree = rep_tree, cal_data=cal_data, test_data = test_data, dependent_varname="carat"))
  expect_error(get_calibrated_predictive_system_mondrian(y_cal_pred = y_cal_pred, y_cal = y_cal, y_test_pred = y_test_pred[1:10], significance_level = 0.05, interval_type = "two", tree = rep_tree, cal_data=cal_data, test_data = test_data, dependent_varname="carat"))
  expect_error(get_calibrated_predictive_system_mondrian(y_cal_pred = y_cal_pred, y_cal = y_cal, y_test_pred = y_test_pred[1:10], significance_level = 0.05, interval_type = "two-tailed", tree = "rep_tree", cal_data=cal_data, test_data = test_data, dependent_varname="carat"))
  expect_error(get_calibrated_predictive_system_mondrian(y_cal_pred = y_cal_pred, y_cal = y_cal, y_test_pred = y_test_pred[1:10], significance_level = 0.05, interval_type = "one-tailed", direction = "left", tree = rep_tree, cal_data=cal_data, test_data = test_data, dependent_varname="carat"))
  expect_error(get_calibrated_predictive_system_mondrian(y_cal_pred = y_cal_pred, y_cal = y_cal, y_test_pred = y_test_pred[1:10], significance_level = 0.05, interval_type = "one-tailed", direction = "left-tailed", tree = "rep_tree", cal_data=cal_data, test_data = test_data, dependent_varname="carat"))
  expect_error(get_calibrated_predictive_system_mondrian(y_cal_pred = y_cal_pred, y_cal = y_cal, y_test_pred = y_test_pred[1:10], significance_level = 0.05, interval_type = "one-tailed", direction = "left-tailed", tree = rep_tree, cal_data="cal_data", test_data = test_data, dependent_varname="carat"))
  expect_error(get_calibrated_predictive_system_mondrian(y_cal_pred = y_cal_pred, y_cal = y_cal, y_test_pred = y_test_pred[1:10], significance_level = 0.05, interval_type = "one-tailed", direction = "left-tailed", tree = rep_tree, cal_data=cal_data, test_data = "test_data", dependent_varname="carat"))
  expect_error(get_calibrated_predictive_system_mondrian(y_cal_pred = y_cal_pred, y_cal = y_cal, y_test_pred = y_test_pred[1:10], significance_level = 0.05, interval_type = "one-tailed", direction = "left-tailed", tree = rep_tree, cal_data=cal_data, test_data = test_data, dependent_varname="carat123"))
})

## Test output
test_that("Test output", {
  expect_equal(class(calibrated_predictions), "data.frame")
  expect_equal(ncol(calibrated_predictions), 3)
})
