## Load libraries
library(timbR)
library(ranger)
library(testthat)
library(ggplot2)

## Prepare inputs
# Get train, test and calibration data (each 1/3)
regr_data <- diamonds %>% data.frame()
train_data <- regr_data[1:17980,]
test_data <- regr_data[17981:17991,] # save runtime and use only 10 observations
cal_data <- regr_data[35961:53940,]

# Train random forest with ranger
rf <- ranger(carat ~ ., data = train_data, num.trees = 10, importance = "permutation")

# Calculate pair-wise distances for all trees
rep_tree <- generate_tree(rf = rf, metric = "splitting variables", train_data = train_data, dependent_varname = "carat", importance.mode = TRUE, imp.num.var = 5, probs_quantiles = c(0.25,0.5,0.75), num.splits = 3)

# Get predictions
y_test_pred <- predict(rep_tree, test_data)$predictions
y_cal_pred <- predict(rep_tree, cal_data)$predictions
y_cal <- cal_data$carat

cps <- get_predictive_distribution(y_cal_pred = y_cal_pred, y_cal = y_cal, y_test_pred = y_test_pred)

## Test correct input ----
test_that("Test correct input", {
  expect_silent(get_predictive_distribution(y_cal_pred = y_cal_pred, y_cal = y_cal, y_test_pred = y_test_pred))
  })


## Test incorrect inputs ----
test_that("Test incorrect inputs", {
  expect_error(get_predictive_distribution(y_cal_pred = "abc", y_cal = y_cal, y_test_pred = y_test_pred))
  expect_error(get_predictive_distribution(y_cal_pred = y_cal_pred, y_cal = "abc", y_test_pred = y_test_pred))
  expect_error(get_predictive_distribution(y_cal_pred = y_cal_pred, y_cal = y_cal, y_test_pred = "abc"))
})

## Test output
test_that("Test output", {
  expect_equal(class(cps), "list")
})
