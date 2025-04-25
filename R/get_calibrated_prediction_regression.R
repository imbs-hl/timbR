#' \code{get_calibrated_prediction_regression} calibrates regression predictions using conformal prediction
#' to a point prediction with prediction interval for each observation of the test data set.
#'
#' @title Calibration of predictions of regression model using conformal prediction
#'
#' @param y_cal_pred          Predictions of regression model for calibration set.
#' @param y_cal               True label of outcome variable for calibration set. Has to be in the same order as y_cal_pred.
#' @param y_test_pred         Predictions of test data set that should be calibrated.
#' @param significance_level   Level of uncertainty that should be reached by calibration, should be between 0 and 1.
#'
#' @author Lea Louisa Kronziel, M.Sc.
#' @return
#'   \item{data frame with point prediction and lower and upper bound of prediction interval.}
#' @import ranger
#' @import dplyr
#' @import checkmate
#' @import data.table
#'
#' @export get_calibrated_prediction_regression
#'
#' @examples
#' require(ranger)
#' require(timbR)
#' require(dplyr)
#'
#' regr_data <- longley %>% data.frame()
#' # Train random forest with ranger
#' rf <- ranger(Employed ~ ., data = regr_data, num.trees = 10, importance = "permutation")
#'
#' # Calculate pair-wise distances for all trees
#' rep_tree <- generate_tree(rf = rf, metric = "splitting variables", train_data = regr_data, dependent_varname = "Employed", importance.mode = TRUE, imp.num.var = 2)
#'
#' # Get predictions
#' rep_tree_predictions <- predict(rep_tree, regr_data)$predictions
#'
#' # Calibrated predictions
#' get_calibrated_prediction_regression(y_cal_pred = rep_tree_predictions, y_cal = regr_data$Employed, y_test_pred = rep_tree_predictions, significance_level = 0.05)
#'


get_calibrated_prediction_regression <- function(y_cal_pred, y_cal, y_test_pred, significance_level){

  # Check input
  if(!is.numeric(y_cal_pred)){
    stop("Predictions of calibration data have to be numeric vector.")
  }
  if(!is.numeric(y_cal)){
    stop("Values of dependent variable in calibration data have to be numeric vector.")
  }
  if(!is.numeric(y_test_pred)){
    stop("Predictions of test data have to be numeric vector.")
  }
  if(!is.numeric(significance_level) | length(significance_level) != 1){
    stop("significance_level has to be a single numerical value")
  }
  if(significance_level>=1 | significance_level<=0){
    stop("significance_level has to be smaller than 1 and bigger than 0.")
  }

  # Calculate absolute errors of calibration set and sort them
  y_cal_abs_error <- sort(abs(y_cal_pred - y_cal), decreasing = T)

  # Find index of error term corresponding to uncertainty level and select it
  idx <- floor(significance_level*(length(y_cal_abs_error)+1)) +1
  half_interval <- y_cal_abs_error[idx]

  # Calculate lower and upper bound of prediction interval
  upper_bound <- y_test_pred + half_interval
  lower_bound <- y_test_pred - half_interval

  # Warning if calibration data is too small for selected significance level
  if(any(is.na(lower_bound)|is.na(upper_bound))){
    stop("Your calibration data is too small resulting in NA in prediction bounds.")
  }

  return(data.frame(prediction = y_test_pred, lower_bound = lower_bound, upper_bound = upper_bound))
}
