#' \code{get_calibrated_prediction_regression} calibrates regression predictions using conformal prediction
#' to a point prediction with prediction interval for each observation of the test data set.
#'
#' @title Calibration of predictions of regression model using conformal prediction
#'
#' @param y_cal_pred          Predictions of regression model for calibration set.
#' @param y_cal               True label of outcome variable for calibration set. Has to be in the same order as y_cal_pred.
#' @param y_test_pred         Predictions of test data set that should be calibrated.
#' @param uncertainty_level   Level of uncertainty that should be reached by calibration, should be between 0 and 1.
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
#'
#' # Train random forest with ranger
#' rf.iris <- ranger(Species ~ .,
#'                   data = iris,
#'                   write.forest=TRUE,
#'                   num.trees = 10,
#'                   importance = "permutation"
#'                   )
#'
#' # Calculate pair-wise distances for all trees
#' rep_tree <- generate_tree_reimplementation(rf = rf.iris, metric = "splitting variables", train_data = iris, dependent_varname = "Species", importance.mode = TRUE, imp.num.var = 2, min.bucket = 25)
#'
#' # Get predictions
#' rep_tree_predictions <- predict(rep_tree, iris)$predictions
#'
#' # Calibrated predictions
#' get_calibrated_prediction_regression(rep_tree_predictions, iris$Species)
#'

get_calibrated_prediction_regression <- function(y_cal_pred, y_cal, y_test_pred, uncertainty_level){
  # Conformal prediction: overall calibration of predictions

  # Calculate absolute errors of calibration set and sort them
  y_cal_abs_error <- sort(abs(y_cal_pred - y_cal), decreasing = T)

  # Find index of error term corresponding to uncertainty level and select it
  idx <- floor(uncertainty_level*(length(y_cal_abs_error)+1)) +1
  half_interval <- y_cal_abs_error[idx]

  # Calculate lower and upper bound of prediction interval
  upper_bound <- y_test_pred + half_interval
  lower_bound <- y_test_pred - half_interval

  return(data.frame(prediction = y_test_pred, lower_bound = lower_bound, upper_bound = upper_bound))
}
