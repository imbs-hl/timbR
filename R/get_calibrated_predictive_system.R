#' \code{get_calibrated_predictive_system} calibrates regression predictions using conformal predictive systems
#' to a point prediction with prediction one- or two-tailed interval for each observation of the test data set.
#'
#' @title Calibration of predictions of regression model using conformal predictive systems
#'
#' @param y_cal_pred          Predictions of regression model for calibration set.
#' @param y_cal               True label of outcome variable for calibration set. Has to be in the same order as y_cal_pred.
#' @param y_test_pred         Predictions of test data set that should be calibrated.
#' @param significance_level  Level of uncertainty that should be reached by calibration, should be between 0 and 1.
#' @param interval_type       Type of interval, choose either two-tailed or one-tailed
#' @param direction           Direction of one-tailed interval, choose either left-tailed or right-tailed
#'
#' @author Lea Louisa Kronziel, M.Sc.
#' @return
#'   \item{data frame with point prediction and lower and upper bound of prediction interval.}
#' @import ranger
#' @import dplyr
#' @import checkmate
#' @import data.table
#'
#' @export get_calibrated_predictive_system
#'
#' @examples
#' require(ranger)
#' require(timbR)
#' require(dplyr)
#' require(ggplot2)
#'
#' # Get train, test and calibration data
#' regr_data <- diamonds %>% data.frame()
#' train_data <- regr_data[1:17980,]
#' test_data <- regr_data[17981:35960,]
#' cal_data <- regr_data[35961:53940,]
#'
#' # Train random forest with ranger
#' rf <- ranger(carat ~ ., data = train_data, num.trees = 10, importance = "permutation")
#'
#' # Calculate pair-wise distances for all trees
#' rep_tree <- generate_tree(rf = rf, metric = "splitting variables", train_data = train_data, dependent_varname = "carat", importance.mode = TRUE, imp.num.var = 2, probs_quantiles = c(0.25,0.5,0.75), num.splits = 5)
#'
#' # Get predictions
#' y_test_pred <- predict(rep_tree, test_data)$predictions
#' y_cal_pred <- predict(rep_tree, cal_data)$predictions
#' y_cal <- cal_data$carat
#'
#' # Calibrated predictions
#' get_calibrated_predictive_system(y_cal_pred = y_cal_pred, y_cal = y_cal, y_test_pred = y_test_pred[1:10], significance_level = 0.05,interval_type = "two-tailed")
#'



get_calibrated_predictive_system <- function(y_cal_pred, y_cal, y_test_pred, significance_level,interval_type = "two-tailed", direction = NULL){

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
  if(length(y_cal_pred) != length(y_cal)){
    stop("The vectors y_cal_pred and y_cal must have the same length.")
  }
  if(interval_type != "two-tailed" & interval_type != "one-tailed"){
    stop("Please use 'two-tailed' or 'one-tailed' for interval type.")
  }
  if(interval_type == "one-tailed" & is.null(direction)){
    stop("Please use 'left-tailed' or 'right-tailed' for direction.")
  }
  if(interval_type == "one-tailed"){
    if(is.na(direction)){
      stop("Please use 'left-tailed' or 'right-tailed' for direction.")
    }
    if(direction != "left-tailed" & direction != "right-tailed"){
      stop("Please use 'left-tailed' or 'right-tailed' for direction.")
    }
  }


  # Get cummulative predictive distribution (cpd)
  cpds <- get_predictive_distribution(y_cal_pred = y_cal_pred, y_cal = y_cal, y_test_pred = y_test_pred)

  # Get prediction interval
  cpd_list <- mapply(cpds, y_test_pred, SIMPLIFY = FALSE,
                     FUN = function(cpd, point_pred){
                       p <- seq(0, 1 - 1/length(cpd), length.out = length(cpd))

                      # two tailed interval
                       if(interval_type == "two-tailed"){
                         # Get Index of interval bounds and select them from cpd
                         lower_index <- tail(which(p <= significance_level/2), 1)
                         upper_index <- which(p >= 1-significance_level/2)[1]

                         # if lower_index < 1 or upper_index > length of cpd than -inf or + inf are returned
                         if(lower_index < 1){
                           lower_bound <- -Inf
                           warning('Calibration data set has too less observations for that signifcance level, so -Inf used for lower bound of the interval.')
                         }else{
                           lower_bound <- cpd[lower_index]
                         }
                         if(is.na(upper_index)){
                           upper_bound <- Inf
                           warning('Calibration data set has too less observations for that signifcance level, so Inf used for upper bound of the interval.')
                         }else{
                           upper_bound <- cpd[upper_index]
                         }
                      # left-tailed interval
                       }else{
                         if(direction == "left-tailed"){
                           # Get Index of interval bounds and select them from cpd
                           lower_index <- tail(which(p <= significance_level), 1)

                           lower_bound <- cpd[lower_index]
                           upper_bound <- Inf
                      # right tailed interval
                         }else{
                           # Get Index of interval bounds and select them from cpd
                           upper_index <- which(p >= 1-significance_level)[1]
                           if(is.na(upper_index)){
                             upper_bound <- Inf
                             warning('Calibration data set has too less observations for that signifcance level, so Inf used for upper bound of the interval.')
                           }else{
                             upper_bound <- cpd[upper_index]
                           }

                           lower_bound <- -Inf
                         }
                       }
                       return(data.frame(prediction = point_pred, lower_bound = lower_bound, upper_bound = upper_bound))
                     }) %>% bind_rows()
  return(cpd_list)
}




