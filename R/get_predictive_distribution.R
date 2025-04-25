#' \code{get_predictive_distribution} calculates cummulative predictive distribution using conformel predictive systems
#' to a point prediction with prediction interval for each observation of the test data set.
#'
#' @title Calculation of cummulative predictive distribution (cpd)
#'
#' @param y_cal_pred          Predictions of regression model for calibration set.
#' @param y_cal               True label of outcome variable for calibration set. Has to be in the same order as y_cal_pred.
#' @param y_test_pred         Predictions of test data set that should be calibrated.
#'
#' @author Lea Louisa Kronziel, M.Sc.
#' @return
#'   \item{list with one cpd for each test observation
#' @import ranger
#' @import dplyr
#' @import checkmate
#' @import data.table
#'
#' @export get_predictive_distribution
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
#' rep_tree <- generate_tree(rf = rf, metric = "splitting variables", train_data = train_data, dependent_varname = "carat", importance.mode = TRUE, imp.num.var = 5, probs_quantiles = c(0.25,0.5,0.75), num.splits = 3)
#'
#' # Get predictions
#' y_test_pred <- predict(rep_tree, test_data)$predictions
#' y_cal_pred <- predict(rep_tree, cal_data)$predictions
#' y_cal <- cal_data$carat
#'
#' # Get cummulative distributions for all test observations
#' cpds <- get_predictive_distribution(y_cal_pred = y_cal_pred, y_cal = y_cal, y_test_pred = y_test_pred)
#'
#' # Plot distribution of first observation
#' cpd <- cpds[[1]]
#' y <- seq(0, 1 - 1/length(cpd), length.out = length(cpd))
#'
#' ggplot(mapping = aes(y=y, x = cpd)) + geom_line()
#'

get_predictive_distribution <- function(y_cal_pred, y_cal, y_test_pred){

  # Check input
  if(is.null(y_cal_pred)){
    stop("y_cal_pred has to be a vector and not NULL")
  }
  if(is.null(y_cal)){
    stop("y_cal has to be a vector and not NULL")
  }
  if(is.null(y_test_pred)){
    stop("y_test_pred has to be a vector and not NULL")
  }
  if(!is.numeric(y_cal_pred)){
    stop("Predictions of calibration data have to be numeric vector.")
  }
  if(!is.numeric(y_cal)){
    stop("Values of dependent variable in calibration data have to be numeric vector.")
  }
  if(!is.numeric(y_test_pred)){
    stop("Predictions of test data have to be numeric vector.")
  }
  if(length(y_cal_pred) != length(y_cal)){
    stop("The vectors y_cal_pred and y_cal must have the same length.")
  }

  # Calculate errors of calibration set and sort them
  y_cal_error <- sort(y_cal_pred - y_cal)

  # One cumulative predictive distribution (cpd) for each observation of test data (saved as list)
  cpds <- lapply(y_test_pred, function(y_test_observation)y_test_observation + y_cal_error)

  # Return list with one entry per observation
  return(cpds)
}
