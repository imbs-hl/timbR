#' \code{get_calibrated_prediction_regression_mondrian} calibrates regression predictions using conformal prediction
#' to a point prediction with prediction interval for each observation of the test data set.
#'
#' @title Node-wise calibration of predictions of decision tree using mondrian conformal prediction
#'
#' @param y_cal_pred          Predictions of decision tree for calibration set.
#' @param y_cal               True label of outcome variable for calibration set. Has to be in the same order as y_cal_pred.
#' @param y_test_pred         Predictions of test data set that should be calibrated.
#' @param significance_level  Level of uncertainty that should be reached by calibration, should be between 0 and 1.
#' @param tree                Tree whose predictions are to be calibrated, tree should be an object of class \code{ranger}.
#' @param cal_data            Data frame with calibration data (should contain same variables as train data).
#' @param test_data           Data frame with test data (should contain same variables as train data).
#' @param dependent_varname   Name of the dependent variable used to create the tree.
#' @param show_node_id        If true the ID of the terminal nodes of each prediction is returned as column in returned data frame.
#'
#' @author Lea Louisa Kronziel, M.Sc.
#' @return
#'   \item{data frame with point prediction and lower and upper bound of prediction interval.}
#' @import ranger
#' @import dplyr
#' @import checkmate
#' @import data.table
#'
#' @export get_calibrated_prediction_regression_mondrian
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
#' rep_tree <- generate_tree(rf = rf, metric = "splitting variables", train_data = regr_data,
#'                                            dependent_varname = "Employed", importance.mode = TRUE, imp.num.var = 2)
#'
#' # Get predictions
#' rep_tree_predictions <- predict(rep_tree, regr_data)$predictions
#'
#' # Calibrated predictions
#' get_calibrated_prediction_regression_mondrian(y_cal_pred = rep_tree_predictions, y_cal = regr_data$Employed,
#'                                               y_test_pred = rep_tree_predictions, significance_level = 0.05,
#'                                               tree = rep_tree, cal_data = regr_data, test_data = regr_data,
#'                                               dependent_varname = "Employed")
#'



get_calibrated_prediction_regression_mondrian <- function(y_cal_pred, y_cal, y_test_pred, significance_level, tree, cal_data, test_data, dependent_varname, show_node_id = FALSE){
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
    stop("significance_level has to be a single numerical value.")
  }
  if(significance_level>=1 | significance_level<=0){
    stop("significance_level has to be smaller than 1 and bigger than 0.")
  }
  if (!checkmate::testClass(tree, "ranger")){
    stop("rf must be of class ranger")
  }
  if(!all(colnames(cal_data) %in% colnames(test_data))){
    stop("All columns of calibration data have to be test data.")
  }
  if(!all(colnames(test_data) %in% colnames(cal_data))){
    stop("All columns of test data have to be calibration data.")
  }
  if(!is.logical(show_node_id)){
    stop("show_node_id has to be TRUE or FALSE.")
  }
  if(!dependent_varname%in%colnames(cal_data)){
    stop("depedent_varname has to be a variable name from calibration data")
  }

  # Get node wise calibration
  calibrated_predictions <- get_calibrated_nodes(y_cal_pred, y_cal, y_test_pred, significance_level, tree, cal_data, test_data, dependent_varname)

  # Get reached terminal node for every observation of test data (+1 because IDs start with 0)
  leaf_ids_test <- suppressWarnings(getTerminalNodeIDs(tree, test_data)) +1

  # Merge calibrated predictions interval to predicted values
  prediction_df <- data.frame(prediction = y_test_pred, leaf = leaf_ids_test) %>%
    left_join(calibrated_predictions, by = c("prediction", "leaf"))

  # Add terminal node ID to returned data frame if wanted
  if(show_node_id){
    return(prediction_df %>% select(prediction, lower_bound, upper_bound, leaf))
  }else{
    return(prediction_df %>% select(prediction, lower_bound, upper_bound))
  }


}
