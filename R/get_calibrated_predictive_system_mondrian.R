#' \code{get_calibrated_predictive_system_mondrian} calibrates regression predictions using Mondrian conformal predictive systems
#' to a point prediction with prediction one- or two-tailed interval for each observation of the test data set.
#'
#' @title Calibration of predictions of regression model using Mondrian conformal predictive systems
#'
#' @param y_cal_pred          Predictions of regression model for calibration set.
#' @param y_cal               True label of outcome variable for calibration set. Has to be in the same order as y_cal_pred.
#' @param y_test_pred         Predictions of test data set that should be calibrated.
#' @param significance_level  Level of uncertainty that should be reached by calibration, should be between 0 and 1.
#' @param interval_type       Type of interval, choose either two-tailed or one-tailed
#' @param direction           Direction of one-tailed interval, choose either left-tailed or right-tailed
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
#' @export get_calibrated_predictive_system_mondrian
#'
#' @examples
#' require(ranger)
#' require(timbR)
#' require(dplyr)
#' require(ggplot2)
#'
#' # Get train, test and calibration data
#' set.seed(1)
#' regr_data <- diamonds %>% data.frame()
#' train_data <- regr_data[1:17980,]
#' test_data <- regr_data[17981:18000,]
#' cal_data <- regr_data[35961:53940,]
#'
#' # Train random forest with ranger
#' rf <- ranger(carat ~ ., data = train_data, num.trees = 10, importance = "permutation")
#'
#' # Calculate pair-wise distances for all trees
#' rep_tree <- generate_tree(rf = rf, metric = "splitting variables", train_data = train_data, dependent_varname = "carat", importance.mode = TRUE, imp.num.var = 2, probs_quantiles = c(0.25,0.5,0.75), num.splits = 4)
#'
#' # Get predictions
#' y_test_pred <- predict(rep_tree, test_data)$predictions
#' y_cal_pred <- predict(rep_tree, cal_data)$predictions
#' y_cal <- cal_data$carat
#'
#' # Calibrated predictions
#' get_calibrated_predictive_system_mondrian(y_cal_pred = y_cal_pred, y_cal = y_cal, y_test_pred = y_test_pred[1:10], significance_level = 0.05,interval_type = "two-tailed", direction = NULL, tree = rep_tree, cal_data=cal_data, test_data = test_data, dependent_varname="carat")
#'




get_calibrated_predictive_system_mondrian <- function(y_cal_pred, y_cal, y_test_pred, significance_level,interval_type = "two-tailed", direction = NULL, tree, cal_data, test_data, dependent_varname, show_node_id = FALSE){
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


  # Get ids of terminal node (leaf) of tree (counting starts with 1)
  leaf_ids <- which(treeInfo(tree)$terminal)

  # Get reached terminal node for every observation of test and calibration data (+1 because IDs start with 0)
  leaf_ids_test <- suppressWarnings(getTerminalNodeIDs(tree, test_data)) +1
  leaf_ids_cal <- suppressWarnings(getTerminalNodeIDs(tree, cal_data)) +1

  # Get calibrated prediction node wise
  calibrated_leafs <- mapply(function(id, y_cal_pred, y_cal, interval_type, direction){
    # Prediction of all calibration and test observtions ending in that node
    y_cal_pred_node <- y_cal_pred[leaf_ids_cal == id]
    y_test_pred_node <- y_test_pred[leaf_ids_test == id]

    # True label of all calibration observtions ending in that node
    y_cal_node <- y_cal[leaf_ids_cal == id]

    # Calculate errors of calibration set and sort them
    y_cal_error <- sort(y_cal_pred - y_cal)

    # One cumulative predictive distribution (cpd) for each observation of test data (saved as list)
    cpd_list <- suppressWarnings(get_calibrated_predictive_system(y_cal_pred = y_cal_pred_node, y_cal = y_cal_node, y_test_pred = y_test_pred_node[1],
                                                 significance_level = significance_level, interval_type = interval_type, direction = direction))

    # return prediction of node with leaf ID and half interval
    return(data.frame(leaf = id, prediction = y_cal_pred_node[1], lower_bound = cpd_list$lower_bound, upper_bound = cpd_list$upper_bound))

  },
  id = leaf_ids,
  MoreArgs = list(y_cal_pred = y_cal_pred,
                  y_cal = y_cal,
                  interval_type = interval_type,
                  direction = direction),
  SIMPLIFY = TRUE)

  calibrated_leafs <- data.frame(leaf = unlist(calibrated_leafs[1,]),
                                 prediction = unlist(calibrated_leafs[2,]),
                                 lower_bound = unlist(calibrated_leafs[3,]),
                                 upper_bound = unlist(calibrated_leafs[4,]))


  # Merge calibrated predictions interval to predicted values
  prediction_df <- left_join(data.frame(leaf = leaf_ids_test), calibrated_leafs, by = "leaf")

  if(interval_type == "two-tailed" & any(prediction_df$upper_bound == Inf)){
    warning("At least one leaf was not reached by enough calibration observations resulting in Inf als interval bound.")
  }

  # Add terminal node ID to returned data frame if wanted
  if(show_node_id){
    return(prediction_df %>% select(prediction, lower_bound, upper_bound, leaf))
  }else{
    return(prediction_df %>% select(prediction, lower_bound, upper_bound))
  }

}
