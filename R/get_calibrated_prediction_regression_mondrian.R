get_calibrated_prediction_regression_mondrian <- function(y_cal_pred, y_cal, y_test_pred, uncertainty_level, tree, cal_data, test_data, dependent_varname, calibrate_all_nodes = FALSE, show_leaf_id = FALSE){
  source("functions/get_calibrated_nodes.R")
  
  # Node wise calibration (of a ranger forest object for first tree)
  # If calibrate_all_nodes = FALSE only for terminal nodes otherwise for all nodes
  if(!calibrate_all_nodes){
    
    calibrated_predictions <- get_calibrated_nodes(y_cal_pred, y_cal, y_test_pred, uncertainty_level, tree, cal_data, test_data, dependent_varname, calibrate_all_nodes)

    # Get reached terminal node for every observation of test data (+1 because IDs start with 0)
    leaf_ids_test <- suppressWarnings(getTerminalNodeIDs(tree, test_data)) +1
    
    # Merge calibrated predictions interval to predicted values
    prediction_df <- data.frame(prediction = y_test_pred, leaf = leaf_ids_test) %>% 
      left_join(calibrated_predictions) 
    
    if(show_leaf_id){
      return(prediction_df %>% select(prediction, lower_bound, upper_bound, leaf))
    }else{
      return(prediction_df %>% select(prediction, lower_bound, upper_bound))
    }
  }

}