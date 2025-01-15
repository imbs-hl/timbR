get_calibrated_nodes <- function(y_cal_pred, y_cal, y_test_pred, uncertainty_level, tree, cal_data, test_data, dependent_varname, calibrate_all_nodes = FALSE){
  if(!calibrate_all_nodes){
    # Get ids of terminal node (leaf) of tree (counting starts with 1)
    leaf_ids <- which(treeInfo(tree)$terminal)
    
    # Get reached terminal node for every observation of test and calibration data (+1 because IDs start with 0)
    leaf_ids_test <- suppressWarnings(getTerminalNodeIDs(tree, test_data)) +1
    leaf_ids_cal <- suppressWarnings(getTerminalNodeIDs(tree, cal_data)) +1
    
    # Get calibrated prediction node wise
    calibrated_leafs <- mapply(function(id, y_cal_pred, y_cal){
      # Prediction of all calibration and test observtions ending in that node
      y_cal_pred_node <- y_cal_pred[leaf_ids_cal == id]
      y_test_pred_node <- y_test_pred[leaf_ids_test == id]
      
      # True label of all calibration observtions ending in that node
      y_cal_node <- y_cal[leaf_ids_cal == id]
      
      # Calculate absolute errors of calibration set and sort them
      y_cal_node_abs_error <- sort(abs(y_cal_pred_node - y_cal_node), decreasing = T)
      
      # Find index of error term corresponding to uncertainty level and select it 
      idx <- floor(uncertainty_level*(length(y_cal_node_abs_error)+1)) +1
      half_interval <- y_cal_node_abs_error[idx] 
      
      # calculate bounds
      lower_bound <- y_cal_pred_node[1] - half_interval
      upper_bound <- y_cal_pred_node[1] + half_interval
      
      # return prediction of node with leaf ID and half interval
      return(data.frame(leaf = id, prediction = y_cal_pred_node[1], lower_bound = lower_bound, upper_bound = upper_bound))
      
    }, 
    id = leaf_ids, 
    MoreArgs = list(y_cal_pred = y_cal_pred,
                    y_cal = y_cal),
    SIMPLIFY = TRUE)
    
    calibrated_leafs <- data.frame(leaf = unlist(calibrated_leafs[1,]),
                                   prediction = unlist(calibrated_leafs[2,]),
                                   lower_bound = unlist(calibrated_leafs[3,]),
                                   upper_bound = unlist(calibrated_leafs[4,]))
    
  }else{
    # all IDs are used
    tree_info <- treeInfo(tree)
    leaf_ids <- 1:(max(tree_info$nodeID)+1)
    
    # data reaching every node
    train_data_node <- timbR:::get_splitted_data(tree_info, train_data, tree, 1)
    cal_data_node <- timbR:::get_splitted_data(tree_info, cal_data, tree, 1)
    
    for(node in 1:max(leaf_ids)){
      # prediction of node (test data)
      pred_node <- mean(unlist(train_data_node[[node]][dependent_varname]))
      tree_info$pred[node] <- pred_node
      
      # calculate bounds with calibration data
      y_cal_pred_node <- pred_node
      y_cal_node <- unlist(cal_data_node[[node]][dependent_varname])
      # sorted alphas for that node
      y_cal_node_abs_error = sort(abs(y_cal_pred_node - y_cal_node), decreasing = T)
      # Find index of element corresponding to epsilon 
      idx <- floor(uncertainty_level*(length(y_cal_node_abs_error)+1)) +1
      # Pick the element 
      half_interval <- y_cal_node_abs_error[idx] 
      # add it to test data ending in that node
      tree_info <- tree_info %>% 
        mutate(lower_bound = ifelse((nodeID+1) == node, pred_node - half_interval, lower_bound),
               upper_bound = ifelse((nodeID+1) == node, pred_node + half_interval, upper_bound))
    }
    calibrated_leafs <- tree_info %>% 
      rename(leaf = nodeID,
             prediction = pred) %>% 
      select(leaf, prediction, lower_bound, upper_bound)
  }
  


  
  return(calibrated_leafs)

}