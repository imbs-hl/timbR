#' Get number of observations of the inbag data that reach respective node
#' @param tree_info_df Data frame containing information about the structure of the decision tree, which is built like a "treeInfo()" data frame from the package "ranger"
#' @param train_data_df        Data frame of the training data with which the random forest was trained
#' @param rf_list              Random forest, which is built like the one you get from ranger()
#' @param tree_number          Number of the decision tree of the rf_list to be displayed
#' @param node_id              Node ID of the node whose parent ID is to be determined
#' @returns                    Character with the number of observations reaching this node

get_observations_node <- function(tree_info_df, train_data_df, rf_list, tree_number, node_id){
  # Get inbag data
  inbag_data_df <- get_inbag_data(rf_list, train_data_df, tree_number)
  
  # Get splitted data
  splitted_data_list <- get_splitted_data(tree_info_df, inbag_data_df, rf_list, tree_number)
  
  # Number of observations in current node
  number_observations <- splitted_data_list[[node_id+1]] %>% nrow()
  
  # Percentage of observations
  percentage_observations <- round(number_observations/nrow(inbag_data_df)*100)
  
  return(paste0("n = ", number_observations, " (", percentage_observations, "\\%)"))
}
