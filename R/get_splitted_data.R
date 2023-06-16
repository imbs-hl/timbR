#' Get for each node which observations of the inbag data reach it
#' @param tree_info_df Data frame containing information about the structure of the decision tree, which is built like a "treeInfo()" data frame from the package "ranger"
#' @param inbag_data_df        Data frame of the inbag data with which the random forest was trained
#' @param rf_list              Random forest, which is built like the one you get from ranger()
#' @param tree_number             Number of the decision tree of the rf_list to be displayed
#' @returns                    List containing for each node the inbag data reaching this node

get_splitted_data <- function(tree_info_df, inbag_data_df, rf_list, tree_number){

  # Create list with one entry per node
  splitted_data_list <- list()

  # All inbag observations start in root node
  splitted_data_list[[1]] <- inbag_data_df

  # Check inbag data for all other nodes
  for(node_id in 1:max(tree_info_df$nodeID)){
    split_variable <- tree_info_df$splitvarName[get_parent_id(tree_info_df, node_id)+1]
    split_variable_type <- class(inbag_data_df[,split_variable] %>% unlist)
    split_value <- tree_info_df$splitval[get_parent_id(tree_info_df, node_id)+1]
    if(split_variable_type == "character" | split_variable_type == "factor"){
      split_levels <- get_factor_split_levels(node_id = node_id,
                                              split_variable = split_variable,
                                              split_variable_type = split_variable_type,
                                              train_data_df = inbag_data_df,
                                              rf_list = rf_list,
                                              split_value = split_value)
      data_node_id <- splitted_data_list[[get_parent_id(tree_info_df, node_id)+1]]
      splitted_data_list[[node_id+1]] <- filter(data_node_id, grepl(gsub(",", "|", split_levels), unlist(data_node_id[,split_variable])))
    }else{
      data_node_id <- splitted_data_list[[get_parent_id(tree_info_df, node_id)+1]]
      if(node_id %% 2 == 0){
        splitted_data_list[[node_id+1]] <- data_node_id[data_node_id[split_variable] > split_value,]
      }else{
        splitted_data_list[[node_id+1]] <- data_node_id[data_node_id[split_variable] <= split_value,]
      }
    }
  }
  return(splitted_data_list)
}
