#' Get split criterion (levels) for factor, character or numerical variables
#' @param tree_info_df Data frame containing information about the structure of the decision tree, which is built like a "treeInfo()" data frame from the package "ranger"
#' @param node_id              Node ID of the node whose parent ID is to be determined
#' @param train_data_df        Data frame of the training data with which the random forest was trained
#' @param rf_list              Random forest, which is built like the one you get from ranger()
#' @author Lea Louisa Kronziel, M.Sc.
#' @returns                    Character with levels or values passed from the parent node to the considered node

get_split_criterion <- function(tree_info_df, node_id, train_data_df, rf_list){
  # Get split value (saved in row of parent node)
  split_value <- tree_info_df$splitval[get_parent_id(tree_info_df, node_id)+1]

  # Get split variable and it's type (saved in row of parent node)
  split_variable <- tree_info_df$splitvarName[get_parent_id(tree_info_df, node_id)+1]
  split_variable_type <- class(train_data_df[,split_variable] %>% unlist)

  # Consider three possible split variants for factors and character variables
  if(split_variable_type == "character" | split_variable_type == "factor"){
    return(get_factor_split_levels(node_id = node_id, split_variable = split_variable, split_variable_type = split_variable_type, split_value = split_value,
                                   train_data_df = train_data_df, rf_list = rf_list))
  }else{
    # For numeric variables: if the ID is even, it is the right child node; if it is odd, it is the left child node.
    split_sign <- ifelse(node_id %% 2 == 0, ">", "\\leq")
    return(paste0("$", split_sign, round(as.numeric(split_value), digits = 2), "$"))
  }
}
