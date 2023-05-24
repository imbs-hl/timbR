#' Get ID of parent node 
#' @param tree_info_df Data frame containing information about the structure of the decision tree, which is built like a "treeInfo()" data frame from the package "ranger"
#' @param node_id      Node ID of the node whose parent ID is to be determined
#' @returns            Parent node ID (number)
#' @import checkmate
#' @import dplyr

get_parent_id <- function(tree_info_df, node_id){  
  ## Calculate output ----
  return(tree_info_df %>% filter(leftChild == node_id | rightChild == node_id) %>% select(nodeID) %>% unlist())
}