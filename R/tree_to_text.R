#' Get latex code in the format of the package "forest" for the plot of the decision tree
#' @param node_id                 Node ID of the node whose parent ID is to be determined
#' @param tree_info_df            Data frame containing information about the structure of the decision tree, which is built like a "treeInfo()" data frame from the package "ranger"
#' @param train_data_df           Data frame of the training data with which the random forest was trained
#' @param rf_list                 Random forest, which is built like the one you get from ranger()
#' @param tree_number             Number of the decision tree of the rf_list to be displayed
#' @param dependent_var           Name of the column of the dependent variable in training data  
#' @param show_sample_size        Option to display percentage of observations that reach nodes during training (TRUE or FALSE)      
#' @param show_prediction_nodes   Option to display prediction in all nodes (TRUE or FALSE)
#' @param vert_sep                Vertical spacing of nodes in mm (parameter from Latex package "forest") 
#' @param hor_sep                 Horizontal spacing of nodes in mm (parameter from Latex package "forest") 
#' @returns                       Character pasted Latex code for the plot with the Latex package "forest"
tree_to_text <- function(node_id, tree_info_df, train_data_df, rf_list, tree_number, dependent_var, 
                         show_sample_size, show_prediction_nodes,
                         vert_sep, hor_sep, colors){
  
  l_sep <- paste0(vert_sep, "mm")
  s_sep <- paste0(hor_sep, "mm")
  
  sample_size <- ifelse(show_sample_size, 
                        paste0("\\\\",
                               get_observations_node(tree_info_df, train_data_df, rf_list, tree_number, node_id)),
                        "")
  
  color_node <- ifelse(is.null(colors), 
                   "white", 
                   colors[node_id+1])
  
  if(tree_info_df$terminal[node_id+1]){
    prediction_nodes <- ifelse(show_prediction_nodes,
                               get_prediction_terminal_node(tree_info_df, train_data_df, rf_list, dependent_var, tree_number, node_id),
                               "")
    leaf <- paste0("{", 
                   tree_info_df$prediction[node_id+1],
                   prediction_nodes, 
                   sample_size,
                   "},", 
                   "align=center,",
                   "node options={rounded corners},",
                   paste("fill=", color_node), ",",
                   paste("l=", l_sep),",",
                   paste("s sep=", s_sep),",",
                   "edge label={node[midway, above,font=\\scriptsize]{", 
                   get_split_criterion(tree_info_df, node_id, train_data_df, rf_list), "}}")
    return(leaf)
  }
  prediction_nodes <- ifelse(show_prediction_nodes,
                             paste0("\\\\",
                                    get_prediction_node(tree_info_df, train_data_df, rf_list, dependent_var, tree_number, node_id)),
                             "")
  
  node <- paste0("{", tree_info_df$splitvarName[node_id+1], 
                 prediction_nodes,
                 sample_size,
                 "},", 
                 "align=center,",
                 paste("l=", l_sep),",",
                 paste("s sep=", s_sep),",",
                 paste("fill=", color_node), ",",
                 "edge label={node[midway,above,font=\\scriptsize]{", 
                 get_split_criterion(tree_info_df, node_id, train_data_df, rf_list), "}}",
                 "[",
                 tree_to_text(tree_info_df$leftChild[node_id+1], tree_info_df, train_data_df, rf_list, tree_number, dependent_var,
                              show_sample_size, show_prediction_nodes,
                              vert_sep, hor_sep, colors), 
                 "]",
                 "[",
                 tree_to_text(tree_info_df$rightChild[node_id+1], tree_info_df, train_data_df, rf_list, tree_number, dependent_var, 
                              show_sample_size, show_prediction_nodes,
                              vert_sep, hor_sep, colors),
                 "]")
  return(node)
}