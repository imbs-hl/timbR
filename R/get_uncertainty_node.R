#' Get uncertainty quantification for terminal nodes (only for regression tree)
#' @param tree_info_df         Data frame containing information about the structure of the decision tree, which is built like a "treeInfo()" data frame from the package "ranger"
#' @param rf_list              Random forest, which is built like the one you get from ranger()
#' @param test_data_df         Data frame of the test data
#' @param tree_number          Number of the decision tree of the rf_list to be displayed
#' @param node_id              Node ID of the node whose parent ID is to be determined
#' @param dependent_var        Name of the dependent variable used to create the forest
#' @param show_coverage        Option to display marginal coverage
#' @author Lea Louisa Kronziel, M.Sc.
#' @returns                    Character with the number of observations reaching this node

get_uncertainty_node <- function(tree_info_df, rf_list, test_data_df, tree_number, node_id, dependent_var, show_coverage = TRUE){
  if(rf_list$treetype == "Regression"){
    # Check if lower and upper bound exist
    if(is.null(tree_info_df$lower_bound) | is.null(tree_info_df$upper_bound)){
      stop("lower_bound and upper_bound must be present in tree_info_df for picturing uncertainty")
    }
    # Only for terminal nodes
    if(!tree_info_df$terminal[node_id+1]){
      return("")
    }
    # Bounds must be numeric
    lower_bound <- tree_info_df %>% filter(nodeID == node_id) %>% select(lower_bound) %>% unlist()
    upper_bound <- tree_info_df %>% filter(nodeID == node_id) %>% select(upper_bound) %>% unlist()
    if(!is.numeric(lower_bound) | !is.numeric(upper_bound)){
      stop("lower and upper bound of terminal nodes have to be numeric")
    }

    # Prediction interval
    pred_interval <- paste0("[", lower_bound, ", ", upper_bound, "]")

    if(show_coverage){
      # Get observations in current node
      splitted_data_list <- timbR:::get_splitted_data(tree_info_df, test_data_df, rf_list, tree_number)
      node_observations <- splitted_data_list[[node_id+1]]

      # Calculate marginal coverage
      marginal_coverage <- paste0(round(sum(node_observations[,dependent_var] >= lower_bound &
                                              node_observations[,dependent_var] <= upper_bound)/nrow(node_observations)*100, 2), "\\%",
                                   "\\\\ $n_{test}$ = ", nrow(node_observations)
                                  )

    }else{
      marginal_coverage <- ""
    }
    # Marginal coverage

    return(paste0(pred_interval, "\\\\ coverage = ", marginal_coverage))
  }else{
    stop("uncertainty only available for regression for now")
  }

}
