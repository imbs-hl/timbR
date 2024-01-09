#' Get split criterion (levels) for factor or character variables
#' Character variables are automatically turned into factors in function ranger() from the package "ranger"
#' Considers the three possibilities to treat and split factors (Parameter "respect.unordered.factors" in ranger()):
#' - Treat the factor like an ordinal variable ("none", "NULL" or "FALSE" in ranger())
#' - Treat the factor as nominal and check all 2-partitions for split ("partition")
#' - Treat factor as ordinal and determine optimal order of levels for the random forest ("order", "TRUE" in ranger())
#' @param node_id              Node ID of the node whose parent ID is to be determined
#' @param split_variable       Column name of the split variable in training data
#' @param split_variable_type  Type of split variable: "factor" for factors, "character" for character variable
#' @param split_value          Number(s) used for the split, for example the entry "splival" from the function treeInfo()
#' @param train_data_df        Data frame of the training data with which the random forest was trained
#' @param rf_list              Random forest, which is built like the one you get from ranger()
#' @returns                    Character with levels passed from the parent node to the considered node

get_factor_split_levels <- function(node_id, split_variable, split_variable_type, split_value, train_data_df, rf_list){
  # Treat the factor like an ordinal variable
  if(is.null(rf_list$call$respect.unordered.factors)){
    if(split_variable_type == "factor"){
      split_variable_levels <- levels(unlist(train_data_df[,split_variable]))
    }else{
      # Character variables are automatically turned into factors in ranger()
      split_variable_levels <- levels(factor(unlist(train_data_df[,split_variable])))
    }
  }
  # Treat the factor as nominal and check all 2-partitions for split
  else if(rf_list$call$respect.unordered.factors == "partition"){
    if(split_variable_type == "factor"){
      split_variable_levels <- levels(unlist(train_data_df[,split_variable]))
    }else{
      # Character variables are automatically turned into factors in ranger()
      split_variable_levels <- levels(factor(unlist(train_data_df[,split_variable])))
    }
    # All comma separated numbers go to the right child (see manual of "ranger")
    right_node_numbers <- c(str_extract_all(split_value, "\\d", simplify = T)) %>%
      as.numeric()
    right_node <- split_variable_levels[right_node_numbers]
    if(node_id %% 2 == 0){
      return(paste(right_node, collapse=","))
    }else{
      left_node <- setdiff(split_variable_levels, right_node)
      return(paste(left_node, collapse=","))
    }
  }else if(rf_list$call$respect.unordered.factors == "ignore" |
           rf_list$call$respect.unordered.factors == FALSE){
    if(split_variable_type == "factor"){
      split_variable_levels <- levels(unlist(train_data_df[,split_variable]))
    }else{
      # Character variables are automatically turned into factors in ranger()
      split_variable_levels <- levels(factor(unlist(train_data_df[,split_variable])))
    }
  }
  # Treat factor as ordinal and determine optimal order of levels for the random forest
  else if(rf_list$call$respect.unordered.factors == "order" |
          rf_list$call$respect.unordered.factors == TRUE){
    split_variable_levels <- rf_list$forest$covariate.levels[split_variable] %>% unlist()
  }

  # All labels below value go to the left for both options, in which the order is considered
  if(node_id %% 2 == 0){
    right_node <- split_variable_levels[ceiling(split_value):length(split_variable_levels)]
    return(paste(right_node, collapse=","))
  }else{
    left_node <- split_variable_levels[1:floor(split_value)]
    return(paste(left_node, collapse=","))
  }
}
