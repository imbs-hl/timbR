#' Get prediction of the node
#' @param tree_info_df         Data frame containing information about the structure of the decision tree, which is built like a "treeInfo()" data frame from the package "ranger"
#' @param train_data_df        Data frame of the training data with which the random forest was trained
#' @param rf_list              Random forest, which is built like the one you get from ranger()
#' @param dependent_var        Name of the column of the dependent variable in training data
#' @param tree_number          Number of the decision tree of the rf_list to be displayed
#' @param node_id              Node ID of the node whose parent ID is to be determined
#' @author Lea Louisa Kronziel, M.Sc.
#' @returns                    Character with the prediction of the node

get_prediction_node <- function(tree_info_df, train_data_df, rf_list, dependent_var, tree_number, node_id){
  # Get inbag data
  inbag_data_df <- get_inbag_data(rf_list, train_data_df, tree_number)

  # Get splitted data
  splitted_data_list <- get_splitted_data(tree_info_df, inbag_data_df, rf_list, tree_number)

  # Get predictions of node: vector with labels of inbag data of node
  label_vector <- splitted_data_list[[node_id+1]][dependent_var] %>% t() %>% as.vector()

  if(rf_list$forest$treetype=="Regression"){
    # Mean of values of the label
    prediction <- round(mean(label_vector), 2)

    # Variance of values
    if(length(label_vector) > 1){
      prediction_acc <- paste0(" ($\\mathbb{S}$ = ", round(sd(label_vector), 2), ")")
    }else{
      prediction_acc <- ""
    }


  }else if(rf_list$forest$treetype=="Classification"){
    # Frequency of labels in node for classification
    prediction_freq <- sort(table(label_vector), decreasing = TRUE)

    # Prediction
    prediction <- prediction_freq[1] %>% names()

    # Accuracy of prediction
    prediction_acc <- paste0("(", round(prediction_freq[1]/length(label_vector)*100), "\\%)")
  }


  return(paste0(prediction, prediction_acc))

}
