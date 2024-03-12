#' Get inbag data of decision tree
#' @param train_data_df        Data frame of the training data with which the random forest was trained
#' @param rf_list              Random forest, which is built like the one you get from ranger()
#' @param tree_number          Number of the decision tree of the rf_list to be displayed
#' @author Lea Louisa Kronziel, M.Sc.
#' @returns                    Data frame with inbag observations of decision tree

get_inbag_data <- function(rf_list, train_data_df, tree_number){
  # extract inbag counts from ranger object
  # one value per observation from train_data_df
  inbag_counts <- rf_list$inbag.counts[[tree_number]]

  # transform inbag counts to logical variables
  inbag_logical <- ifelse(inbag_counts > 0, TRUE, FALSE)

  # return inbag data of selected tree
  return(train_data_df[inbag_logical,])
}
