#' \code{get_distance_values} calculated tree based distance values for a single tree or random forest built with \code{ranger}.
#' Returned distance values still need to be aggregated, e.g. with get_distance_score.R.
#' function to get distance values that returns matrix with distance values
#'
#'
#' @param rf         Object of class \code{ranger} used with \code{write.forest = TRUE}.
#' @param metric     Specification of distance (dissimilarity) metric. Available are "splitting variables",
#'                   "weighted splitting variables", and "prediction".
#' @param test_data  Additional data set comparable to the data set \code{rf} was build on. Only needed for metric "prediction".
#'
#' @author Lea Louisa Kronziel, M.sc.
#' @return matrix of values for distance calculation (cols = trees)
#' @import ranger
#' @import dplyr
#' @import checkmate
#'

get_distance_values <- function(rf, metric = metric, test_data = NULL){

  # Check inputs ----
  if (!checkmate::testClass(rf, "ranger")){
    stop("rf must be of class ranger")
  }
  if (!checkmate::testList(rf$forest)){
    stop("rf must be trained using write.forest = TRUE.")
  }
  if (!checkmate::testChoice(metric,
                             choices = c("splitting variables", "weighted splitting variables", "prediction", "terminal nodes"))){
    stop(paste("metric has to be from c('splitting variables', 'weighted splitting variables', 'prediction', 'terminal nodes')."))
  }
  if (metric %in% c("prediction")){
    if (checkmate::testNull(test_data)){
      stop("You have to provide a test data set for distance measure by prediction.")
    }
    if ("try-error" %in% class(try(predict(rf, data = test_data)))){
      stop("The provided test data set does not fit to the provided ranger object")
    }
    if (nrow(test_data) < 2){
      stop("You have to provide at least two samples as a test data set")
    }
  } else {
    if (!checkmate::testNull(test_data)){
      message("You provided a test data set for a distance measure by splitting variables. This is not necessary and will be ignored.")
    }
  }


  # Calculation for d0 of Banerjee et al. (2012) ----
  if (metric == "splitting variables"){
    # # Extract number of features
    num_features <- rf$num.independent.variables
    # Simplify for each tree which features were used
    feature_usage <- matrix(unlist(lapply(X      = 1:rf$num.trees,
                                          FUN    = function(x){
                                            splitting_variables <- treeInfo(rf, x)$splitvarID
                                            fu <- rep(0, num_features)
                                            fu[(splitting_variables+1)] <- 1
                                            fu
                                          }
    )
    ),
    nrow = num_features
    )

    return(feature_usage)
  }

  # Calculation weighted version of d0 ----
  if (metric == "weighted splitting variables"){
    # Extract number of features
    num_features <- rf$num.independent.variables

    # Calculate usage score for each variable
    US <- lapply(1:rf$num.trees, function(i){
      # initialize levels for ith tree
      split_level <- rep(1, length(rf$forest$split.varIDs[[i]]))

      # Extract child node IDs for ith tree
      child.nodeIDs <- rf$forest$child.nodeIDs[[i]]

      # Extract split var IDs for ith tree
      split.varIDs <- rf$forest$split.varIDs[[i]]

      # Child nodes get level of parent node + 1 ----
      for (j in 1:length(rf$forest$split.varIDs[[i]])){
        if (child.nodeIDs[[1]][j] != 0){
          split_level[child.nodeIDs[[1]][j] + 1] <- split_level[j] + 1
        }

        if (child.nodeIDs[[2]][j] != 0){
          split_level[child.nodeIDs[[2]][j] + 1] <- split_level[j] + 1
        }
      }

      # Usage score for each variable
      US <- rep(0, rf$num.independent.variables)

      US <- lapply(1:rf$num.independent.variables, function(j){
        if(j-1==0){
          # root node and terminal nodes have ID 0
          terminal_node <- treeInfo(rf, tree = i)$terminal
          sum(1/(2^(split_level[(split.varIDs == (j-1) & !terminal_node)] - 1))) / (max(split_level) - 1)
        }else{
          sum(1/(2^(split_level[split.varIDs == (j-1)] - 1))) / (max(split_level) - 1)
        }
      })

      as.numeric(do.call("cbind", US))
    })

    US <- do.call("rbind", US)

    return(t(US))
  }

  # Calculation for d1 of Banerjee et al. (2012) ----
  if (metric == "terminal nodes"){

    # Calculate terminal nodes for every tree and observation
    terminal_nodes <- predict(rf, data = test_data, type = "terminalNodes")$predictions
    return(terminal_nodes)
  }

  # Calculation for d2 of Banerjee et al. (2012) ----
  if (metric == "prediction"){

    # Predict outcome for all test data
    pred <- predict(rf, data = test_data, predict.all = TRUE)$predictions

    return(pred)
  }
}
