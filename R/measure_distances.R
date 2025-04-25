#' \code{measure_distances} uses tree-based distance metrics to calculate
#' standardized values for pair-wise dissimilarity of trees in a random
#' forest trained with \code{ranger}.
#'
#' @title Measure pair-wise distances between trees of a random forest
#'
#' @param rf         Object of class \code{ranger} used with \code{write.forest = TRUE}.
#' @param metric     Specification of the tree metric. Available are "splitting variables",
#'                   "weighted splitting variables", "terminal nodes" and "prediction".
#' @param test_data  Additional data set comparable to the data set \code{rf} was build on.
#'
#' @author Lea Louisa Kronziel, M.Sc.
#' @return
#'   \item{\code{distances}}{matrix of size \code{num.trees}x\code{num.trees}}
#'
#' @export measure_distances
#'
#' @import ranger
#' @import dplyr
#' @import checkmate
#'
#' @examples
#' require(ranger)
#' require(timbR)
#'
#' set.seed(12345)
#' # Train random forest with ranger
#' rg.iris <- ranger(Species ~ ., data = iris, write.forest=TRUE, num.trees = 10)
#'
#' #
#' measure_distances(rf = rg.iris, metric = "splitting variables")
#' measure_distances(rf = rg.iris, metric = "weighted splitting variables")
#' measure_distances(rf = rg.iris, metric = "terminal nodes", test_data = iris)
#' measure_distances(rf = rg.iris, metric = "prediction", test_data = iris)
#'
measure_distances <- function(rf, metric = "splitting variables", test_data = NULL){

  # Check inputs ----
  if (!checkmate::testClass(rf, "ranger")){
    stop("rf must be of class ranger")
  }
  if (!checkmate::testList(rf$forest)){
    stop("rf must be trained using write.forest = TRUE.")
  }
  if (!checkmate::testChoice(metric,
                             choices = c("splitting variables", "weighted splitting variables", "terminal nodes", "prediction"))){
    stop(paste("metric has to be from c('splitting variables', 'weighted splitting variables', 'terminal nodes', 'prediction')."))
  }
  if (metric %in% c("terminal nodes", "prediction")){
    if (checkmate::testNull(test_data)){
      stop("You have to provide a test data set for distance measure by terminal nodes or prediction.")
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

    # Calculate standardized pair-wise distances
    distances <- as.matrix(dist(t(feature_usage), method = "euclidian"))^2 / num_features
  }

  # Calculation weighted version of d0 ----
  if (metric == "weighted splitting variables"){
    # # Extract number of features
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

    distances <- as.matrix(dist(US, method = "euclidian"))^2
  }

  # Calculation for d1 of Banerjee et al. (2012) ----
  if (metric == "terminal nodes"){

    # Calculate terminal nodes for every tree and observation
    terminal_nodes <- predict(rf, data = test_data, type = "terminalNodes")$predictions

    # Function to calculate list of matrices if two observations end in same leaf for every tree
    calculate_same_leaf <- function(tree_nodes){as.matrix(dist(tree_nodes)) == 0}

    # Calculate if two observations end in same leaf of the trees
    same_leaf_list = apply(terminal_nodes, 2, calculate_same_leaf, simplify = F)

    # Function to calculate the frequency of how often pairwise observations in two trees behave differently
    # (same leaf in one tree vs. different leaves in the other tree)
    calculate_dist_terminal_leafs <- function(same_nodes_a,same_nodes_b){
      return(sum(xor(same_nodes_a,same_nodes_b))/(nrow(same_nodes_a)^2-(nrow(same_nodes_a))))
    }

    # calculate distance
    distances <- outer(same_leaf_list,same_leaf_list,Vectorize(calculate_dist_terminal_leafs))
  }

  # Calculation for d2 of Banerjee et al. (2012) ----
  if (metric == "prediction"){

    # Predict outcome for all test data
    pred <- predict(rf, data = test_data, predict.all = TRUE)

    # Controll if outcome is factor
    if (is.factor(rf$predictions)){
      # Calculate standardized pair-wise distances
      distances <- as.matrix(dist(t(pred$predictions), method = "manhattan")) / nrow(test_data)
    } else {
      # Calculate standardized pair-wise distances: quadratic euclidian distance
      distances <- as.matrix(dist(t(pred$predictions), method = "euclidian"))^2 / nrow(test_data)
    }
  }

  # Return distance matrix ----
  return(distances)
}
