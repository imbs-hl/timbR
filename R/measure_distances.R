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
#'@author Dr. Bjoern-Hergen Laabs
#' @return
#'   \item{\code{distances}}{matrix of size \code{num.trees}x\code{num.trees}}
#' @export measure_distances
#' @import ranger
#' @import dplyr
#' @import checkmate
#'
#' @examples
#' require(ranger)
#' require(timbR)
#'
#' set.seed(12345)
#' ## Train random forest with ranger
#' rg.iris <- ranger(Species ~ ., data = iris, write.forest=TRUE, num.trees = 10)
#'
#' ##
#' measure_distances(rf = rg.iris, metric = "splitting variables")
#' measure_distances(rf = rg.iris, metric = "weighted splitting variables")
#' measure_distances(rf = rg.iris, metric = "terminal nodes", test_data = iris)
#' measure_distances(rf = rg.iris, metric = "prediction", test_data = iris)
#'
measure_distances <- function(rf, metric = "splitting variables", test_data = NULL){

  ## Check inputs ----
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

  ## Prepare matrix for output ----
  distances <- matrix(data = NA, nrow = rf$num.trees, ncol = rf$num.trees)

  ## Extract outcome id
  outcome_id <- rf$forest$dependent.varID

  ## Extract number of features
  num_features <- rf$num.independent.variables

  ## Calculation for d0 of Banerjee et al. (2012) ----
  if (metric == "splitting variables"){
    ## Simplify for each tree which features were used
    feature_usage <- lapply(X      = 1:rf$num.trees,
                            FUN    = function(x){
                              splitting_variables <- sort(unique(treeInfo(rf, x)$splitvarID))
                              fu <- rep(0, num_features)
                              fu[(splitting_variables+1)] <- 1
                              fu
                            })

    ## Calculate standardized pair-wise distances
    for (i in 1:rf$num.trees){
      for (j in 1:rf$num.trees){
        distances[i,j] <- sum((feature_usage[[i]] - feature_usage[[j]])^2)/num_features
      }
    }
  }

  ## Calculation weighted version of d0 ----
  if (metric == "weighted splitting variables"){
    ## Calculate usage score for each variable
    US <- lapply(1:rf$num.trees, function(i){
      ## initialize levels for ith tree
      split_level <- rep(1, length(rf$forest$split.varIDs[[i]]))

      ## Extract child node IDs for ith tree
      child.nodeIDs <- rf$forest$child.nodeIDs[[i]]

      ## Extract split var IDs for ith tree
      split.varIDs <- rf$forest$split.varIDs[[i]]

      ## Child nodes get level of parent node + 1 ----
      for (j in 1:length(rf$forest$split.varIDs[[i]])){
        if (child.nodeIDs[[1]][j] != 0){
          split_level[child.nodeIDs[[1]][j] + 1] <- split_level[j] + 1
        }

        if (child.nodeIDs[[2]][j] != 0){
          split_level[child.nodeIDs[[2]][j] + 1] <- split_level[j] + 1
        }
      }

      ## Usage score for each variable
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


    distance <- lapply(1:rf$num.trees, function(x){
      distance <- lapply(1:rf$num.trees, function(y){
        #1/rf$num.independent.variables * sum((US[x,] - US[y,])^2)
        sum((US[x,] - US[y,])^2)
      })

      as.numeric(do.call("rbind", distance))
    })

    distances <- as.matrix(do.call("rbind", distance))

  }

  ## Calculation for d1 of Banerjee et al. (2012) ----
  if (metric == "terminal nodes"){
    distances <- matrix(data = 0, nrow = rf$num.trees, ncol = rf$num.trees)

    ## Initialize matrix for terminal nodes
    term_node <- predict(rf, data = test_data, type = "terminalNodes")$predictions

    ## Calculate if observations end in same terminal node for each tree
    I <- list()

    for (x in 1:rf$num.trees){
      I[[x]] <- matrix(data = NA, nrow = nrow(test_data), ncol = nrow(test_data))

      for (i in 1:nrow(test_data)){
        for (j in 1:nrow(test_data)){
          if (term_node[i,x] == term_node[j,x]){
            I[[x]][i,j] <- 1
          } else {
            I[[x]][i,j] <- 0
          }
        }
      }
    }

    ## Calculate distances
    for (x in 1:rf$num.trees){
      for (y in 1:rf$num.trees){
        for (i in 1:(nrow(test_data)-1)){
          for (j in (i+1):nrow(test_data)){
            distances[x,y] <- distances[x,y] + abs(I[[x]][i,j] - I[[y]][i,j])
          }
        }
      }
    }

    ## Normailze distances
    distances <- distances / choose(nrow(test_data), 2)
  }

  ## Calculation for d2 of Banerjee et al. (2012) ----
  if (metric == "prediction"){

    ## Predict outcome for all test data
    pred <- predict(rf, data = test_data, predict.all = TRUE)

    ## Controll if outcome is factor
    if (is.factor(rf$predictions)){
      ## Calculate standardized pair-wise distances
      for (i in 1:rf$num.trees){
        for (j in 1:rf$num.trees){
          distances[i,j] <- sum(pred$predictions[,i] != pred$predictions[,j])/nrow(test_data)
        }
      }
    } else {
      ## Calculate standardized pair-wise distances
      for (i in 1:rf$num.trees){
        for (j in 1:rf$num.trees){
          distances[i,j] <- sum((pred$predictions[,i] - pred$predictions[,j])^2)/nrow(test_data)
        }
      }
    }
  }

  ## Return distance matrix ----
  return(distances)
}
