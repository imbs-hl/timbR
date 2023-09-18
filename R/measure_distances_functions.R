#' Funktionen zur Berechnung von verschiedenen paarweisen Distanzmaßen in Random Forests
#'
#' @param rf trainiertes ranger Objekt
#' @param test_data Testdaten auf denen die prediction Distanz bestimmt werden kann
#'
#'


library(ranger)

## Funktion zur Berechnung der verschiedenen Distanzmaße
## "weighted"

# rf <- ranger(Species ~ ., data = iris, num.trees = 10)

  ## Calculation weighted version of d0 ----
  weighted_dist_fun <- function(rf){

    ## Prepare matrix for output ----
    distances <- matrix(data = NA, nrow = rf$num.trees, ncol = rf$num.trees)

    ## Extract outcome id
    outcome_id <- rf$forest$dependent.varID

    ## Extract number of features
    num_features <- rf$num.independent.variables
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
        sum(1/(2^(split_level[split.varIDs == j] - 1))) / (max(split_level) - 1)
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

    ## return of the distance matrix
    return(distances)
  }


## "prediction"

## Calculation for d2 of Banerjee et al. (2012) ----
prediction_dist_fun <- function(rf, test_data){

  ## Prepare matrix for output ----
  distances <- matrix(data = NA, nrow = rf$num.trees, ncol = rf$num.trees)

  ## Extract outcome id
  outcome_id <- rf$forest$dependent.varID

  ## Extract number of features
  num_features <- rf$num.independent.variables

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
  ## return of the distance matrix
  return(distances)
}

## "splitting Variables"

## Calculation for d0 of Banerjee et al. (2012) ----
splitting_variables_fun <- function(rf){

  ## Prepare matrix for output ----
  distances <- matrix(data = NA, nrow = rf$num.trees, ncol = rf$num.trees)

  ## Extract outcome id
  outcome_id <- rf$forest$dependent.varID

  ## Extract number of features
  num_features <- rf$num.independent.variables


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

## "terminal Nodes"

## Calculation for d1 of Banerjee et al. (2012) ----
terminal_nodes_fun <- function(rf, test_data){

  ## Prepare matrix for output ----
  distances <- matrix(data = NA, nrow = rf$num.trees, ncol = rf$num.trees)

  ## Extract outcome id
  outcome_id <- rf$forest$dependent.varID

  ## Extract number of features
  num_features <- rf$num.independent.variables


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

