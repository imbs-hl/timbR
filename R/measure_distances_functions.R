library(ranger)

## Funktion zur Berechnung der verschiedenen Distanzmaße
## "weighted"

rf <- ranger(Species ~ ., data = iris, num.trees = 10)

  ## Calculation weighted version of d0 ----
  weighted_dist <- function(rf){
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
prediction_dist <- function(rf, test_data){

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

