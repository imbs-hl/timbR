#' \code{generate_tree} uses pair-wise dissimilarity of trees in a random
#' forest trained with \code{ranger} to generate an artificail most representative tree
#' which is not part aof the original ensemble
#'
#' @title Generate most representative tree for a random forest
#'
#' @param rf              Object of class \code{ranger} used with \code{write.forest = TRUE} to generate tree for.
#' @param metric          Specification of the tree metric. Available are "splitting variables",
#'                        "weighted splitting variables", "terminal nodes" and "prediction".
#' @param train_data      Data set for training of artificial representative tree
#' @param importance.mode If TRUE variable importance measures will be used to prioritize next split in tree generation.
#'                        Improves speed. Variable importance values have to be included in ranger object.
#' @param imp.num.var     Number of variables to be pre selected based on importance values.
#' @param ...             Further parameters passed on to measure_distances (e.g. test_data)
#'
#' @author Bjoern-Hergen Laabs, M.Sc.
#' @return
#'   \item{\code{rep.trees}}{\code{ranger} object containing the artificial most representative tree}
#' @import ranger
#' @import dplyr
#' @import checkmate
#' @import data.table
#'
#' @export generate_tree
#'
#' @examples
#' require(ranger)
#' require(timbR)
#'
#' ## Train random forest with ranger
#' rg.iris <- ranger(Species ~ .,
#'                   data = iris,
#'                   write.forest=TRUE,
#'                   num.trees = 10,
#'                   importance = "permutation"
#'                   )
#'
#' ## Calculate pair-wise distances for all trees
#' rep_tree <- generate_tree(rf = rg.iris, metric = "splitting variables", train_data = iris)
#'
generate_tree <- function(rf, metric = "weighted splitting variables", train_data, test_data = NULL, importance.mode = FALSE, imp.num.var = 1){
  ## Check input ----
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
    if ("try-error" %in% class(try(predict(rf, data = test_data), silent = TRUE))){
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

  if ((rf$treetype %in% c("Classification", "Regression") == FALSE)){
    stop("Treetyp not supported. Please use classification or regression trees.")
  }

  if (checkmate::testNull(train_data)){
    stop("You have to provide a train data set for generating the artificial representative tree.")
  }

  if ("try-error" %in% class(try(predict(rf, data = train_data), silent = TRUE))){
    stop("The provided train data set does not fit to the provided ranger object")
  }

  if (importance.mode & rf$importance.mode == "none"){
    stop("Please provide importance values in ranger object")
  }

  if (imp.num.var > length(rf$variable.importance)){
    stop("You tried to select more variables by imp.num.var, than splitting variables in the random forest.")
  }

  ## Extract split points ----
  split_points <- lapply(1:rf$num.trees, function(X){
    ## Extract tree info
    tree_info <- treeInfo(rf, tree = X)

    ## Exclude terminal nodes
    internal_nodes <- tree_info[tree_info$terminal == FALSE,]

    ## Extract split points
    #split_points <- paste(internal_nodes$splitvarID, internal_nodes$splitvarName, internal_nodes$splitval, sep = "_")
    split_points <- data.frame(split_varID   = internal_nodes$splitvarID,
                               split_var     = internal_nodes$splitvarName,
                               split_val     = internal_nodes$splitval)
    split_points <- unique(split_points)
    return(split_points)
  })

  ## Unlist split points
  split_points <- data.table::rbindlist(split_points)
  split_points <- unique(split_points)

  if (importance.mode){
    # Recode variable importance values
    imp <- data.frame(imp = rf$variable.importance, var = names(rf$variable.importance))
    # Select fraction of variables
    imp <- imp[sort(imp$imp, decreasing = TRUE, index.return = TRUE)$ix[1:imp.num.var],]

    # Match split points with importance values
    split_points <- split_points[split_points$split_var %in% imp$var,]
  }

  split_points <- list(split_points)

  ## Initialize tree ----
  min_dist <- Inf
  min_dist_tree <- Inf
  min_pred_error <- Inf
  node_data <- list(train_data)

  rf_rep <- rf

  rf_rep$num.trees <- rf_rep$num.trees + 1
  rf_rep$forest$num.trees <- rf_rep$forest$num.trees + 1

  rf_rep$forest$child.nodeIDs[[rf_rep$num.trees]] <- list(0,0)
  rf_rep$forest$split.varIDs[[rf_rep$num.trees]] <- 0

  ## Add prediction
  dependent_varname <- strsplit(as.character(rf$call)[2], " ~")[[1]][1]
  if(rf_rep$treetype == "Classification"){
    prediction <- names(which.max(table(train_data[,names(train_data) == dependent_varname])))
    rf_rep$forest$split.values[[rf_rep$num.trees]] <- as.numeric(which(rf_rep$forest$levels == prediction))
  } else if(rf_rep$treetype == "Regression"){
    rf_rep$forest$split.values[[rf_rep$num.trees]] <- mean(train_data[,names(train_data) == dependent_varname])
  }

  ## Add new node ----
  add_node <- function(rf_rep, node, split_point){
    split_varID <- split_point[1]
    split_var   <- as.character(split_point[2])
    split_val   <- as.numeric(split_point[3])

    max_node <- max(unlist(rf_rep$forest$child.nodeIDs[[rf_rep$num.trees]]))

    ## Add child nodes
    rf_rep$forest$child.nodeIDs[[rf_rep$num.trees]][[1]][node] <- max_node + 1
    rf_rep$forest$child.nodeIDs[[rf_rep$num.trees]][[2]][node] <- max_node + 2

    rf_rep$forest$child.nodeIDs[[rf_rep$num.trees]][[1]][max_node + 2] <- 0
    rf_rep$forest$child.nodeIDs[[rf_rep$num.trees]][[2]][max_node + 2] <- 0

    rf_rep$forest$child.nodeIDs[[rf_rep$num.trees]][[1]][max_node + 3] <- 0
    rf_rep$forest$child.nodeIDs[[rf_rep$num.trees]][[2]][max_node + 3] <- 0

    ## Add split varID
    rf_rep$forest$split.varIDs[[rf_rep$num.trees]][node] <- as.numeric(split_varID)
    rf_rep$forest$split.varIDs[[rf_rep$num.trees]][max_node + 2] <- 0
    rf_rep$forest$split.varIDs[[rf_rep$num.trees]][max_node + 3] <- 0

    ## Add split value
    rf_rep$forest$split.values[[rf_rep$num.trees]][node] <- as.numeric(split_val)

    ## Split node_data by splitpoint
    node_data_left <- node_data[[node]]
    node_data_left <- node_data_left[as.numeric(unlist(node_data_left[,names(node_data_left) == split_var])) <= split_val,]

    node_data_right <- node_data[[node]]
    node_data_right <- node_data_right[as.numeric(unlist(node_data_right[,names(node_data_right) == split_var])) > split_val,]

    ## Add predictions
    if(rf_rep$treetype == "Classification"){
      prediction <- names(which.max(table(node_data_left[,names(node_data_left) == dependent_varname])))
      rf_rep$forest$split.values[[rf_rep$num.trees]][max_node + 2] <- as.numeric(which(rf_rep$forest$levels == prediction))

      prediction <- names(which.max(table(node_data_right[,names(node_data_right) == dependent_varname])))
      rf_rep$forest$split.values[[rf_rep$num.trees]][max_node + 3] <- as.numeric(which(rf_rep$forest$levels == prediction))

    } else if(rf_rep$treetype == "Regression"){
      rf_rep$forest$split.values[[rf_rep$num.trees]][max_node + 2] <- mean(node_data_left[,names(node_data_left) == dependent_varname])
      rf_rep$forest$split.values[[rf_rep$num.trees]][max_node + 3] <- mean(node_data_right[,names(node_data_right) == dependent_varname])
    }
    return(rf_rep)
  }


  while(min_dist_tree < min_dist | min_dist == Inf){
    min_dist <- min_dist_tree
    max_node <- max(unlist(rf_rep$forest$child.nodeIDs[[rf_rep$num.trees]]))
    terminal_nodes <- which(treeInfo(rf_rep, rf_rep$num.trees)$terminal)
    if (nrow(rbindlist(split_points[terminal_nodes])) > 0){
      ## Generate trees for all possible split points
      possible_rf_rep <- lapply(terminal_nodes, function(x){
        possible_rf_rep <- apply(split_points[[x]], 1, function(X){
          return(add_node(rf_rep, x, X))
        })
      })

      possible_rf_rep <- unlist(possible_rf_rep, recursive = FALSE)

      ## Calculate mean distances for all possible split points
      mean_distances <- lapply(possible_rf_rep, function(X){
        dist <- suppressMessages(mean(measure_distances(X, metric, test_data)[rf_rep$num.trees]))
        return(dist)
      })
      mean_distances <- unlist(mean_distances)

      ## Estimate prediction accuracy for all possible split points
      pred_error <- lapply(possible_rf_rep, function(X){
        pred <- suppressWarnings(predict(X, train_data, predict.all = TRUE)$predictions[,X$num.trees])
        if(rf_rep$treetype == "Classification"){
          true <- as.character(unlist(train_data[,names(train_data) == dependent_varname]))
          pred <- rf$forest$levels[pred]
          return(sum(pred != true)/length(pred))
        } else if(rf_rep$treetype == "Regression"){
          true <- as.numeric(train_data[,names(train_data) == dependent_varname])
          return(sum((pred - true)^2)/length(pred))
        }
      })
      pred_error <- unlist(pred_error)

      ## Select optimal tree
      if(min(mean_distances, na.rm = TRUE) < Inf){
        min_dist_trees <- which(mean_distances == min(mean_distances, na.rm = TRUE))
        opt_tree <- which(pred_error[min_dist_trees] == min(pred_error[min_dist_trees], na.rm = TRUE))
        opt_idx  <- min_dist_trees[opt_tree][1]

        if(mean_distances[opt_idx] < min_dist){
          ## Set new rf_rep
          node <- which(treeInfo(rf_rep, rf_rep$num.trees)$terminal != treeInfo(possible_rf_rep[[opt_idx]], possible_rf_rep[[opt_idx]]$num.trees)$terminal[1:nrow(treeInfo(rf_rep, rf_rep$num.trees))])

          rf_rep <- possible_rf_rep[[opt_idx]]

          ## Set possible new split_points
          used_split_point <- split_points[[node]][opt_idx,]
          used_split_point <- data.frame(split_varID = treeInfo(rf_rep, rf_rep$num.trees)$splitvarID[node],
                                         split_val   = treeInfo(rf_rep, rf_rep$num.trees)$splitval[node],
                                         split_var   = treeInfo(rf_rep, rf_rep$num.trees)$splitvarName[node])
          split_points[[max_node + 2]] <- split_points[[node]][split_points[[node]]$split_varID != used_split_point$split_varID | split_points[[node]]$split_val < used_split_point$split_val,]
          split_points[[max_node + 3]] <- split_points[[node]][split_points[[node]]$split_varID != used_split_point$split_varID | split_points[[node]]$split_val > used_split_point$split_val,]

          ## Set train_data
          node_data_left <- node_data[[node]]
          node_data_left <- node_data_left[as.numeric(unlist(node_data_left[,names(node_data_left) == used_split_point$split_var])) <= used_split_point$split_val,]

          node_data_right <- node_data[[node]]
          node_data_right <- node_data_right[as.numeric(unlist(node_data_right[,names(node_data_right) == used_split_point$split_var])) > used_split_point$split_val,]

          node_data[[max_node + 2]] <- node_data_left
          node_data[[max_node + 3]] <- node_data_right

          min_dist_tree <- mean_distances[opt_idx]
          min_pred_error <- pred_error[opt_idx]
        }
      }
    }
  }

  ## Select and return final tree
  idx <- rf_rep$num.trees
  rf_rep$num.trees <- 1
  rf_rep$forest$num.trees <- 1
  rf_rep$forest$child.nodeIDs <- rf_rep$forest$child.nodeIDs[idx]
  rf_rep$forest$split.varIDs  <- rf_rep$forest$split.varIDs[idx]
  rf_rep$forest$split.values  <- rf_rep$forest$split.values[idx]

  if(length(rf$inbag.counts) > 0){
    rf_rep$inbag.counts <- rf_rep$inbag.counts[idx]
  }

  rf_rep$predictions      <- NULL
  rf_rep$prediction.error <- NULL

  ## Return reduced ranger object
  return(rf_rep)
}
