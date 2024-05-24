#' \code{generate_tree} uses pair-wise dissimilarity of trees in a random
#' forest trained with \code{ranger} to generate an artificial most representative tree
#' which is not part of the original ensemble
#'
#' @title Generate most representative tree for a random forest
#'
#' @param rf                Object of class \code{ranger} used with \code{write.forest = TRUE} to generate tree for.
#' @param metric            Specification of the tree metric. Available are "splitting variables",
#'                          "weighted splitting variables", "terminal nodes" and "prediction".
#' @param train_data        Data set for training of artificial representative tree
#' @param dependent_varname Name of the dependent variable used to create the forest
#' @param importance.mode   If TRUE variable importance measures will be used to prioritize next split in tree generation.
#'                          Improves speed. Variable importance values have to be included in ranger object.
#' @param imp.num.var       Number of variables to be pre selected based on importance values. If "automatic" the Boruta
#'                          variable selection algorithm from Kursa et al. (2010) is used (could be time consuming).
#'                          Insert a numeric value, if you want to define the number on your own.
#' @param test_data         Additional data set comparable to the data set \code{rf} was build on.
#' @param probs_quantiles   Vector with values from 0 to 1 or NULL. Possibility to choose quantiles as split points
#'                          (e.g. c(0.25, 0.5, 0.75)) for continuous variables, otherwise could be very time-consuming.
#' @param epsilon           The creation of the tree is continued even if the similarity stays the same if the percentage of
#'                          the prediction improves by 1 - epsilon.
#' @param ...               Further parameters passed on to Boruta (e.g. pValue)
#'
#' @author Bjoern-Hergen Laabs, M.Sc.
#' @return
#'   \item{\code{rep.trees}}{\code{ranger} object containing the artificial most representative tree}
#' @import ranger
#' @import dplyr
#' @import checkmate
#' @import data.table
#' @import Boruta
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
#' rep_tree <- generate_tree(rf = rg.iris, metric = "splitting variables", train_data = iris, dependent_varname = "Species", importance.mode = TRUE, imp.num.var = 1)
#'
generate_tree <- function(rf, metric = "weighted splitting variables", train_data, test_data = NULL, dependent_varname, importance.mode = FALSE, imp.num.var = NULL, probs_quantiles = NULL, epsilon = 0, ...){

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

  if(importance.mode == TRUE & is.null(imp.num.var)){
    stop("Please insert a number or the string 'automatic' for imp.num.var")
  }

  if(importance.mode == TRUE & !is.null(imp.num.var)){
    if(!(is.numeric(imp.num.var) | imp.num.var == "automatic")){
      stop("Please insert a number or the string 'automatic' for imp.num.var")
    }
  }

  if (importance.mode & rf$importance.mode == "none"){
    stop("Please provide importance values in ranger object")
  }

  if(!is.null(imp.num.var)){
    if (is.numeric(imp.num.var)){
      if (imp.num.var > 0 & rf$importance.mode == "none" |
          imp.num.var > 0 & importance.mode == FALSE){
        stop("Your input was not consistent regarding the use or non-use of importance.")
      }
      if (imp.num.var > length(rf$variable.importance)){
        stop("You tried to select more variables by imp.num.var, than splitting variables in the random forest.")
      }
    }

    if (imp.num.var == "automatic" & (rf$importance.mode == "none" | importance.mode == FALSE)){
      stop("Your input was not consistent regarding the use or non-use of importance.")
    }
  }

  if(!is.null(probs_quantiles) & !is.numeric(probs_quantiles)){
    stop("probs_quantiles must be NULL or numerical.")
  }

  if(!is.null(probs_quantiles) & any(probs_quantiles < 0 | probs_quantiles > 1)){
    stop("probs_quantiles must consist of probabilities from 0 to 1.")
  }

  if(!is.numeric(epsilon) | is.na(epsilon) | is.null(epsilon)){
    stop("epsilon most consist of a numerical value from 0 to 1.")
  }
  if(is.numeric(epsilon)){
    if(epsilon < 0 | epsilon > 1){
      stop("epsilon most consist of a numerical value from 0 to 1.")
    }
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

  # Use quantiles instead of all possible used split points for continuous variables
  if(!is.null(probs_quantiles)){
    # numbers of split points of all variables
    number_split_points <- data.frame(table(split_points$split_var))

    # add type of variables
    type_variables <- rf$forest$covariate.levels
    if(!is.null(type_variables)){
      type_variables <- data.frame(variable = names(type_variables),
                                   levels = sapply(type_variables, function(x) ifelse(is.null(x), NA, x)))

      number_split_points <- left_join(number_split_points, type_variables, by = c("Var1" = "variable"))
    }else{
      number_split_points$type = NA
    }

    number_split_points <- number_split_points %>% mutate(use_quantiles = ifelse(Freq>length(probs_quantiles)& is.na(levels), 1, 0))

    # check if any variable has more split points than wanted quantiles
    if(any(number_split_points$use_quantiles == 1)){

      ids_split <- as.character(number_split_points$Var1[number_split_points$use_quantiles == 1])

      # calculate quantiles for new split points
      new_split_points <- lapply(ids_split, function(x){
        # calculate quantiles of split points
        split_quantiles <- quantile(split_points$split_val[split_points$split_var == x],
                                    probs = probs_quantiles)

        # define new split points
        data.frame(split_varID = split_points$split_varID[split_points$split_var == x][1],
                   split_var = x,
                   split_val = split_quantiles)
      })

      new_split_points <- bind_rows(new_split_points) %>% unique()

      # remove old split points and add new ones
      split_points <- split_points %>%
        filter(!(split_var %in% ids_split)) %>%
        bind_rows(new_split_points)
    }
  }


  if(!is.null(imp.num.var)){
    if(importance.mode & imp.num.var == "automatic"){
      # Variable selection with Boruta algorithm from Kursa et al. (2010)
      x = train_data %>%
        select(-all_of(dependent_varname))
      y = train_data %>%
        select(all_of(dependent_varname)) %>%
        unlist()

      # Test for importance with Boruta
      imp = Boruta(x = x, y = y, ...)$finalDecision

      # Filter the important variables
      var <- names(imp)
      decision <- data.frame(var = var, decision = imp) %>%
        filter(decision == "Confirmed")

      # Match split points with importance values
      split_points <- split_points[split_points$split_var %in% decision$var,]

    }
    if (importance.mode & is.numeric(imp.num.var)){
      # Recode variable importance values
      imp <- data.frame(imp = rf$variable.importance, var = names(rf$variable.importance))
      # Select fraction of variables
      imp <- imp[sort(imp$imp, decreasing = TRUE, index.return = TRUE)$ix[1:imp.num.var],]

      # Match split points with importance values
      split_points <- split_points[split_points$split_var %in% imp$var,]
    }
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

    ## Split node_data by splitpoint
    node_data_left <- node_data[[node]]
    node_data_left <- node_data_left[as.numeric(unlist(node_data_left[,names(node_data_left) == split_var])) <= split_val,]

    node_data_right <- node_data[[node]]
    node_data_right <- node_data_right[as.numeric(unlist(node_data_right[,names(node_data_right) == split_var])) > split_val,]

    ## check if data is passed in the left and right nodes, otherwise the split would be useless
    if(nrow(node_data_left) == 0 | nrow(node_data_right) == 0){
      return(rf_rep)
    }

    ## Check if split is allowed
    if(rf_rep$treetype == "Classification"){
      prediction_left <- names(which.max(table(node_data_left[,names(node_data_left) == dependent_varname])))
      prediction_right <- names(which.max(table(node_data_right[,names(node_data_right) == dependent_varname])))
    } else if(rf_rep$treetype == "Regression"){
      prediction_left <- mean(node_data_left[,names(node_data_left) == dependent_varname])
      prediction_right <- mean(node_data_right[,names(node_data_right) == dependent_varname])
    }
    ## Exclude nonsense splits
    if(is.na(prediction_left) | is.na(prediction_right) | is.nan(prediction_left) | is.nan(prediction_right) | is.infinite(prediction_left) | is.infinite(prediction_right)){
      return(rf_rep)
    } else if(prediction_left != prediction_right){
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
    }else{
      return(rf_rep)
    }
  }

  # Parameter to check, if adding nodes should be continued
  # (better similarity or equal simularity with better prediction performance than 1 - epsilon)
  continue <- TRUE

  while((min_dist_tree <= min_dist | min_dist == Inf) & continue){
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
        opt_error <- pred_error[opt_idx]

        # check if prediction is better than 1- epsilon
        pred_check <- opt_error / min_pred_error < 1 - epsilon

        if(mean_distances[opt_idx] < min_dist |
           (mean_distances[opt_idx] == min_dist & pred_check)){

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
          continue <- TRUE
        }else{
          continue <- FALSE
          # checken
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
