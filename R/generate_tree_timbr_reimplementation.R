#' \code{generate_tree_reimplementation} uses pair-wise dissimilarity of trees in a random
#' forest trained with \code{ranger} to generate an artificial most representative tree
#' which is not part of the original ensemble
#'
#' @title Generate artificial representative tree (art) for a random forest
#'
#' @param rf                Random forest (rf), object of class \code{ranger} used with \code{write.forest = TRUE} to generate tree for.
#' @param metric            Specification of the distance (dissimilarity) metric. Available are "splitting variables",
#'                          "weighted splitting variables" and "prediction".
#' @param train_data        Data set for training of artificial representative tree
#' @param dependent_varname Name of the dependent variable used to create the \code{rf}
#' @param importance.mode   If TRUE variable importance measures will be used to prioritize next split in tree generation.
#'                          Improves speed. Variable importance values have to be included in ranger object.
#' @param imp.num.var       Number of variables to be pre selected based on importance values. If "automatic" the Boruta
#'                          variable selection algorithm from Kursa et al. (2010) is used (could be time consuming).
#'                          Insert a numeric value, if you want to define the number on your own.
#' @param test_data         Additional data set comparable to the data set \code{rf} was build on. Only necessary for \code{metric} prediction.
#' @param probs_quantiles   Vector with values from 0 to 1 or NULL. Possibility to choose quantiles as split points
#'                          (e.g. c(0.25, 0.5, 0.75)) for continuous variables, otherwise could be very time-consuming.
#' @param epsilon           The creation of the tree is continued even if the similarity stays the same if the percentage of
#'                          the prediction improves by 1 - epsilon.
#' @param min.bucket        Minimal terminal node size. No nodes with less obersavtions smaller than this value can occur. Improves speed.
#' @param num.splits        The generated tree consists of a maximum of num.splits splits. Improves speed.
#' @param ...               Further parameters passed on to Boruta (e.g. pValue)
#'
#' @author Lea Louisa Kronziel, M.Sc.
#' @return
#'   \item{\code{rep.trees}}{\code{ranger} object containing the artificial most representative tree}
#' @import ranger
#' @import dplyr
#' @import checkmate
#' @import data.table
#' @import Boruta
#'
#' @export generate_tree_reimplementation
#'
#' @examples
#' require(ranger)
#' require(timbR)
#'
#' # Train random forest with ranger
#' rf.iris <- ranger(Species ~ .,
#'                   data = iris,
#'                   write.forest=TRUE,
#'                   num.trees = 10,
#'                   importance = "permutation"
#'                   )
#'
#' # Calculate pair-wise distances for all trees
#' rep_tree <- generate_tree_reimplementation(rf = rf.iris, metric = "splitting variables", train_data = iris, dependent_varname = "Species", importance.mode = TRUE, imp.num.var = 2, min.bucket = 25)
#'


generate_tree_reimplementation <- function(rf, metric = "weighted splitting variables", train_data, test_data = NULL, dependent_varname,
                                           importance.mode = FALSE, imp.num.var = NULL, probs_quantiles = NULL, epsilon = 0,
                                           min.bucket = 0, num.splits = NULL, ...){

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

  if(rf$treetype == "Classification" & is.numeric(rf$predictions)){
    stop("Your classification ranger has to be trained on a factor as outcome.")
  }

  if(!is.null(num.splits)){
    if(!is.numeric(num.splits)){
      stop("num.splits has to be a positive numerical value.")
    }
    if(num.splits<1 | is.infinite(num.splits)){
      stop("num.splits has to be a positive numerical value smaller than infinity. If no num.splits filter should be applied, please use num.splits = NULL")
    }
   }
  if(!is.numeric(min.bucket)){
    stop("min.bucket has to be a positive numerical value.")
  }
  if(min.bucket<0){
    stop("min.bucket has to be a positive numerical value.")
  }
  if(min.bucket>=nrow(train_data)){
    stop("min.bucket should be smaller than the number of observations in train data. Please use a meaningful value.")
  }


  # ----------
  # Prepare set up to build most similar stump

  # Forest object from ranger
  forest <- rf$forest

  # Get all used split points in RF
  # In the ranger object, the list of splits also contains all terminal nodes (leafs), which have the prediction saved as the split value.
  # The value 0 is entered for both child nodes for leafs so that the leafs can be filtered out of the list of split points.
  split_points <- data.frame(split_varID = cbind(unlist(forest$split.varIDs)),
                             split_value = round(cbind(unlist(forest$split.values)), 100)) %>%
    mutate(split_var = forest$independent.variable.names[split_varID+1])

  # Get information about child nodes to exclude leafs
  child_nodes <- bind_rows(lapply(forest$child.nodeIDs, function(x){return(data.frame(left=x[[1]], right=x[[2]]))}))
  split_points <- data.frame(split_points, child_nodes)

  # Remove leafs and duplicate split points
  split_points <- split_points %>%
    filter(!(left == 0 & right == 0)) %>%
    select(-left, -right) %>%
    unique()

  # Reduce split points to important ones selected by Boruta or to fraction of xx% important ones (if wanted)
  if(importance.mode){
    if(imp.num.var == "automatic"){
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
    if(is.numeric(imp.num.var)){
      # Recode variable importance values
      imp <- data.frame(imp = rf$variable.importance, var = names(rf$variable.importance))

      # Select fraction of variables
      imp <- imp[sort(imp$imp, decreasing = TRUE, index.return = TRUE)$ix[1:imp.num.var],]

      # Match split points with importance values
      split_points <- split_points[split_points$split_var %in% imp$var,]
    }
  }

  # Use quantiles instead of all possible used split points for continuous variables (if wanted)
  if(!is.null(probs_quantiles)){
    # Numbers of split points of all variables
    number_split_points <- data.frame(table(split_points$split_var))

    # Add type of variables
    type_variables <- rf$forest$covariate.levels
    if(!is.null(type_variables)){
      type_variables <- data.frame(variable = names(type_variables),
                                   type = sapply(type_variables, function(x) ifelse(is.null(x), NA, x)))

      number_split_points <- left_join(number_split_points, type_variables, by = c("Var1" = "variable"))
    }else{
      number_split_points$type = NA
    }

    number_split_points <- number_split_points %>% mutate(use_quantiles = ifelse(Freq>length(probs_quantiles)& is.na(type), 1, 0))

    # Check if any variable has more split points than wanted quantiles
    if(any(number_split_points$use_quantiles == 1)){

      ids_split <- as.character(number_split_points$Var1[number_split_points$use_quantiles == 1])

      # Calculate quantiles for new split points
      new_split_points <- lapply(ids_split, function(x){
        # Calculate quantiles of split points
        split_quantiles <- quantile(split_points$split_val[split_points$split_var == x],
                                    probs = probs_quantiles)

        # Define new split points
        data.frame(split_varID = split_points$split_varID[split_points$split_var == x][1],
                   split_var = x,
                   split_value = split_quantiles)
      })

      new_split_points <- bind_rows(new_split_points) %>% unique()

      # remove old split points and add new ones
      split_points <- split_points %>%
        filter(!(split_var %in% ids_split)) %>%
        bind_rows(new_split_points)
    }
  }

  # Initialize tree as the forest part of a ranger object, rest of ranger object is added later if necessary
  tree <- forest
  tree$num.trees <- 1
  tree$child.nodeIDs <- list()
  tree$split.varIDs <- list()
  tree$split.values <- list()

  # empty ranger object that is used to build ranger object if necessary
  ranger_tree <- rf
  ranger_tree$predictions <- NA
  ranger_tree$num.trees <- 1
  ranger_tree$mtry <- NA
  ranger_tree$min.node.size <- NA
  ranger_tree$variable.importance <- NA
  ranger_tree$prediction.error <- NA
  ranger_tree$confusion.matrix <- NA
  ranger_tree$splitrule <- metric
  ranger_tree$max.depth <- NA
  ranger_tree$forest <- tree

  # ------
  # Get distance values for trees in RF (trees as columns)
  dist_val_rf <- get_distance_values(rf, metric = metric, test_data = test_data)

  # Build all possible stumps
  stumps <- list()
  dist_score_stump <- c()
  node_data_list <- list()
  keep_stump <- c()
  for(i in 1:nrow(split_points)){
    split.varIDs <- split_points[i,"split_varID"]
    split_value <- split_points[i,"split_value"]
    split.var <- split_points[i,"split_var"]

    # Add split varID and value
    tree$split.varIDs[[1]] <- c(split.varIDs, 0, 0)
    tree$split.values[[1]] <- split_value

    # Add child nodes
    tree$child.nodeIDs[[1]] <- list(c(1,0,0), c(2,0,0))

    # Add prediction
    # Get data of left and right node
    node_data <- seperate_data_nodes(split_var = split.var, split_value = split_value, node_data = train_data)

    # Check min.bucket size
    if(nrow(node_data$left) < min.bucket | nrow(node_data$right) < min.bucket){
      keep_stump[i] <- FALSE
    }else{
      keep_stump[i] <- TRUE
    }

    if(rf$treetype == "Classification"){
      prediction <- names(which.max(table(node_data$left[,dependent_varname])))
      tree$split.values[[tree$num.trees]][2] <- as.numeric(which(tree$levels == prediction))

      prediction <- names(which.max(table(node_data$right[,dependent_varname])))
      tree$split.values[[tree$num.trees]][3] <- as.numeric(which(tree$levels == prediction))

    } else if(rf$treetype == "Regression"){
      tree$split.values[[tree$num.trees]][2] <- mean(node_data$left[,dependent_varname])
      tree$split.values[[tree$num.trees]][3] <- mean(node_data$right[,dependent_varname])
    }

    # Save stumps, used data per node of stump and distance score of stump
    node_data_list[[i]] <- node_data
    stumps[[i]] <- tree
    ranger_tree$forest <- tree
    distance_values <- suppressMessages(get_distance_values(ranger_tree, metric = metric, test_data = test_data))
    dist_score_stump[i] <- get_distance_score(rf, dist_val_rf = dist_val_rf, dist_val_tree = distance_values, metric = metric, test_data = test_data)
  }

  # Check min.bucket size
  if(all(!keep_stump)){
    stop("min.bucket value is too high for your train data set")
  }else{
    node_data_list <- node_data_list[keep_stump]
    stumps <- stumps[keep_stump]
    dist_score_stump <- dist_score_stump[keep_stump]
    split_points <- split_points[keep_stump,]
  }

  # Find stump with minimal distance
  minimal_dist <- min(dist_score_stump)
  ids_minimal_dist <- which(dist_score_stump == minimal_dist)

  # If multiple ones found, use that one with maximal impurity reduction (classification) or best prediction accuracy using mse (regression)
  if(length(ids_minimal_dist)>1){
    error_stumps <- mapply(function(stump, ranger_tree, train_data){
      ranger_tree$forest <- stump
      pred <- suppressWarnings(predict(ranger_tree, data = train_data, predict.all = TRUE)$predictions)
      if(ranger_tree$treetype == "Classification"){
        true <- data.frame(char = as.character(unlist(train_data[,dependent_varname]))) %>%
          left_join(data.frame(char = stump$levels, num = c(1:length(stump$levels))), by = "char")
        return(sum(pred != true$num)/length(pred))
      } else if(ranger_tree$treetype == "Regression"){
        true <- as.numeric(train_data[,dependent_varname])
        return(sum((pred - true)^2)/length(pred))
      }
    }, stumps[ids_minimal_dist],
    MoreArgs = list(ranger_tree = ranger_tree, train_data = train_data))

    ids_minimal_dist <- ids_minimal_dist[which.min(error_stumps)][1]

    # Save stump with minimal dist and smallest error as art
    art <- stumps[[ids_minimal_dist]]
    # Save accuracy of selected stump
    error_last_tree <- min(error_stumps)
  }else{
    # Save stump with minimal dist as art
    art <- stumps[[ids_minimal_dist]]
    ranger_tree$forest <- art
    pred <- suppressWarnings(predict(ranger_tree, data = train_data, predict.all = TRUE)$predictions)

    # Get predictions and save accuracy of selected stump
    if(ranger_tree$treetype == "Classification"){
      true <- data.frame(char = as.character(unlist(train_data[,dependent_varname]))) %>%
        left_join(data.frame(char = art$levels, num = c(1:length(art$levels))), by = "char")
      error_last_tree <- sum(pred != true$num)/length(pred)
    } else if(ranger_tree$treetype == "Regression"){
      true <- as.numeric(train_data[,dependent_varname])
      error_last_tree <- sum((pred - true)^2)/length(pred)
    }
  }

  # Save which observations of train data are used in every node
  node_data_art <- list(train_data, node_data_list[[ids_minimal_dist]]$left, node_data_list[[ids_minimal_dist]]$right)

  # Check if node is pure, in this case stop splitting in that node
  splits_per_node_list <- list(split_points)
  for(i in 2:3){
    if(nrow(unique(node_data_art[[i]][dependent_varname])) == 1){
      splits_per_node_list[[i]] <- NA
    }else{
      # If node of stump is not pure, list all possible split points that could be added to that leaf
      # List with possible splits per node and remove split used in root in daughter nodes
      split_points_i <- split_points[-ids_minimal_dist,]
      # Check for other potential splits if data is passed to child nodes, if that split would be used
      # Check if number of observations on nodes is not smaller than min.bucket otherwise remove that potential split
      number_splits <- nrow(split_points_i)
      remove_split <- rep(FALSE, number_splits)
      node_data <- node_data_art[[i]]
      for(splits in 1:number_splits){
        labels_splits <- as.numeric(node_data[split_points_i[splits,"split_var"]][,1])
        data_left <- labels_splits <= split_points_i[splits,"split_value"]
        data_right <- labels_splits  > split_points_i[splits,"split_value"]
        # Remove split if no data is passed to left or right daughter node
        if(!(any(data_left) & any(data_right))){
          remove_split[splits] <- TRUE
          # Remove split point regarding min bucket size
        }else if(sum(data_left)<min.bucket | sum(data_right)<min.bucket){
          remove_split[splits] <- TRUE
        }
      }
      if(all(remove_split)){
        splits_per_node_list[[i]] <- NA
      }else if(any(remove_split)){
        splits_per_node_list[[i]] <- split_points_i[!remove_split,]
      }else{
        splits_per_node_list[[i]] <- split_points_i
      }
    }
  }

  # Save distance of used stump (art)
  dist_last_tree <- minimal_dist

  # ------
  # Add more nodes
  # List all node ids to which nodes could be added
  ranger_tree$forest <- art
  # New possible art(s) will be saved in ranger_temp
  ranger_temp <- ranger_tree

  while(TRUE){
    # Build new art
    treeinfo_art <- treeInfo(ranger_temp)
    # Count splits and stop growing tree if num.splits is reached
    number_splits <- nrow(treeinfo_art %>% filter(!terminal))
    if(!is.null(num.splits)){
      if(number_splits == num.splits){
        return(ranger_tree)
      }
    }

    node_ids_addable <- treeinfo_art %>% filter(terminal) %>% select(nodeID) %>% unlist() +1 # IDs are counted 0,1,...
    max_node <- max(treeinfo_art$nodeID)+1
    # Build all possible splits combinations added for every terminal node
    possible_trees <- list()
    node_data_list <- list()
    dist_score_trees <- c()
    split_point_list <- list()
    for(node in node_ids_addable){

      split_points <- splits_per_node_list[[node]]
      # Check if at least one split point is useable (not NA)
      if(all(!is.na(split_points))){
        # Add all splits from list
        for(j in 1:nrow(split_points)){
          split_point_list <- c(split_point_list, list(split_points))
          tree <- art

          split.varIDs <- split_points[j,"split_varID"]
          split_value <- split_points[j,"split_value"]
          split.var <- split_points[j,"split_var"]

          # Add split varID
          tree$split.varIDs[[1]] <- c(tree$split.varIDs[[1]], 0, 0)
          tree$split.varIDs[[1]][[node]] <- split.varIDs

          # Add child nodes
          # Odd node ids in list 1
          # Even node ids in list 2
          tree$child.nodeIDs[[1]][[1]] <- c(tree$child.nodeIDs[[1]][[1]], 0, 0)
          tree$child.nodeIDs[[1]][[1]][node] <- max_node
          tree$child.nodeIDs[[1]][[2]] <- c(tree$child.nodeIDs[[1]][[2]], 0, 0)
          tree$child.nodeIDs[[1]][[2]][node] <- max_node+1

          # Add new split value
          tree$split.values[[1]][[node]] <- split_value

          # Add prediction (saved in split value)
          # Get data of left and right node
          splitted_data <- seperate_data_nodes(split_var = split.var, split_value = split_value, node_data = node_data_art[[node]])

          if(rf$treetype == "Classification"){
            prediction <- names(which.max(table(splitted_data$left[,dependent_varname])))
            tree$split.values[[1]][max_node+1] <- as.numeric(which(tree$levels == prediction))

            prediction <- names(which.max(table(splitted_data$right[,dependent_varname])))
            tree$split.values[[1]][max_node+2] <- as.numeric(which(tree$levels == prediction))

          }else{
            tree$split.values[[1]][max_node+1] <- mean(splitted_data$left[,dependent_varname])
            tree$split.values[[1]][max_node+2] <- mean(splitted_data$right[,dependent_varname])
          }
          # Add possible tree to list
          possible_trees <- c(possible_trees, list(tree))

          # Save data passed to nodes
          node_data_list <- c(node_data_list, list(splitted_data))

          # Save distance to RF
          ranger_temp$forest <- tree
          distance_values <- suppressMessages(get_distance_values(ranger_temp, metric = metric, test_data = test_data))
          dist_score_trees <- c(dist_score_trees, get_distance_score(rf, dist_val_rf = dist_val_rf, dist_val_tree = distance_values, metric = metric, test_data = test_data))
        }
      }
    }
    # Stop if no splits can be added (no possible trees existing)
    if(length(possible_trees)==0){
      return(ranger_tree)
    }

    # Find tree(s) with minimal distance
    minimal_dist <- min(dist_score_trees)
    ids_minimal_dist <- which(dist_score_trees == minimal_dist)

    # Stop growing ART if dist gets bigger or stays the same with no better accuracy
    if(minimal_dist > dist_last_tree){
      return(ranger_tree)
    }

    # If multiple ones found, use that one with maximal impurity reduction (classification) or
    # best prediction accuracy using mse (regression)
    if(length(ids_minimal_dist)>1){
      error_trees <- mapply(function(tree, ranger_temp, train_data){
        ranger_temp$forest <- tree
        pred <- suppressWarnings(predict(ranger_temp, data = train_data, predict.all = TRUE)$predictions)
        if(ranger_temp$treetype == "Classification"){
          true <- data.frame(char = as.character(unlist(train_data[,dependent_varname]))) %>%
            left_join(data.frame(char = tree$levels, num = c(1:length(tree$levels))), by = "char")
          return(sum(pred != true$num)/length(pred))
        } else if(ranger_temp$treetype == "Regression"){
          true <- as.numeric(train_data[,dependent_varname])
          return(sum((pred - true)^2)/length(pred))
        }
      }, possible_trees[ids_minimal_dist],
      MoreArgs = list(ranger_temp = ranger_temp, train_data = train_data))

      ids_minimal_dist <- ids_minimal_dist[which.min(error_trees)][1]

      # Save stump with minimal dist and smallest error as art
      art <- possible_trees[[ids_minimal_dist]]
      # Save accuracy of selected stump
      error_tree <- min(error_trees)
    }else{
      # Save stump with minimal dist as art
      art <- possible_trees[[ids_minimal_dist]]
      ranger_temp$forest <- art
      pred <- suppressWarnings(predict(ranger_temp, data = train_data, predict.all = TRUE)$predictions)
      # Get prediction and save accuracy of selected stump
      if(ranger_temp$treetype == "Classification"){
        true <- data.frame(char = as.character(unlist(train_data[,dependent_varname]))) %>%
          left_join(data.frame(char = art$levels, num = c(1:length(art$levels))), by = "char")
        error_tree <- sum(pred != true$num)/length(pred)
      } else if(ranger_temp$treetype == "Regression"){
        true <- as.numeric(train_data[,dependent_varname])
        error_tree <- sum((pred - true)^2)/length(pred)
      }
    }

    # Stop growing ART if distance stays the same with no better accuracy
    if((minimal_dist == dist_last_tree & error_last_tree <= error_tree) |
       (minimal_dist == dist_last_tree & error_tree/error_last_tree > (1-epsilon))){
      return(ranger_tree)
    }

    # Save used data in all nodes
    node_data_art <- c(node_data_art, list(node_data_list[[ids_minimal_dist]]$left), list(node_data_list[[ids_minimal_dist]]$right))

    # Save used split points
    for(i in (max_node+1):(max_node+2)){
      # Check if node is pure (classification), in this case stop splitting in that node
      if(nrow(unique(node_data_art[[i]][dependent_varname])) == 1){
        splits_per_node_list[[i]] <- NA
      }else{
        split_points_i <- split_point_list[[ids_minimal_dist]]
        # Check for other splits if data is passed, if split would be used
        number_splits <- nrow(split_points_i)
        remove_split <- rep(FALSE, number_splits)
        node_data <- node_data_art[[i]]

        for(splits in 1:number_splits){
          labels_splits <- as.numeric(node_data[split_points_i[splits,"split_var"]][,1])
          data_left <- labels_splits <= split_points_i[splits,"split_value"]
          data_right <- labels_splits  > split_points_i[splits,"split_value"]
          # Remove split if no data is passed to left or right daughter node
          if(!(any(data_left) & any(data_right))){
            remove_split[splits] <- TRUE
            # Remove split point regarding min bucket size
          }else if(sum(data_left)<min.bucket | sum(data_right)<min.bucket){
            remove_split[splits] <- TRUE
          }
        }

        if(all(remove_split)){
          splits_per_node_list[[i]] <- NA
        }else if(any(remove_split)){
          # If no plausible split points are left, stop splitting in that node
          if(nrow(split_points_i[!remove_split,]) == 0){
            splits_per_node_list[[i]] <- NA
          }else{
            splits_per_node_list[[i]] <- split_points_i[!remove_split,]
          }
        }else{
          splits_per_node_list[[i]] <- split_points_i
        }
      }
    }


    # Update distance and accuracy
    dist_last_tree <- minimal_dist
    error_last_tree <- error_tree

    # art from current loop saved in ranger_tree
    ranger_tree <- ranger_temp


  }
}

