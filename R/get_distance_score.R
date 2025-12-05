#' \code{get_distance_score} calculated tree based distance score for a single tree to a random forest built with \code{ranger}.
#'
#'
#' @param rf            Object of class \code{ranger} used with \code{write.forest = TRUE}.
#' @param dist_val_rf   Distance values for the random forest created with get_distance_values()
#' @param dist_val_tree Distance values for the tree created with get_distance_values()
#' @param metric        Specification of distance (dissimilarity) metric. Available are "splitting variables",
#'                      "weighted splitting variables", and "prediction".
#' @param test_data     Additional data set comparable to the data set \code{rf} was build on. Only needed for metric "prediction".
#'
#' @author Lea Louisa Kronziel, M.sc.
#' @return mean distance of tree to random forest
#' @import ranger
#' @import dplyr
#' @import checkmate

get_distance_score <- function(rf, dist_val_rf, dist_val_tree, metric, test_data = NULL){

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

  #-----
  distance_values <- cbind(dist_val_tree,dist_val_rf)
  if (metric == "splitting variables"){
    # Extract number of features
    num_features <- rf$num.independent.variables
    # Calculate standardized pair-wise distances
    distances <- as.matrix(dist(t(distance_values), method = "euclidian"))^2 / num_features
    return(mean(distances[,1]))
  }
  if (metric == "weighted splitting variables"){
    # Extract number of features
    num_features <- rf$num.independent.variables
    # Calculate standardized pair-wise distances
    distances <- as.matrix(dist(t(distance_values), method = "euclidian"))^2
    return(mean(distances[,1]))
  }
  if (metric == "prediction"){

    # Controll if outcome is factor
    if (is.factor(rf$predictions)){
      # Calculate standardized pair-wise distances
      distances <- as.matrix(dist(t(distance_values), method = "manhattan")) / nrow(test_data)
    } else {
      # Calculate standardized pair-wise distances: quadratic euclidian distance
      distances <- as.matrix(dist(t(distance_values), method = "euclidian"))^2 / nrow(test_data)
    }
    return(mean(distances[,1]))
  }
  if(metric == "terminal nodes"){

    # terminal nodes of rf in dist_val_rf, those of tree in dist_val_tree
    terminal_nodes <- cbind(dist_val_tree, dist_val_rf)

    # measure_distances

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

    return(mean(distances[,1]))
  }

}
