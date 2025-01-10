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
#' @return
#'   \item{\code{}}{mean distance of tree to random forest}
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
#' rf.iris <- ranger(Species ~ ., data = iris, write.forest=TRUE, num.trees = 10)
#' tree.iris <- ranger(Species ~ ., data = iris, write.forest=TRUE, num.trees = 1, mtry = 3)
#'
#' #
#' dist_values_rf_sv <- get_distance_values(rf = rf.iris, metric = "splitting variables")
#' dist_values_rf_tree_sv <- get_distance_values(rf = tree.iris, metric = "splitting variables")
#' get_distance_score(rf = rf.iris, dist_val_rf = dist_values_rf_sv, dist_val_tree = dist_values_rf_tree_sv, metric = "splitting variables", test_data = NULL)
#'
#' dist_values_rf_wsv <- get_distance_values(rf = rf.iris, metric = "weighted splitting variables")
#' dist_values_rf_tree_wsv <- get_distance_values(rf = tree.iris, metric = "weighted splitting variables")
#' get_distance_score(rf = rf.iris, dist_val_rf = dist_values_rf_wsv, dist_val_tree = dist_values_rf_tree_wsv, metric = "weighted splitting variables", test_data = NULL)
#'
#' dist_values_rf_pred <- get_distance_values(rf = rf.iris, metric = "prediction", test_data = iris)
#' dist_values_rf_tree_pred <- get_distance_values(rf = tree.iris, metric = "prediction", test_data = iris)
#' get_distance_score(rf = rf.iris, dist_val_rf = dist_values_rf_pred, dist_val_tree = dist_values_rf_tree_pred, metric = "prediction", test_data = iris)
#'

get_distance_score <- function(rf, dist_val_rf, dist_val_tree, metric, test_data = NULL){

  # Check inputs ----
  if (!checkmate::testClass(rf, "ranger")){
    stop("rf must be of class ranger")
  }
  if (!checkmate::testList(rf$forest)){
    stop("rf must be trained using write.forest = TRUE.")
  }
  if (!checkmate::testChoice(metric,
                             choices = c("splitting variables", "weighted splitting variables", "prediction"))){
    stop(paste("metric has to be from c('splitting variables', 'weighted splitting variables', 'prediction')."))
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
}
