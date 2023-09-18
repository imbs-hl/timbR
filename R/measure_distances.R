#' \code{measure_distances} uses tree-based distance metrics to calculate
#' standardized values for pair-wise dissimilarity of trees in a random
#' forest trained with \code{ranger}.
#'
#' @title Measure pair-wise distances between trees of a random forest
#'
#' @param rf         Object of class \code{ranger} used with \code{write.forest = TRUE}.
#' @param metric     Specification of the tree metric. Available are "splitting variables",
#'                   "weighted splitting variables", "terminal nodes", "prediction" and "combined".
#' @param test_data  Additional data set comparable to the data set \code{rf} was build on.
#'
#' @author Bjoern-Hergen Laabs, M.Sc.
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
#' measure_distances(rf = rg.iris, metric = "combined", test_data = iris)

## Load Functions
## TODO



measure_distances <- function(rf, metric = "splitting variables", test_data = NULL){

source(file.path("./measure_distances_functions", "measure_distances_functions.R"))

  ## Check inputs ----
  if (!checkmate::testClass(rf, "ranger")){
    stop("rf must be of class ranger")
  }
  if (!checkmate::testList(rf$forest)){
    stop("rf must be trained using write.forest = TRUE.")
  }
  if (!checkmate::testChoice(metric,
                             choices = c("splitting variables", "weighted splitting variables", "terminal nodes", "prediction", "combined"))){
    stop(paste("metric has to be from c('splitting variables', 'weighted splitting variables', 'terminal nodes', 'prediction', 'combined')."))
  }
  if (metric %in% c("terminal nodes", "prediction", "combined")){
    if (checkmate::testNull(test_data)){
      stop("You have to provide a test data set for distance measure by terminal nodes, prediction or combined.")
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
    distance <- splitting_variables_fun(rf)
  }

  ## Calculation weighted version of d0 ----
  if (metric == "weighted splitting variables"){
    distances <- weighted_dist_fun(rf)
  }

  ## Calculation for d1 of Banerjee et al. (2012) ----
  if (metric == "terminal nodes"){
    distances <- terminal_nodes_fun(rf, test_data)
  }

  ## Calculation for d2 of Banerjee et al. (2012) ----
  if (metric == "prediction"){
    distances <- prediction_dist_fun(rf, test_data)
  }

  ## Calculation of a combined distance measure : weighted splitting variables and prediction
  if (metric == "combined"){


    # Calculation of th weighted and the prediction measure

    w_dist <- weighted_dist(rf)
    p_dist <- prediction_dist(rf, test_data)

    # Calculation of the pairwise means

    distances <- (w_dist + p_dist)/2

  }


  ## Return distance matrix ----
  return(distances)
}
