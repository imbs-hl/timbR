#' \code{select_trees} uses pair-wise dissimilarity of trees in a random
#' forest trained with \code{ranger} to identify the most representative trees
#' from the ensemble
#'
#' @title Select most representative trees of a random forest
#'
#' @param rf              Object of class \code{ranger} used with \code{write.forest = TRUE} to select trees from.
#' @param num.trees       Number of trees to be selected from \code{rf}.
#' @param distance.matrix Add matrix of precalculated distances.
#'
#' @author Bjoern-Hergen Laabs, M.Sc.
#' @return
#'   \item{\code{rep.trees}}{\code{ranger} object containing the most representative trees}
#' @export select_trees
#'
#' @examples
#' require(ranger)
#' require(timbR)
#'
#' ## Train random forest with ranger
#' rg.iris <- ranger(Species ~ ., data = iris, write.forest=TRUE, num.trees = 10)
#'
#' ## Calculate pair-wise distances for all trees
#' distances <- measure_distances(rf = rg.iris, metric = "splitting variables")
#'
#' ## Select 5 most representative trees
#' rg.iris.rep <- select_trees(rf = rg.iris, num.trees = 5, distance.matrix = distances)
#'
#'
select_trees <- function(rf, num.trees = NULL, distance.matrix = NULL){

  ## Check inputs ----
  if (!checkmate::testClass(rf, "ranger")){
    stop("rf must be of class ranger.")
  }
  if (!checkmate::testList(rf$forest)){
    stop("rf must be trained using write.forest = TRUE.")
  }
  if(checkmate::testNull(num.trees)){
    stop("Specify number of trees to be selected from forest.")
  }
  if(!checkmate::testDouble(num.trees) | !checkmate::testCount(num.trees)){
    stop("num.trees has to be a single numerical value.")
  }
  if(checkmate::testNull(distance.matrix)){
    stop("Specify precalculated pair-wise distances.")
  }
  if(sum(dim(distance.matrix) == rf$num.trees) != 2){
    stop("Dimensions of distance matrix do not fit to ranger object.")
  }
  ## if rf consists of only one tree, return the tree
  if(rf$num.trees==1 & num.trees == 1){
    warning("Your RF consists of only one tree, your RF is returned")
    return(rf)
  }
  if(num.trees > rf$num.trees){
    stop("You can not select more representative trees than trees in the ranger object.")
  }



  ## Calculate distance score for each tree ----
  dist_score <- rowSums(distance.matrix)

  ## Find most representative trees
  rep_trees_idx <- sort(dist_score,
                        decreasing   = FALSE,
                        index.return = TRUE)[[2]][1:num.trees]

  ## Reduce rf to most representative trees
  rf_rep <- rf
  rf_rep$num.trees <- num.trees
  rf_rep$forest$num.trees <- num.trees
  rf_rep$forest$child.nodeIDs <- rf_rep$forest$child.nodeIDs[rep_trees_idx]
  rf_rep$forest$split.varIDs  <- rf_rep$forest$split.varIDs[rep_trees_idx]
  rf_rep$forest$split.values  <- rf_rep$forest$split.values[rep_trees_idx]

  if(length(rf$inbag.counts) > 0){
    rf_rep$inbag.counts <- rf_rep$inbag.counts[rep_trees_idx]
  }

  rf_rep$predictions      <- NULL
  rf_rep$prediction.error <- NULL

  ## Return reduced ranger object
  return(rf_rep)
}
