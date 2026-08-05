#' \code{select_trees_clust} uses pair-wise dissimilarity of trees in a random
#' forest trained with \code{ranger} to identify the most representative trees
#' from the ensemble
#'
#' @title Select most representative trees of a random forest
#'
#' @param rf              Object of class \code{ranger} used with \code{write.forest = TRUE} to select trees from.
#' @param num.trees       Number of trees to be selected from \code{rf}.
#' @param distance.matrix Add matrix of precalculated distances.
#' @param plot            Logical, whether dendrogramm of \code{distance matrix} should be plotted.
#' @param method          Method used for hierarchical clustering using \code{\link[stats]{hclust}}.
#'
#' @author Gero Szepannek
#' @return
#'   \item{\code{rep.trees}}{\code{ranger} object containing the most representative trees}
#' @export select_trees_clust
#'
#' @examples
#' require(ranger)
#' require(timbR)
#'
#' ## Train random forest with ranger
#' rg.iris <- ranger(Species ~ ., data = iris, write.forest=TRUE, num.trees = 50)
#'
#' ## Calculate pair-wise distances for all trees
#' distances <- measure_distances(rf = rg.iris, metric = "splitting variables")
#'
#' ## Select 5 most representative trees
#' rg.iris.rep <- select_trees_clust(rf = rg.iris, num.trees = 2, distance.matrix = distances)
#'
#'

select_trees_clust <- function(rf, num.trees = NULL, distance.matrix = NULL, plot = TRUE, method = "complete"){

  distances <- NULL
  for (i in 1:(ncol(distance.matrix) - 1)) distances <- c(distances, distance.matrix[(i + 1):nrow(distance.matrix), i])
  attr(distances, "Labels") <- colnames(distance.matrix)
  attr(distances, "Size")   <- ncol(distance.matrix)
  attr(distances, "Metric") <- "tree dissimilarity"
  class(distances) <- "dissimilarity"

  dendro <- hclust(distances, method = method)
  if(plot) plot(dendro)
  clusts <- cutree(dendro, num.trees)

  # assign trees to clusters
  dmats      <- list()
  tree.ids   <- list()
  for(i in seq(along.with = sort(unique(clusts)))){
    tree.ids[[i]] <- which(clusts == i)
    dmats[[i]]    <- distance.matrix[clusts == i, clusts == i]
  }

  # select one tree per cluster
  within.cl.dists <- lapply(dmats, function(z){if(length(dim(z))>1){z <- rowSums(z)}; return(z)})
  within.ids <- sapply(within.cl.dists, function(z) which.min(z)[1]) # REM: if several candidates, the first one is picked

  selected.treeids <- sapply(seq(along.with = within.ids), function(j) tree.ids[[j]][within.ids[j]])
  return(selected.treeids)

  # ## Reduce rf to most representative trees
  # rf_rep <- rf
  # rf_rep$num.trees <- num.trees
  # rf_rep$forest$num.trees <- num.trees
  # rf_rep$forest$child.nodeIDs <- rf_rep$forest$child.nodeIDs[rep_trees_idx]
  # rf_rep$forest$split.varIDs  <- rf_rep$forest$split.varIDs[rep_trees_idx]
  # rf_rep$forest$split.values  <- rf_rep$forest$split.values[rep_trees_idx]
  #
  # if(length(rf$inbag.counts) > 0){
  #   rf_rep$inbag.counts <- rf_rep$inbag.counts[rep_trees_idx]
  # }
  #
  # rf_rep$predictions      <- NULL
  # rf_rep$prediction.error <- NULL
  #
  # ## Return reduced ranger object
  # return(rf_rep)

}


