#' \code{cluster_trees} uses pair-wise dissimilarity of trees in a random
#' forest trained with \code{ranger} to idetify clusters of similar trees.
#'
#' @title Cluster trees based on their similarityof a random forest
#'
#' @param rf              Object of class \code{ranger} used with \code{write.forest = TRUE} to select trees from.
#' @param num.clusters    Number of clusters to seperate the trees in. Can be an integer smaller than the number of trees or "AUTO" to auto detect the best number of clusters
#' @param distance.matrix Add matrix of precalculated distances.
#'
#' @author Bjoern-Hergen Laabs, M.Sc.
#' @return
#'   \item{\code{cluster_ids}}{List of numerical vectors where each entry represents the tree ids belonging to one cluster}
#' @export cluster_trees
#'
#' @examples
#' require(ranger)
#' require(timbR)
#' require(mlr)
#' require(clue)
#' require(clusterSim)
#'
#' ## Train random forest with ranger
#' rg.iris <- ranger(Species ~ ., data = iris, write.forest=TRUE, num.trees = 500)
#'
#' ## Calculate pair-wise distances for all trees
#' distances <- measure_distances(rf = rg.iris, metric = "weighted splitting variables")
#'
#' ## Cluster trees in two clusters
#' clustered_trees <- cluster_trees(rf = rg.iris, num.clusters = 2, distance.matrix = distances)
#'
#' ## Auto cluster trees in clusters
#' clustered_trees <- cluster_trees(rf = rg.iris, num.clusters = "AUTO", distance.matrix = distances)
#'
#' ## Plot clustered trees colored by cluster in principal component space
#' plot(clustered_trees$PC1, clustered_trees$PC2, col = clustered_trees$cluster)
#'
cluster_trees <- function(rf, num.clusters = NULL, distance.matrix = NULL){

  ## Check inputs ----
  if (!checkmate::testClass(rf, "ranger")){
    stop("rf must be of class ranger.")
  }
  if(checkmate::testNull(num.clusters)){
    stop("Specify number of clusters to be selected from forest.")
  }
  if((!checkmate::testDouble(num.clusters) | !checkmate::testCount(num.clusters)) & !checkmate::testSetEqual(num.clusters, "AUTO")){
    stop("num.clusters has to be a single numerical value.")
  }
  if(checkmate::testNull(distance.matrix)){
    stop("Specify precalculated pair-wise distances.")
  }
  if(sum(dim(distance.matrix) == rf$num.trees) != 2){
    stop("Dimensions of distance matrix do not fit to ranger object.")
  }
  if(num.clusters > rf$num.trees & num.clusters != "AUTO"){
    stop("You can not select more clusters than trees in the ranger object.")
  }

## Calculate principal component analysis
  pca <- prcomp(distance.matrix, rank = 2)

if(num.clusters == "AUTO"){
  cluster.task = makeClusterTask(data = as.data.frame(pca$rotation))

  ps = makeParamSet(
    makeNumericParam("centers", lower = 1, upper = floor(sqrt(rf$num.trees)))
  )

  ctrl = makeTuneControlGrid(resolution = floor(sqrt(rf$num.trees)))

  rdesc = makeResampleDesc("CV", iters = 5L)

  res = tuneParams("cluster.kmeans",
                   task = cluster.task,
                   resampling = rdesc,
                   par.set = ps,
                   control = ctrl)


  clusters <- kmeans(x = pca$rotation, centers = res$x$centers)
} else {
  clusters <- kmeans(x = pca$rotation, centers = num.clusters)
}

output <- data.frame(cbind(pca$rotation, clusters$cluster))
names(output) <- c("PC1", "PC2", "cluster")
return(output)
}
