#' \code{seperate_data_nodes} splits the input data using the inserted split variable into
#' two sub-data sets like a stump of a decision tree using the inserted split value.
#'
#'
#' @param split_var      Name of variable that is used to split the data.
#' @param split_value    Split value of variable that is used to split the data.
#' @param node_data      Data set that should be splitted.
#'
#' @author Lea Louisa Kronziel, M.Sc.
#' @return
#'   \item{}{list containing data frames, first entry for left daughter node, second entry for right daughter node}
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
#' rf.iris <- ranger(Species ~ ., data = iris, write.forest=TRUE, num.trees = 10)
#' rf.iris.treeInfo <- treeInfo(rf.iris)
#' # Get data of daughter nodes of first tree
#' seperated_data <- seperate_data_nodes(rf.iris.treeInfo$splitvarName[1], rf.iris.treeInfo$splitval[1], iris)
#'

seperate_data_nodes <- function(split_var, split_value, node_data){
  ## Split node_data by splitpoint like it's done in ranger package
  node_data_left <- node_data[as.numeric(node_data[,split_var]) <= split_value,]
  node_data_right <- node_data[as.numeric(node_data[,split_var]) > split_value,]

  return(list(left = node_data_left, right = node_data_right))
}
