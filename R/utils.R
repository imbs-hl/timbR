## Extract all combinations of splitting variables and splitting values used in a random forest
extract_splitinfo <- function(rf){
  splitinfo <- lapply(1:rf$num.trees, function(x){
    ## Extract info for tree x
    tree <- treeInfo(rf, x)
    ## Exclude leafs
    tree <- tree[-which(is.na(tree$splitvarID)),]
    return(paste(tree$splitvarName, tree$splitval, sep = "_"))
  })

  splitinfo <- unique(unlist(splitinfo))
  splitinfo <- sort(splitinfo)

  return(splitinfo)
}

