% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/select_trees.R
\name{select_trees}
\alias{select_trees}
\title{Select most representative trees of a random forest}
\usage{
select_trees(rf, num.trees = NULL, distance.matrix = NULL)
}
\arguments{
\item{rf}{Object of class \code{ranger} used with \code{write.forest = TRUE} to select trees from.}

\item{num.trees}{Number of trees to be selected from \code{rf}.}

\item{distance.matrix}{Add matrix of precalculated distances.}
}
\value{
\item{\code{rep.trees}}{\code{ranger} object containing the most representative trees}
}
\description{
\code{select_trees} uses pair-wise dissimilarity of trees in a random
forest trained with \code{ranger} to identify the most representative trees
from the ensemble
}
\examples{
require(ranger)
require(timbR)

## Train random forest with ranger
rg.iris <- ranger(Species ~ ., data = iris, write.forest=TRUE, num.trees = 10)

## Calculate pair-wise distances for all trees
distances <- measure_distances(rf = rg.iris, metric = "splitting variables")

## Select 5 most representative trees
rg.iris.rep <- select_trees(rf = rg.iris, num.trees = 5, distance.matrix = distances)


}
\author{
Dr. Bjoern-Hergen Laabs
}
