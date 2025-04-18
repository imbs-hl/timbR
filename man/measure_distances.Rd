% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/measure_distances.R
\name{measure_distances}
\alias{measure_distances}
\title{Measure pair-wise distances between trees of a random forest}
\usage{
measure_distances(rf, metric = "splitting variables", test_data = NULL)
}
\arguments{
\item{rf}{Object of class \code{ranger} used with \code{write.forest = TRUE}.}

\item{metric}{Specification of the tree metric. Available are "splitting variables",
"weighted splitting variables", "terminal nodes" and "prediction".}

\item{test_data}{Additional data set comparable to the data set \code{rf} was build on.}
}
\value{
\item{\code{distances}}{matrix of size \code{num.trees}x\code{num.trees}}
}
\description{
\code{measure_distances} uses tree-based distance metrics to calculate
standardized values for pair-wise dissimilarity of trees in a random
forest trained with \code{ranger}.
}
\examples{
require(ranger)
require(timbR)

set.seed(12345)
# Train random forest with ranger
rg.iris <- ranger(Species ~ ., data = iris, write.forest=TRUE, num.trees = 10)

#
measure_distances(rf = rg.iris, metric = "splitting variables")
measure_distances(rf = rg.iris, metric = "weighted splitting variables")
measure_distances(rf = rg.iris, metric = "terminal nodes", test_data = iris)
measure_distances(rf = rg.iris, metric = "prediction", test_data = iris)

}
\author{
Lea Louisa Kronziel, M.Sc.
}
