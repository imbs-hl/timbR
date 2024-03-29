% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_split_criterion.R
\name{get_split_criterion}
\alias{get_split_criterion}
\title{Get split criterion (levels) for factor, character or numerical variables}
\usage{
get_split_criterion(tree_info_df, node_id, train_data_df, rf_list)
}
\arguments{
\item{tree_info_df}{Data frame containing information about the structure of the decision tree, which is built like a "treeInfo()" data frame from the package "ranger"}

\item{node_id}{Node ID of the node whose parent ID is to be determined}

\item{train_data_df}{Data frame of the training data with which the random forest was trained}

\item{rf_list}{Random forest, which is built like the one you get from ranger()}
}
\value{
Character with levels or values passed from the parent node to the considered node
}
\description{
Get split criterion (levels) for factor, character or numerical variables
}
\author{
Lea Louisa Kronziel, M.Sc.
}
