#' Generates pdf document with plot from decision tree (classification or regression)
#' @param tree_info_df            Data frame containing information about the structure of the decision tree, which is built like a "treeInfo()" data frame from the package "ranger"
#' @param train_data_df           Data frame of the training data with which the random forest was trained
#' @param test_data_df            Data frame of the test data (only needed, if show_coverage = TRUE)
#' @param rf_list                 Random forest, which is built like the one you get from ranger()
#' @param tree_number             Number of the decision tree of the rf_list to be displayed
#' @param dependent_var           Name of the column of the dependent variable in training data
#' @param show_sample_size        Option to display percentage of observations that reach nodes during training, inbag data must be available (TRUE or FALSE, TRUE could be time consuming)
#' @param show_prediction_nodes   Option to display prediction in all nodes, inbag data must be available (TRUE or FALSE, TRUE could be time consuming)
#' @param show_uncertainty        Option to display uncertainty quantification in terminal nodes (for now only available for regression)
#' @param show_coverage           Option to display marginal coverage (only in combination with show_uncertainty = TRUE)
#' @param vert_sep                Vertical spacing of nodes in mm (parameter from Latex package "forest")
#' @param hor_sep                 Horizontal spacing of nodes in mm (parameter from Latex package "forest")
#' @param work_dir                Path where plot should be saved
#' @param plot_name               Plot name
#' @param colors                  Vector with color names with one entry for each node, so for each row in tree_info_df
#' @returns                       PDF document with plot
#'
#' @author Lea Louisa Kronziel, M.Sc.
#'
#' @import dplyr
#' @import checkmate
#' @import knitr
#' @import tinytex
#' @import data.table
#'
#' @export plot_tree
#'
#' @examples
#' require(dplyr)
#' require(knitr)
#' require(tinytex)
#' require(ranger)
#' require(timbR)
#'
#' ## Specify the path to the folder where the plot should be saved
#' work_dir <- getwd()
#'
#' data(iris)
#'
#' set.seed(12345)
#' ## Train random forest with ranger
#' rf_iris <- ranger(Species ~ ., data = iris, write.forest=TRUE, num.trees = 10, min.node.size = 70)
#' ## Get the treeInfo() of the first tree
#' treeinfo_iris <- treeInfo(rf_iris)
#'
#' ## Plot the first tree
#' timbR::plot_tree(tree_info_df = treeinfo_iris, train_data_df = iris, test_data_df = iris, rf_list = rf_iris,
#'                  dependent_var = "Species", work_dir = work_dir, plot_name = "example_plot")


plot_tree <- function(tree_info_df, train_data_df, test_data_df = NULL, rf_list, tree_number = 1, dependent_var,
                      show_sample_size = FALSE, show_prediction_nodes = FALSE, show_uncertainty = FALSE, show_coverage = FALSE,
                      vert_sep = 25, hor_sep = 25,
                      work_dir, plot_name, colors = NULL){

  ## Check inputs ----
  # any input is NULL
  if(any(is.null(tree_info_df), is.null(train_data_df), is.null(rf_list), is.null(tree_number), is.null(dependent_var),
         is.null(show_sample_size), is.null(show_prediction_nodes),
         is.null(vert_sep), is.null(hor_sep),
         is.null(work_dir), is.null(plot_name))){
    stop("One of the input parameters is NULL.")
  }

  # any input is NA
  if(any(is.na(tree_number), is.na(dependent_var),
         is.na(show_sample_size), is.na(show_prediction_nodes),
         is.na(vert_sep), is.na(hor_sep),
         is.na(work_dir), is.na(plot_name))){
    stop("One of the input parameters is NA.")
  }

  # any input is a false class
  if(!checkmate::test_class(rf_list, "ranger")){
    stop("rf_list must be of class ranger.")
  }
  if(!checkmate::test_class(tree_info_df, "data.frame")){
    stop("tree_info_df muste be a data.frame.")
  }
  if(!checkmate::test_class(train_data_df, "data.frame")){
    stop("train_data_df muste be a data.frame.")
  }
  if(!checkmate::test_class(dependent_var, "character")){
    stop("dependent_var must be the name of the dependent variable in the training dataset.")
  }
  if(!(checkmate::test_class(vert_sep, "numeric") | checkmate::test_class(vert_sep, "double") | checkmate::test_class(vert_sep, "integer"))){
    stop("vert_sep must be numerical.")
  }
  if(!(checkmate::test_class(hor_sep, "numeric") | checkmate::test_class(hor_sep, "double") | checkmate::test_class(hor_sep, "integer"))){
    stop("hor_sep must be numerical.")
  }
  if(!(checkmate::test_class(tree_number, "numeric") | checkmate::test_class(tree_number, "double") | checkmate::test_class(tree_number, "integer"))){
    stop("tree_number must be numerical.")
  }
  if(!(checkmate::test_class(show_sample_size, "logical"))){
    stop("show_sample_size must be logical.")
  }
  if(!(checkmate::test_class(show_prediction_nodes, "logical"))){
    stop("show_prediction_nodes must be logical.")
  }
  if(!checkmate::test_class(plot_name, "character")){
    stop("plot_name must a character.")
  }

  # rf_list
  if(rf_list$treetype != "Classification" & rf_list$treetype != "Regression"){
    stop("It must be a random forest for regression or classification.")
  }
  if(any(colnames(tree_info_df) %in% rf_list$forest$independent.variable.names)){
    stop("At least one variable in tree_info_df is not a variable of rf_list.")
  }

  # tree_info_df
  if (any(!(c("nodeID", "leftChild","rightChild",  "splitvarID",
              "splitvarName","splitval", "terminal","prediction") %in% colnames(tree_info_df)))){
    stop("tree_info_df must be built like treeInfo() from ranger.")
  }
  if (nrow(tree_info_df) < 2){
    stop("tree_info_df must have at least two entries.")
  }
  if (!any(rf_list$forest$independent.variable.names %in% na.omit(unique(tree_info_df$splitvarName)))){
    stop("All used variables in rf_list must be included in the training dataset.")
  }
  # train_data_df
  if (nrow(train_data_df) == 0){
    stop("train_data_df must not be empty.")
  }
  if (!all(na.omit(unique(tree_info_df$splitvarName)) %in% colnames(train_data_df))){
    stop("All split variables from tree_info_df must be included in the training dataset.")
  }

  # tree_number
  if(tree_number > rf_list$num.trees | tree_number < 1){
    stop("The tree_number must not be greater than the number of trees in the random forest or smaller than 1.")
  }

  # dependent_var
  if(!dependent_var %in% colnames(train_data_df)){
    stop("dependent_var is not present in the training data set.")
  }

  # show_sample_size, show_prediction_nodes
  if(is.null(rf_list$inbag.counts) & (show_sample_size == TRUE | show_prediction_nodes == TRUE)){
    stop("For show_sample_size = TRUE or show_prediction_nodes = TRUE, inbag.counts must be present in ranger (set keep.inbag = TRUE).")
  }
  # show_uncertainty, show_coverage
  if(!show_uncertainty & show_coverage){
    stop("For show_coverage = TRUE, show_uncertainty has to be TRUE set is needed.")
  }
  if(is.null(test_data_df) & (show_coverage == TRUE)){
    stop("For show_coverage = TRUE, test_data set is needed.")
  }

  # test_data_df
  if(!is.null(test_data_df)){
    if(!checkmate::test_class(test_data_df, "data.frame")){
      stop("test_data_df muste be a data.frame.")
    }
    if(!dependent_var %in% colnames(test_data_df)){
      stop("dependent_var has to be a column of test_data_df.")
    }
  }

  # work_dir
  if (!file.exists(work_dir)){
    stop("Path to work_dir does not exist.")
  }

  # plot_name
  if (plot_name == ""){
    stop("Please enter a name for the plot_name.")
  }

  # colors
  if (!is.null(colors) & length(colors)!= nrow(tree_info_df)){
    stop("Please insert a vector with a color for each node including terminal nodes ( = number of rows in tree_info_df).")
  }

  ## Plot tree ----

  # Generate Latex code for the plot of the tree
  tree_code <- paste0("[",
                      tree_to_text(node_id = 0,
                                   tree_info_df = tree_info_df,
                                   train_data_df = train_data_df,
                                   test_data_df = test_data_df,
                                   rf_list = rf_list,
                                   tree_number = tree_number,
                                   dependent_var = dependent_var,
                                   show_sample_size = show_sample_size,
                                   show_prediction_nodes = show_prediction_nodes,
                                   show_uncertainty = show_uncertainty,
                                   show_coverage = show_coverage,
                                   vert_sep = vert_sep,
                                   hor_sep = hor_sep,
                                   colors = colors),
                      "]")

  # Code for the Latex document
  template = r"(
  \documentclass[tikz, border=2mm]{standalone}
  \usepackage[edges]{forest}
  \usepackage[english]{babel}
  \usepackage{amsfonts}
  \definecolor{oceangreen_uzl}{RGB}{0,75,90}
  \definecolor{imbs_orange}{RGB}{203,81,25}
  \begin{document}
  \begin{forest}
  for tree={draw,
  forked edge},
  where n children=0{tier=word}{}
  %% begin.rcode test_leaf, results='asis', echo=FALSE, cache=FALSE
  % cat(tree_code)
  %% end.rcode
  \end{forest}
  \end{document}
  )"

  tex_datei = knit(text = template)
  temp_tex_path <- file.path(tempdir(), "tex_temp.tex")


  # Knit Latex code in temporary document
  tex_datei <-  knit(text = template)
  temp_tex_path <- file.path(tempdir(), "tex_temp.tex")

  write(tex_datei, temp_tex_path)

  # save plot as PDF document
  pdflatex(temp_tex_path, pdf_file = file.path(work_dir, paste0(plot_name, ".pdf")), clean = TRUE)

  # print where plot is saved
  print(paste0("Your plot is saved here: ", work_dir, paste0("/", plot_name, ".pdf")))
}
