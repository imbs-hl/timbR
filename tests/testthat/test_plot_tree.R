## Load libraries
library(timbR)
library(ranger)
library(testthat)
library(dplyr)
library(knitr)

## set working directory
work_dir = getwd()

## load data sets for classification and regression
class_data <- iris
regr_data <- longley %>% data.frame()


## train initial random forests
class_rf <- ranger(Species ~ ., data = class_data, num.trees = 10)
regr_rf <- ranger(Employed ~ ., data = regr_data, num.trees = 10)

class_rf_inbag <- ranger(Species ~ ., data = class_data, num.trees = 10, keep.inbag = TRUE)
regr_rf_inbag <- ranger(Employed ~ ., data = regr_data, num.trees = 10, keep.inbag = TRUE)

## build treeInfo() of forests
class_info <- treeInfo(class_rf)
regr_info <- treeInfo(regr_rf)

class_info_inbag <- treeInfo(class_rf_inbag)
regr_info_inbag <- treeInfo(regr_rf_inbag)

## test input
test_that("Test input", {
  ## missing input
  # NULL
  expect_error(plot_tree(tree_info_df = NULL, train_data_df = class_data, rf_list = class_rf,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = "abc"))
  expect_error(plot_tree(tree_info_df = class_info_inbag, train_data_df = NULL, rf_list = class_rf,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = "abc"))
  expect_error(plot_tree(tree_info_df = class_info_inbag, train_data_df = class_data, rf_list = NULL,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = "abc"))
  expect_error(plot_tree(tree_info_df = class_info_inbag, train_data_df = class_data, rf_list = class_rf,
                         tree_number = NULL, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = "abc"))
  expect_error(plot_tree(tree_info_df = class_info_inbag, train_data_df = class_data, rf_list = class_rf,
                         tree_number = 1, dependent_var = NULL,
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = "abc"))
  expect_error(plot_tree(tree_info_df = class_info_inbag, train_data_df = class_data, rf_list = class_rf,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = NULL, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = "abc"))
  expect_error(plot_tree(tree_info_df = class_info_inbag, train_data_df = class_data, rf_list = class_rf,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = NULL,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = "abc"))
  expect_error(plot_tree(tree_info_df = class_info_inbag, train_data_df = class_data, rf_list = class_rf,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = NULL, hor_sep = 25,
                         work_dir = work_dir, plot_name = "abc"))
  expect_error(plot_tree(tree_info_df = class_info_inbag, train_data_df = class_data, rf_list = class_rf,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = NULL,
                         work_dir = work_dir, plot_name = "abc"))
  expect_error(plot_tree(tree_info_df = class_info_inbag, train_data_df = class_data, rf_list = class_rf,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = NULL, plot_name = "abc"))
  expect_error(plot_tree(tree_info_df = class_info_inbag, train_data_df = class_data, rf_list = class_rf,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = NULL))
  # NA
  expect_error(plot_tree(tree_info_df = NA, train_data_df = class_data, rf_list = class_rf,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = "abc"))
  expect_error(plot_tree(tree_info_df = class_info_inbag, train_data_df = NA, rf_list = class_rf,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = "abc"))
  expect_error(plot_tree(tree_info_df = class_info_inbag, train_data_df = class_data, rf_list = NA,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = "abc"))
  expect_error(plot_tree(tree_info_df = class_info_inbag, train_data_df = class_data, rf_list = class_rf,
                         tree_number = NA, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = "abc"))
  expect_error(plot_tree(tree_info_df = class_info_inbag, train_data_df = class_data, rf_list = class_rf,
                         tree_number = 1, dependent_var = NA,
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = "abc"))
  expect_error(plot_tree(tree_info_df = class_info_inbag, train_data_df = class_data, rf_list = class_rf,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = NA, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = "abc"))
  expect_error(plot_tree(tree_info_df = class_info_inbag, train_data_df = class_data, rf_list = class_rf,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = NA,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = "abc"))
  expect_error(plot_tree(tree_info_df = class_info_inbag, train_data_df = class_data, rf_list = class_rf,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = NA, hor_sep = 25,
                         work_dir = work_dir, plot_name = "abc"))
  expect_error(plot_tree(tree_info_df = class_info_inbag, train_data_df = class_data, rf_list = class_rf,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = NA,
                         work_dir = work_dir, plot_name = "abc"))
  expect_error(plot_tree(tree_info_df = class_info_inbag, train_data_df = class_data, rf_list = class_rf,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = NA, plot_name = "abc"))
  expect_error(plot_tree(tree_info_df = class_info_inbag, train_data_df = class_data, rf_list = class_rf,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = NA))

  ## wrong input
  # tree_info_df
  expect_error(plot_tree(tree_info_df = regr_info, train_data_df = class_data, rf_list = class_rf,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = "abc"))
  expect_error(plot_tree(tree_info_df = 123, train_data_df = class_data, rf_list = class_rf,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = "abc"))
  expect_error(plot_tree(tree_info_df = data.frame(), train_data_df = class_data, rf_list = class_rf,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = "abc"))
  # train_data_df
  expect_error(plot_tree(tree_info_df = class_info_inbag, train_data_df = regr_data, rf_list = class_rf,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = "abc"))
  expect_error(plot_tree(tree_info_df = class_info_inbag, train_data_df = 123, rf_list = class_rf,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = "abc"))
  expect_error(plot_tree(tree_info_df = class_info_inbag, train_data_df = data.frame(), rf_list = class_rf,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = "abc"))
  # rf_list
  expect_error(plot_tree(tree_info_df = class_info_inbag, train_data_df = class_data, rf_list = regr_rf,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = "abc"))
  expect_error(plot_tree(tree_info_df = class_info_inbag, train_data_df = class_data, rf_list = 123,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = "abc"))
  # tree_number
  expect_error(plot_tree(tree_info_df = class_info, train_data_df = class_data, rf_list = class_rf,
                         tree_number = 0, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = "abc"))
  expect_error(plot_tree(tree_info_df = class_info, train_data_df = class_data, rf_list = class_rf,
                         tree_number = 1000, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = "abc"))
  expect_error(plot_tree(tree_info_df = class_info, train_data_df = class_data, rf_list = class_rf,
                         tree_number = "1", dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = "abc"))
  # dependent_var
  expect_error(plot_tree(tree_info_df = class_info, train_data_df = class_data, rf_list = class_rf,
                         tree_number = 1, dependent_var = "abc",
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = "abc"))
  expect_error(plot_tree(tree_info_df = class_info, train_data_df = class_data, rf_list = class_rf,
                         tree_number = 1, dependent_var = 123,
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = "abc"))
  # show_sample_size
  expect_error(plot_tree(tree_info_df = class_info, train_data_df = class_data, rf_list = class_rf,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = TRUE, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = "abc"))
  expect_error(plot_tree(tree_info_df = class_info_inbag, train_data_df = class_data, rf_list = class_rf_inbag,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = 123, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = "abc"))
  # show_prediction_nodes
  expect_error(plot_tree(tree_info_df = class_info, train_data_df = class_data, rf_list = class_rf,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = TRUE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = "abc"))
  expect_error(plot_tree(tree_info_df = class_info_inbag, train_data_df = class_data, rf_list = class_rf_inbag,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = 123,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = "abc"))
  # vert_sep
  expect_error(plot_tree(tree_info_df = class_info, train_data_df = class_data, rf_list = class_rf,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = "25", hor_sep = 25,
                         work_dir = work_dir, plot_name = "abc"))
  # hor_sep
  expect_error(plot_tree(tree_info_df = class_info, train_data_df = class_data, rf_list = class_rf,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = "25",
                         work_dir = work_dir, plot_name = "abc"))
  # work_dir
  expect_error(plot_tree(tree_info_df = class_info, train_data_df = class_data, rf_list = class_rf,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = 123, plot_name = "abc"))
  expect_error(plot_tree(tree_info_df = class_info, train_data_df = class_data, rf_list = class_rf,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = "123", plot_name = "abc"))
  # plot_name
  expect_error(plot_tree(tree_info_df = class_info, train_data_df = class_data, rf_list = class_rf,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = 123))
})












