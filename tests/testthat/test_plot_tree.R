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
  expect_error(plot_tree(tree_info_df = class_info_inbag, train_data_df = class_data, rf_list = class_rf,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = FALSE, show_uncertainty = TRUE,,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = NULL))
  expect_error(plot_tree(tree_info_df = regr_info_inbag, train_data_df = regr_data, rf_list = regr_rf,
                         tree_number = 1, dependent_var = "Employed",
                         show_sample_size = FALSE, show_prediction_nodes = FALSE, show_uncertainty = TRUE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = "abc"))
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
  # colors
  expect_error(plot_tree(tree_info_df = class_info, train_data_df = class_data, rf_list = class_rf,
                         tree_number = 1, dependent_var = "Species",
                         show_sample_size = FALSE, show_prediction_nodes = FALSE,
                         vert_sep = 25, hor_sep = 25,
                         work_dir = work_dir, plot_name = "abc", colors = "red"))
})


test_that("Missing mandatory arguments cause errors", {
  expect_error(plot_tree())                                                                 # no args
  expect_error(plot_tree(tree_info_df = NULL, train_data_df = iris, rf_list = rf,
                         dependent_var = "Species", work_dir = work_dir, plot_name = "p"))
  expect_error(plot_tree(tree_info_df = tree_info, train_data_df = NULL, rf_list = rf,
                         dependent_var = "Species", work_dir = work_dir, plot_name = "p"))
  expect_error(plot_tree(tree_info_df = tree_info, train_data_df = iris, rf_list = NULL,
                         dependent_var = "Species", work_dir = work_dir, plot_name = "p"))
  expect_error(plot_tree(tree_info_df = tree_info, train_data_df = iris, rf_list = rf,
                         dependent_var = NULL, work_dir = work_dir, plot_name = "p"))
  expect_error(plot_tree(tree_info_df = tree_info, train_data_df = iris, rf_list = rf,
                         dependent_var = "Species", work_dir = NULL, plot_name = "p"))
  expect_error(plot_tree(tree_info_df = tree_info, train_data_df = iris, rf_list = rf,
                         dependent_var = "Species", work_dir = work_dir, plot_name = NULL))
})

test_that("Incorrect types for parameters cause errors", {
  expect_error(plot_tree(tree_info_df = 123, train_data_df = iris, rf_list = rf,
                         dependent_var = "Species", work_dir = work_dir, plot_name = "p"))
  expect_error(plot_tree(tree_info_df = tree_info, train_data_df = 123, rf_list = rf,
                         dependent_var = "Species", work_dir = work_dir, plot_name = "p"))
  expect_error(plot_tree(tree_info_df = tree_info, train_data_df = iris, rf_list = 123,
                         dependent_var = "Species", work_dir = work_dir, plot_name = "p"))
  expect_error(plot_tree(tree_info_df = tree_info, train_data_df = iris, rf_list = rf,
                         dependent_var = 123, work_dir = work_dir, plot_name = "p"))
  expect_error(plot_tree(tree_info_df = tree_info, train_data_df = iris, rf_list = rf,
                         dependent_var = "Species", vert_sep = "25",
                         work_dir = work_dir, plot_name = "p"))
  expect_error(plot_tree(tree_info_df = tree_info, train_data_df = iris, rf_list = rf,
                         dependent_var = "Species", hor_sep = "25",
                         work_dir = work_dir, plot_name = "p"))
  expect_error(plot_tree(tree_info_df = tree_info, train_data_df = iris, rf_list = rf,
                         dependent_var = "Species", work_dir = 123,
                         plot_name = "p"))
  expect_error(plot_tree(tree_info_df = tree_info, train_data_df = iris, rf_list = rf,
                         dependent_var = "Species", work_dir = work_dir,
                         plot_name = 123))
})

test_that("Invalid hyperparameter values cause errors", {
  # tree_number out of range
  expect_error(plot_tree(tree_info_df = tree_info, train_data_df = iris, rf_list = rf,
                         tree_number = 0, dependent_var = "Species",
                         work_dir = work_dir, plot_name = "p"))
  expect_error(plot_tree(tree_info_df = tree_info, train_data_df = iris, rf_list = rf,
                         tree_number = 100, dependent_var = "Species",
                         work_dir = work_dir, plot_name = "p"))
  expect_error(plot_tree(tree_info_df = tree_info, train_data_df = iris, rf_list = rf,
                         tree_number = "one", dependent_var = "Species",
                         work_dir = work_dir, plot_name = "p"))

  # logical flags not logical
  expect_error(plot_tree(tree_info_df = tree_info, train_data_df = iris, rf_list = rf,
                         show_sample_size = 1, dependent_var = "Species",
                         work_dir = work_dir, plot_name = "p"))
  expect_error(plot_tree(tree_info_df = tree_info, train_data_df = iris, rf_list = rf,
                         show_prediction_nodes = "TRUE", dependent_var = "Species",
                         work_dir = work_dir, plot_name = "p"))
  expect_error(plot_tree(tree_info_df = tree_info, train_data_df = iris, rf_list = rf,
                         show_uncertainty = 2, dependent_var = "Species",
                         work_dir = work_dir, plot_name = "p"))
  expect_error(plot_tree(tree_info_df = tree_info, train_data_df = iris, rf_list = rf,
                         show_coverage = NA, dependent_var = "Species",
                         work_dir = work_dir, plot_name = "p"))
  expect_error(plot_tree(tree_info_df = tree_info, train_data_df = iris, rf_list = rf,
                         show_intervalwidth = "no", dependent_var = "Species",
                         work_dir = work_dir, plot_name = "p"))

  # vert_sep / hor_sep negative
  expect_error(plot_tree(tree_info_df = tree_info, train_data_df = iris, rf_list = rf,
                         vert_sep = -10, dependent_var = "Species",
                         work_dir = work_dir, plot_name = "p"))
  expect_error(plot_tree(tree_info_df = tree_info, train_data_df = iris, rf_list = rf,
                         hor_sep = -10, dependent_var = "Species",
                         work_dir = work_dir, plot_name = "p"))

  # colors wrong length
  expect_error(plot_tree(tree_info_df = tree_info, train_data_df = iris, rf_list = rf,
                         dependent_var = "Species",
                         colors = c("red"), work_dir = work_dir, plot_name = "p"))
})


test_that("test_data_df invalid type yields error", {
  expect_error(plot_tree(tree_info, iris, test_data_df = 1:5, rf, dependent_var = "Species",
                         work_dir = work_dir, plot_name = "p"))
})


test_that("significance_level must be between 0 and 1", {

  ## significance_level > 1 → Fehler
  expect_error(
    plot_tree(
      tree_info_df = class_info_inbag,
      train_data_df = class_data,
      cal_data_df = class_data,
      rf_list = class_rf_inbag,
      dependent_var = "Species",
      significance_level = 1.5,
      show_cpd = TRUE,
      work_dir = work_dir,
      plot_name = "siglev_over1"
    )
  )

  ## significance_level < 0 → Fehler
  expect_error(
    plot_tree(
      tree_info_df = class_info_inbag,
      train_data_df = class_data,
      cal_data_df = class_data,
      rf_list = class_rf_inbag,
      dependent_var = "Species",
      significance_level = -0.2,
      show_cpd = TRUE,
      work_dir = work_dir,
      plot_name = "siglev_under0"
    )
  )

})


test_that("direction is validated only for one-tailed interval", {

  # invalid direction
  expect_error(
    plot_tree(
      tree_info_df = class_info_inbag,
      train_data_df = class_data,
      cal_data_df = class_data,
      rf_list = class_rf_inbag,
      dependent_var = "Species",
      interval_type = "one-tailed",
      direction = "nonsense",
      show_cpd = TRUE,
      work_dir = work_dir,
      plot_name = "dir_invalid"
    )
  )

  # direction must be NULL for two-tailed
  expect_error(
    plot_tree(
      tree_info_df = class_info_inbag,
      train_data_df = class_data,
      cal_data_df = class_data,
      rf_list = class_rf_inbag,
      dependent_var = "Species",
      interval_type = "two-tailed",
      direction = "left-tailed",
      show_cpd = TRUE,
      work_dir = work_dir,
      plot_name = "dir_notnull_twotailed"
    )
  )
})
