#' \code{get_cpd_plot_node} builds ggplot object for CPS of a terminal node of a tree
#'
#' @title Plot conformal predictive systems of a terminal node
#'
#' @param nodeID                   ID of terminal node, e.g select one nodeID of a terminal node from ranger::treeInfo()
#' @param cal_data_node            Calibration data (data frame that includes outcome variable) that reaches in that terminal node
#' @param dependent_varname        Name of the dependent variable used to create the tree.
#' @param tree                     Tree that should be used to build the CPS plot, tree should be an object of class \code{ranger}.
#' @param threshold                Vector of numerical threshhold(s) for which the probability of reaching this threshold is to be displayed in the plot; in addition, the probability of reaching at least the threshold is returned. Set to NULL if not used.
#' @param significance_level       Level of uncertainty that should be reached by calibration, should be between 0 and 1.
#' @param interval_type            Type of interval, choose either two-tailed or one-tailed
#' @param direction                Direction of one-tailed interval, choose either left-tailed or right-tailed. Set to NULL if not used.
#' @param show_point_prediction    Set to TRUE, if you want to display dashed line with point prediction for this terminal node in plot
#' @param show_prediction_interval Set to TRUE, if you want to display prediction interval for this terminal node in plot
#'
#' @author Lea Louisa Kronziel, M.Sc.
#' @return
#'   list, first element is ggplot object, second a data.frame with probabilities beeing above inserted treshold
#' @import ranger
#' @import dplyr
#' @import checkmate
#' @import data.table
#'



get_cpd_plot_node <- function(nodeID, cal_data_node, dependent_varname, tree, threshold, significance_level = 0.05,
                              interval_type = "two-tailed", direction = NULL, show_point_prediction = TRUE, show_prediction_interval = TRUE){

  # Checks for input paramter
  #------------

  if(!is.numeric(nodeID) | length(nodeID) != 1){
    stop("nodeID has to be a single numerical value of a terminal node from a tree build with ranger.")
  }
  if(!is.data.frame(cal_data_node)){
    stop("cal_data has to be a data frame that contains all calibration observations that reach that terminal node")
  }
  if(!dependent_varname%in%colnames(cal_data)){
    stop("depedent_varname has to be a variable name from calibration data")
  }
  if (!checkmate::testClass(tree, "ranger")){
    stop("tree must be of class ranger")
  }
  if(!(nodeID %in% (treeInfo(tree) %>% filter(terminal) %>% select(nodeID) %>% unlist() %>% as.numeric()))){
    stop("nodeID has to be a terminal node from a tree build with ranger, e.g. look in treeInfo of that tree to select a nodeID")
  }
  if(!(is.null(threshold) | is.numeric(threshold))){
    stop("threshold must be NULL or a vector containing numerical values")
  }
  if(!is.numeric(significance_level) | length(significance_level) != 1){
    stop("significance_level has to be a single numerical value")
  }
  if(significance_level>=1 | significance_level<=0){
    stop("significance_level has to be smaller than 1 and bigger than 0.")
  }
  if(interval_type != "two-tailed" & interval_type != "one-tailed"){
    stop("Please use 'two-tailed' or 'one-tailed' for interval type.")
  }
  if(interval_type == "one-tailed" & is.null(direction)){
    stop("Please use 'left-tailed' or 'right-tailed' for direction.")
  }
  if(interval_type == "one-tailed"){
    if(is.na(direction)){
      stop("Please use 'left-tailed' or 'right-tailed' for direction.")
    }
    if(direction != "left-tailed" & direction != "right-tailed"){
      stop("Please use 'left-tailed' or 'right-tailed' for direction.")
    }
  }
  if(!is.logical(show_point_prediction)){
    stop("show_point_prediction has to be TRUE or FALSE")
  }
  if(!is.logical(show_prediction_interval)){
    stop("show_prediction_interval has to be TRUE or FALSE")
  }

  #------------


  # Calculate cpd
  cpd_dist <- get_predictive_distribution(y_cal = cal_data_node[,dependent_varname],
                                          y_cal_pred = predict(tree, cal_data_node)$predictions,
                                          y_test_pred = predict(tree, cal_data_node)$predictions)[[1]]

  # Calculate "p-values" of cpd
  p <- seq(0, 1 - 1/length(cpd_dist), length.out = length(cpd_dist))
  data_cps <- data.frame(cpd = c(cpd_dist,max(cpd_dist+0.000001), Inf),
                         p = c(p,1,1))

  # Set limits for axis
  x_min <- min(data_cps[-1,"cpd"])
  x_max <- data_cps[data_cps$p==1,"cpd"][1]


  # Create basic plot
  plot <- ggplot() +
    theme_bw() +
    geom_line(data = data_cps, aes(x = cpd, y = p), linewidth = 1.5) +
    xlab(paste0("y = ", dependent_varname)) +
    ylab("Q(y)")+
    theme(legend.position = "bottom")+
    labs(col = "")+
    ylim(-0.0001, 1.0001) +
    xlim(x_min, x_max)

  # Display point prediction
  if(show_point_prediction){
    plot <- plot +
      geom_segment(aes(x = predict(tree, cal_data_node)$predictions, y = 0, yend = 1, color = "prediction"), linewidth = 1.5,
                   linetype = "dashed")
  }
  # Display prediction interval
  if(show_prediction_interval){
    # two-tailed
    if(interval_type == "two-tailed"){
      significance_level <- significance_level/2

      # calculate lower and upper bound
      lower_index <- tail(which(data_cps$p <= significance_level), 1)
      upper_index <- which(data_cps$p >= (1-significance_level))[1]

      low_percentile <-  data_cps$cpd[lower_index]
      high_percentile <-  data_cps$cpd[upper_index]

      data_interval <- data_cps %>%
        filter(cpd <= high_percentile & cpd >= low_percentile)

      plot <- plot +
        geom_area(data = data_interval,
                  aes(x = cpd, y = p), linewidth = 1.5, fill = "red", alpha = 0.15, position = "identity")

    }
    else if(interval_type == "one-tailed" & direction == "left-tailed"){
      # Calculate lower bound
      lower_index <- tail(which(p <= significance_level), 1)
      low_percentile <-  data_cps$cpd[lower_index]

      data_interval <- data_cps %>%
        filter(cpd >= low_percentile)

      plot <- plot +
        geom_area(data = data_interval,
                  aes(x = cpd, y = p), linewidth = 1.5, fill = "red", alpha = 0.15, position = "identity")
    }
    else if(interval_type == "one-tailed" & direction == "right-tailed"){
      # Calculate upper bound
      upper_index <- which(data_cps$p >= (1-significance_level))[1]
      high_percentile <-  data_cps$cpd[upper_index]

      data_interval <- data_cps %>%
        filter(cpd <= high_percentile)

      plot <- plot +
        geom_area(data = data_interval,
                  aes(x = cpd, y = p), linewidth = 1.5, fill = "red", alpha = 0.15, position = "identity")
    }
  }
  # Display probability for threshold(s)
  threshold_list <- list()
  if(!is.null(threshold)){
    for(i in 1:length(threshold)){
      prob_above_threshold <- data_cps$p[which(data_cps$cpd > threshold[i])[1]]
      data_threshold <- data.frame(value = threshold[i], prob = prob_above_threshold)
      # Probability to be above threshold
      threshold_list[[i]] <- data.frame(value = threshold[i], prob = 1-prob_above_threshold)
      plot <- plot +
        geom_segment(data = data_threshold,
                     aes(x = value, y = 0, yend = prob, color = paste0("Q(", value, ")")), linewidth = 1.5, show.legend = TRUE)+
        geom_segment(data = data_threshold,
                     aes(x = min(data_cps$cpd), xend = value, y = prob, color = paste0("Q(", value, ")")), linewidth = 1.5, show.legend = TRUE)
    }
  }
  plot <- plot +
    theme(text = element_text(size = 30),
          legend.text = element_text(size = 30),
          legend.title = element_text(size = 30),
          legend.key.size = unit(0.6, 'cm'),
          strip.text.y = element_text(size = 30),
          strip.text.x = element_text(size = 30))
  return(list(plot, bind_rows(threshold_list)))
}
