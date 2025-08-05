#' Get brier score for predicted probabilities
#' @param predictions            2D object with predictions (data.frame or matrix), observations as rows, classes as columns
#' @param true_class_labels      Vector with true class labels
#' @author Lea Louisa Kronziel, M.Sc.
#' @returns                     Brier score as numerical value


get_brier_score <- function(predictions, true_class_labels){
  # Number of observations and classes
  n <- nrow(predictions)
  k <- ncol(predictions)

  # Classes as numerical indices (as done in ranger package)
  if (is.factor(true_class_labels)) {
    true_class_labels <- as.integer(true_class_labels) - 1  # 0-basiert wie in C++
  }

  # Calculate brier score
  total_error <- 0
  num_predictions <- 0

  for (i in 1:n) {
    for (j in 1:k) {
      y <- ifelse(true_class_labels[i] == (j - 1), 1, 0)
      p <- predictions[i, j]
      total_error <- total_error + (y - p)^2
    }
  }

  # Calculate mean
  brier_score <- total_error / n
  return(unname(brier_score))
}
