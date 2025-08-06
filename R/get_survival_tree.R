# Funktion zur Berechnung der Nelson-Aalen-CHF
compute_chf_node <- function(time, status, death_times) {
  sapply(death_times, function(tj) {
    di <- sum(status == 1 & time == tj)
    yi <- sum(time >= tj)
    if (yi == 0) return(0)
    di / yi
  }) |> cumsum()
}

#library(survival)
# Funktion für Berechnung des Fehlers
compute_prediction_error_internal <- function(rg, data) {

  n <- nrow(data)
  num_trees <- rg$num.trees
  unique_times <- rg$unique.death.times
  num_timepoints <- length(unique_times)

  # Initialisieren
  predictions <- array(0, dim = c(n, num_timepoints))

  # Terminal nodes
  term_nodes <- predict(rg, data = data, type = "terminalNodes")$predictions+1

  # Loop über alle Bäume
  for (tree_idx in 1:num_trees) {
    for (sampleID in 1:n) {
      node_id <- term_nodes[sampleID, tree_idx]
      chf_vec <- rg$forest$chf[[tree_idx]][[node_id]]

      # Padding falls CHF kürzer als max Zeitpunkte
      chf_vec <- approx(
        x = 1:length(chf_vec), y = chf_vec,
        xout = seq_len(num_timepoints),
        method = "constant", rule = 2, f = 1
      )$y

      predictions[sampleID, ] <- predictions[sampleID, ] + chf_vec
    }
  }

  # Mittelung der CHF über Bäume und Summieren
  sum_chf <- numeric()

  predictions <- predictions / num_trees
  for (i in 1:n) {

    sum_chf[i] <- sum(predictions[i, ])
  }

  # C-Index berechnen auf Stichprobe
  surv_obj <- Surv(data$time, data$status)
  cidx <- concordance(surv_obj ~ sum_chf)
  prediction_error <- cidx$concordance

  return(prediction_error)
}
