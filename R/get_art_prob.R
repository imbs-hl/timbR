#' \code{get_art_prob} transforms a classification tree to a probability predictor (scoring classifier), e.g. an artificial representative tree (art)
#'
#'
#' @param art            Tree object of class \code{ranger}, e.g. an artificial representative tree (art) build with generate_tree()
#' @param train_data     Additional data set comparable to the data set \code{rf} was build on. Only needed for metric "prediction".
#'
#' @author Lea Louisa Kronziel, M.sc.
#' @return
#'   \item{\code{}}{ranger object predicting probabilities instead of classes}
#' @import ranger
#' @import dplyr
#' @import checkmate
#'
#' @examples
#' require(ranger)
#' require(timbR)
#' require(dplyr)
#'
#' set.seed(12345)
#' # Train random forest with ranger
#' rf.iris <- ranger(Species ~ ., data = iris, write.forest=TRUE, num.trees = 10)
#'
#' # generate art
#' art <- generate_tree_reimplementation(rf = rf.iris, metric = "splitting variables", train_data = iris, dependent_varname = "Species", min.bucket = 25)
#'
#'# transform classification tree to probability predictor
#' probability_art <- get_art_prob(art = art, train_data = iris)
#'


get_art_prob <- function(art, train_data){
  ## Check input ----
  if (!checkmate::testClass(rf, "ranger")){
    stop("rf must be of class ranger")
  }
  if ((rf$treetype %in% c("Classification") == FALSE)){
    stop("Treetyp not supported. Please use a classification tree.")
  }

  if (checkmate::testNull(train_data)){
    stop("You have to provide a train data set.")
  }

  if ("try-error" %in% class(try(predict(rf, data = train_data), silent = TRUE))){
    stop("The provided train data set does not fit to the provided ranger object")
  }

  if(art$num.trees != 1 | art$forest$num.trees != 1){
    stop("ranger object has to be a single tree.")
  }


  # ------
  # set new art
  art_prob <- art

  # change values from classification to probability estimation
  art_prob$treetype <- "Probability estimation"
  art_prob$forest$treetype <- "Probability estimation"
  art_prob$confusion.matrix <- NULL

  # tree info of original ART
  treeinfo_art <- treeInfo(art)

  # observations of train data that are passed to each node
  splitted_data_list <- timbR:::get_splitted_data(tree_info_df = treeinfo_art,
                                                  inbag_data_df = train_data,
                                                  rf_list = art,
                                                  tree_number = 1)

  # build terminal class counts
  # add how many observations end in each node (num.samples.nodes; otherwise treeInfo() won't work)
  terminal.class.counts <- list()
  num.samples.nodes <- list()
  for(i in 1:nrow(treeinfo_art)){
    abs_values <- data.frame(table(splitted_data_list[[i]]$Species))
    rel_values <- abs_values$Freq/nrow(splitted_data_list[[i]])
    terminal.class.counts[[i]] <- as.vector(t(rel_values))
    num.samples.nodes[[i]] <- as.vector(t(abs_values$Freq))
  }
  terminal.class.counts <- list(terminal.class.counts)
  num.samples.nodes <- list(num.samples.nodes)

  art_prob$forest$terminal.class.counts <- terminal.class.counts
  art_prob$forest$num.samples.nodes <- num.samples.nodes

  return(art_prob)
  }


