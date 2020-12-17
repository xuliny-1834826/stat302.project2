#' My k-Nearest Neighbors Cross-Validation
#'
#' This function can predict an unknown class using other known classes.
#'
#' @param train input data frame.
#' @param cl true class value of your training data.
#' @param k_nn integer representing the number of neighbors.
#' @param k_cv integer representing the number of folds.
#'
#'
#' @return a \code{list} with:
#' @return \code{class} with elements:
#' @return \code{cv_err} the numeric test statistic.
#'
#' @keywords prediction
#'
#' @examples
#' my_knn_cv(penguins_train, penguins_knn$species, 1, 5)
#' my_knn_cv(penguins_train, penguins_knn$species, 5, 5)
#'
#'
#' @export
#'
# generate the function my_knn_cv
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # split data into folds
  fold <- sample(rep(1:k_cv, length = length(cl)))
  data <- data.frame("x" = train, "y" = cl, "fold" = fold)

  # initiate output
  class <- 1:k_cv
  cv_err <- 1:k_cv

  # iterate through all folds
  for (i in 1:k_cv) {
    # get train data and test data
    data_train <- data %>% filter(fold != i)
    data_test <- data %>% filter(fold == i)
    # Train the model
    y_hat <- as.character(knn(data_train[, 1:4], data_test[, 1:4], data_train$y, k_nn))
    class[i] <- y_hat
    #calculate the cv error
    cv_err[i] <- mean(data_train$y == y_hat)
  }

  # calculate the total cv_err
  cv_error <- mean(cv_err)


  # return a list of class, cv_err as output
  return(list("class" = class, "cv_err" = cv_error))
}
