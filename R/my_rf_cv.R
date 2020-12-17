#' Random Forest Cross-Validation
#'
#' This function can predict an unknown class using other known classes.
#'
#' @param k number of folds
#'
#'
#' @return a numeric with the cross-validation error
#'
#' @keywords prediction
#'
#' @examples
#' my_rf_cv(5)
#' my_rf_cv(k = 8)
#'
#'
#' @export
#'
# generate function my_rf_cv
my_rf_cv <- function(k) {
  fold <- sample(rep(1:k, length = nrow(my_penguins)))
  data <- data.frame(my_penguins, "fold" = fold)
  MSE <- 1:k

  # iterate through all folds
  for (i in 1:k) {
    # get train data and test data
    data_train <- data %>% filter(fold != i)
    data_test <- data %>% filter(fold == i)

    # generate the model
    MODEL <- randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm
                          + flipper_length_mm, data = data_train,
                          ntree = 100)

    # get predicted values
    PREDICTIONS <- predict(MODEL, data_test[, -4])

    # get the MSE
    MSE[i] <- mean((PREDICTIONS - data_test[, 4])^2)
  }

  # output the general MSE
  return(mean(MSE))
}
