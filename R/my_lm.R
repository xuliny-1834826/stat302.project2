#' My Linear Model Function
#'
#' This function fits a linear model in R.
#'
#' @param formula a \code{formula} class object, similar to \code{lm()}.
#' @param data input data frame.
#'
#'
#' @return a \code{table} similar to the coefficent table from \code{summary()}
#'   with rows for each coefficient (including the \code{(Intercept)}!)
#'   and columns for the \code{Estimate}, \code{Std. Error}, \code{t value},
#'   and \code{Pr(>|t|)}. There should be row and column names.
#'
#' @keywords inference
#'
#' @examples
#' my_lm(eruptions ~ waiting, faithful)
#'
#'
#' @export
#'
# create function:my_lm
my_lm <- function(formula, data) {
  # extract the model matrix X
  X <- model.matrix(formula, data)

  # extract the model response Y
  Y <- model.response(model.frame(formula, data))

  # solve for linear regression coefficients
  coeff <- solve(t(X) %*% X) %*% t(X) %*% Y

  # get the  degrees of freedom
  df_lm <- nrow(X) - ncol(X) - 1

  # estimate the standard error for the coefficients
  sigma_sq <- sum((Y - X %*% coeff) ^ 2) / df_lm
  se <- sqrt(diag(sigma_sq * solve(t(X) %*% X)))

  # get the t value
  t_value <- (coeff - 0) / se

  # get two-sided p-value from t test
  pr <- 2 * pt(-abs(t_value), df = df_lm, lower.tail = TRUE)

  # name the result matrix
  result <- cbind(coeff, se, t_value, pr)
  colnames(result) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  rownames(result) <- colnames(X)

  # return result as a table
  return(as.table(result))
}
