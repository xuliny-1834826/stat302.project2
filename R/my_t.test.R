#' My t-test Function
#'
#' This function performs a one sample t-test in R.
#'
#' @param x a numeric vector of data.
#' @param alternative a character string specifying the alternative hypothesis.
#'   This should only accept \code{"two.sided"}, \code{"less"},
#'   or \code{"greater"}. Otherwise, your function should
#'   throw an informative error.
#' @param mu a number indicating the null hypothesis value of the mean.
#'
#'
#' @return a \code{list} with elements:
#' @return \code{test_stat}: the numeric test statistic.
#' @return \code{df}: the degrees of freedom.
#' @return \code{alternative}: the value of the parameter \code{alternative}
#' @return \code{p_val}: the numeric p-value.
#'
#' @keywords inference
#'
#' @examples
#' data <- c(1, 3, 6, 9, 10, 28, 37, 46, 55, 83, 27, 39, 42, 63, 100)
#' my_t.test(data, "two.sided", 60)
#' my_t.test(data, "less", 60)
#' my_t.test(data, "greater", 60)
#'
#'
#' @importFrom stats pt sd
#'
#' @export
#'
# create my own test function
my_t.test <- function(x, alternative, mu) {

  # throw an error if x is not numerical vector
  if (!(is.numeric(x))) {
    stop("Input is not numeric!!!")
  } else if (!is.vector(x)) {
    stop("Input is not vector!!!")
  }

  # sample size
  size <- length(x)

  # calculate the test stat
  test_stat <- (mean(x) - mu) / (sd(x) / sqrt(size))

  # degrees of freedom
  df <- size - 1

  # calculate p_val
  if (alternative == "less") {
    p_val <- pt(test_stat, df, lower.tail = TRUE)
  } else if (alternative == "greater") {
    p_val <- pt(test_stat, df, lower.tail = FALSE)
  } else if (alternative == "two.sided") {
    p_val <- pt(-abs(test_stat), df, lower.tail = TRUE) + pt(abs(test_stat), df, lower.tail = FALSE)
  } else {
    stop("Input is not accepted!")
  }

  # return the result list
  return(list(t = test_stat, df = df, alternative = alternative, p_val = p_val))
}
