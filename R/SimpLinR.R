#' Simple Linear Regression
#'
#' @param x vector for independent variable
#' @param y vector for dependent variable
#'
#' @return List of different linear regression metrics: fitted coefficients, predictions, residuals, standard errors of coefficients, and 95% confidence interval of coefficients
#'
#' @examples
#' x <- c(1, 2, 3, 4)
#' y <- c(2, 4, 6, 11)
#' SimpLinR(x, y)
#'
#' @useDynLib SimpLin, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#'
#' @export
SimpLinR <- function(x, y) {
  if(length(x) != length(y)) {
    stop("x and y must have equal lengths.")
  }
  
  SimpLinCpp(x, y)
}