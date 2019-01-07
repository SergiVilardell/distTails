#' Log-likelihood function
#'
#' This function computes the log-likelihood of the tail weibull with the input sample data.
#' @param x Sample data.
#' @param threshold Minimum value of the tail.
#' @param scale Scale parameter.
#' @param shape Shape parameter.
#' @keywords weibull
#' @export
#' @examples
#' ltailweibull()
ltailweibull <- function(x, threshold, scale, shape) {
  l <- length(x) * log(scale) + length(x) * log(shape) + (shape - 1) * sum(log(x)) - scale * sum(x^shape - threshold^shape)
  return(l)
}
