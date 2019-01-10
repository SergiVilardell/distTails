#' Probability function
#'
#' This function computes the probability of the tail weibull with the input sample data.
#' @param x Sample data.
#' @param threshold Minimum value of the tail.
#' @param scale Scale parameter.
#' @param shape Shape parameter.
#' @keywords weibull
#' @export
#' @examples
#' #ptailweibull()
ptailweibull <- function(x, threshold, scale, shape) {
  p <- 1 - exp(-scale * x^shape + scale * threshold^shape)
  return(p)
}
