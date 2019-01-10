#' Quantile function
#'
#' This function computes the quantile of the tail weibull.
#' @param p Probability.
#' @param threshold Minimum value of the tail.
#' @param scale Scale parameter.
#' @param shape Shape parameter.
#' @keywords weibull
#' @export
#' @examples
#' #qtailweibull()
qtailweibull<- function(p, threshold, scale, shape) {
  q <- ((-log(1 - p) / scale) + threshold^shape)^(1 / shape)
  return(q)
}
