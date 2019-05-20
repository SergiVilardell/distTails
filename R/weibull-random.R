#' Random function
#'
#' This function generates random deviates for the tail weibull distribution.
#' @param n Sample size.
#' @param threshold Minimum value of the tail.
#' @param scale Scale parameter.
#' @param shape Shape parameter.
#' @keywords weibull
#' @export
#' @examples
#' #rtailw()
rtailw<- function(n, threshold, scale, shape) {
  x <- stats::runif(n)
  q <- qtailw(x, threshold, scale, shape)
  return(q)
}
