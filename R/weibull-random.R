#' Random function
#'
#' This function generates random deviates for the tail Weibull distribution.
#' @param n Sample size.
#' @param threshold Minimum value of the tail.
#' @param scale Scale parameter.
#' @param shape Shape parameter.
#' @keywords weibull
#' @export
#' @examples
#' x -> rtailw(1000, 1, 2, 3)
#' hist(x, breaks = "FD")
rtailw<- function(n, threshold, scale, shape) {
  x <- stats::runif(n)
  q <- qtailw(x, threshold, scale, shape)
  return(q)
}
