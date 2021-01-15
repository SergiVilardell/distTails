#' Quantile function
#'
#' This function computes the quantile function of the tailW.
#' \deqn{Q(p,\alpha, \beta, \nu) = \left(\frac{-\log(1 - p)}{\alpha} + \nu^\beta\right)^{1 / \beta}}
#' @param p Probability.
#' @param threshold Minimum value of the tail.
#' @param scale Scale parameter.
#' @param shape Shape parameter.
#' @return Gives the quantiles of the TailW. The length of the result is determined by the length of x.
#' @keywords TailW
#' @references Vilardell, Sergi & Serra, Isabel & Abella, Jaume & del Castillo, Joan & Cazorla, Francisco. (2019). Software Timing Analysis for Complex Hardware with Survivability and Risk Analysis. 227-236. <doi:10.1109/ICCD46524.2019.00036>.
#' @export
#' @examples
#' qtailw(0.5, 1, 1, 1)
qtailw <- function(p, threshold, scale, shape) {
  q <- ((-log(1 - p) / scale) + threshold^shape)^(1 / shape)
  return(q)
}
