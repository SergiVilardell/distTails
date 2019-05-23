#' Probability function
#'
#' This function computes the quantiles of the full-tail gamma with the input sample data. The expression for the probability used is:
#' \deqn{F(x; \alpha, \theta, \rho) = 1- \frac{\Gamma(\alpha, \rho + \theta x)}{\Gamma(\alpha, \rho)}}
#' @param p probabilities.
#' @param alpha alpha.
#' @param theta theta.
#' @param rho rho.
#' @keywords dFTG
#' @export
#' @examples
#' #qFTG()
qFTG <- function(p, alpha, theta, rho){
  (zipfR::Igamma.inv(alpha, (1 - p)*zipfR::Igamma(alpha, rho)) - rho)/theta
}

