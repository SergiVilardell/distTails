#' Probability function
#'
#' This function computes the probability of the full-tail gamma with the input sample data. The expression for the probability used is:
#' \deqn{G(x; \alpha, \theta, \rho) = 1 - \Gamma(\alpha, \rho + \theta x)/\Gamma(\alpha, \rho).}
#' @param x Sample data.
#' @param alpha alpha.
#' @param theta theta.
#' @param rho rho.
#' @keywords dFTG
#' @export
#' @examples
#' #pFTG()
pFTG <- function(x, alpha, theta, rho){
  1- zipfR::Igamma(alpha, rho + theta*x, lower = F)/zipfR::Igamma(alpha, rho, lower = F)
}

