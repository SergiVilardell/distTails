#' Probability function
#'
#' This function computes the probability of the full-tail gamma with the input sample data. The expression for the density used is:
#' \deqn{F(x; \alpha, \theta, \rho, u) = 1- \frac{\Gamma(\alpha, \rho + \theta(x + u))}{\Gamma(\alpha, \rho + \theta u)}}
#' @param x Sample data.
#' @param threshold Minimum value of the tail.
#' @param alpha alpha.
#' @param theta theta.
#' @param rho rho.
#' @keywords dFTG
#' @export
#' @examples
#' #pFTG()
pFTG <- function(x, alpha, theta, rho, threshold){
  1- gsl::gamma_inc(alpha, rho + theta * (x + threshold))/gsl::gamma_inc(alpha, rho + theta * threshold)
}
