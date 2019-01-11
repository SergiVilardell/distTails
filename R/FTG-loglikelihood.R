#' Loglikelihood function
#'
#' This function computes the loglikelihood of the full-tail gamma with the input sample data. The expression used is:
#' \deqn{l(x; \alpha, \sigma, \rho) = -n(\log\Gamma(\lpha, \rho) + \log(\sigma\rho^{-\alpha}) - \frac{\alpha - 1}{n}
#' \sum_{i = 1}{n}\log(1 + \frac{x_{i}}{\sigma}) + \frac{\rho}{n} \sum_{i = 1}{n}(1 + \frac{x_{i}}{\sigma}))}
#' @param x Sample data.
#' @param alpha alpha.
#' @param theta theta.
#' @param rho rho.
#' @keywords lFTG
#' @export
#' @examples
#' #lFTG()
lFTG <- function(x, alpha, theta, rho){
  sigma <- theta/rho
  n <- length
  l <- -n * (log(gsl::gamma_inc(alpha, rho)) + log(sigma * rho^(-alpha)) - ((alpha - 1)/n)*sum(log(1 + x/sigma))
             + (rho / n) * sum(1 + x/sigma))
  return(l)
}
