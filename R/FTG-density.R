#' Density function
#'
#' This function computes the density of the full-tail gamma with the input sample data. The expression for the density used is:
#' \deqn{g(x; \alpha, \theta, \rho) = \theta(\rho + \theta x)^{\alpha - 1}\exp(-(\rho + \theta x))/\Gamma(\alpha, \rho).}
#' @param x Sample data.
#' @param alpha alpha.
#' @param theta theta.
#' @param rho rho.
#' @keywords dFTG
#' @export
#' @examples
#' a <- 0.3
#' t <- 0.3
#' r <- 0.8
#' n <- 1000
#' sample <- rFTG(n, a, t, r)
#' x <- seq(min(sample), max(sample), length.out = 200)
#' d <- dFTG(x, a, t, r)
#' hist(sample, breaks = "FD", probability = TRUE)
#' lines(x, d, col = "red")
dFTG <- function(x, alpha, theta, rho) {
  theta * (rho + theta * x)^(alpha - 1) * exp(-(rho + theta * x)) / zipfR::Igamma(alpha, rho, lower = F)
}
