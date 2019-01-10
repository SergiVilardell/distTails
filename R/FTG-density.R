#' Density function
#'
#' This function computes the density of the full-tail gamma with the input sample data. The expression for the density used is:
#' \deqn{f(x; \alpha, \theta, \rho, u) = \theta(\rho + \theta(x + u))^{\alpha - 1}\exp(-(\rho + \theta(u + x)))/ \Gamma(\alpha, \rho + \theta u)}
#' @param x Sample data.
#' @param threshold Minimum value of the tail.
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
#' sample <- sort(sample)
#' u <- sample[length(sample) - n/2]
#' xu <- seq(u, max(sample), length.out = 200)
#' du <- dFTG(xu, a, t, r, u)
#' hist(sample, breaks = "FD", probability = T)
#' lines(xu, du, col = "red")
dFTG <- function(x, alpha, theta, rho, threshold) {
  theta * (rho + theta * threshold + theta * x)^(alpha - 1) *
    exp(-(rho + theta * threshold + theta * x)) / gsl::gamma_inc(alpha, rho + theta * threshold)
}
