#' FTG Density Function
#'
#' This function computes the density of the full-tail gamma with the input sample data. The expression for the density used is:
#' \deqn{g(x; \alpha, \theta, \rho) = \theta(\rho + \theta x)^{\alpha - 1}\exp(-(\rho + \theta x))/\Gamma(\alpha, \rho).}
#' @param x Sample data.
#' @param threshold Minimum value of the tail.
#' @param scale Scale parameter.
#' @param shape Shape parameter.
#' @keywords FTG
#' @export
#' @references del Castillo, Joan & Daoudi, Jalila & Serra, Isabel. (2012). The full-tails gamma distribution applied to model extreme values. ASTIN Bulletin.
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
dFTG <- function(x, threshold, scale, shape) {
 scale * (threshold + scale * x)^(shape - 1) * exp(-(threshold + scale * x)) / zipfR::Igamma(shape, threshold, lower = F)
}
