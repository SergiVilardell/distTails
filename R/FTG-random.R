#' Random sample function
#'
#' This function computes n random variates from full-tail gamma with a rejection method.
#' @param n Sample size.
#' @param threshold Minimum value of the tail.
#' @param alpha alpha.
#' @param theta theta.
#' @param rho rho.
#' @keywords rFTG
#' @export
#' @examples
rFTG <- function(n, alpha, theta, rho) {
  sample <- c()
  m <- 0
  while (m < n) {
    x <- rexp(1, rate = rho)
    u <- runif(1)
    if (u <= (1 + x)^(alpha - 1)) {
      sample[m + 1] <- x
    }
    m <- length(sample)
  }
  sample * rho / theta
}
