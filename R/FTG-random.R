#' Random sample function
#'
#' This function computes n random variates from full-tail gamma with a rejection method.
#' @param n Sample size.
#' @param alpha alpha.
#' @param theta theta.
#' @param rho rho.
#' @keywords rFTG
#' @export
#' @examples
#' x -> rFTG(100, 1, 1, 1)
#' hist(x, breaks = "FD")
rFTG <- function(n, alpha, theta, rho) {
  sample <- c()
  m <- 0
  while (m < n) {
    x <- stats::rexp(1, rate = rho)
    u <- stats::runif(1)
    if (u <= (1 + x)^(alpha - 1)) {
      sample[m + 1] <- x
    }
    m <- length(sample)
  }
  sample * rho / theta
}
