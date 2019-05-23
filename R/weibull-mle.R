#' Maximum Likelihood Estimation of Weibull tails
#'
#' This function scales the input data w.r.t. the threshold and performs MLE with a Weibull tail.
#' @param sample Sample data.
#' @keywords weibull
#' @export
#' @examples
#' scale <- 2
#' shape <- 1
#' threshold <- 1
#' s <- rtailw(1000, threshold = threshold , scale = scale, shape = shape)
#' fits <- fittailw(s)
#' x_seq <- seq(threshold, max(s), length.out = 500)
#' theo_density <- dtailw(x_seq, threshold = threshold, scale = fits$scale, shape = fits$shape)
#' hist(s, probability = TRUE, breaks = "FD")
#' lines(x = x_seq, y = theo_density, col = "red")
fittailw <- function(sample) {
  tailweib <- sample / min(sample)
  # Compute initial value for shape
  aux <- function(p) {
    -ltailw(tailweib, threshold = 1, scale = 1, shape = p)
  }
  solfit <- stats::nlm(aux, 1)
  shape0 <- solfit$estimate
  # Compute initial value for scale
  aux <- function(p) {
    -ltailw(tailweib, threshold = 1, scale = p, shape = 1)
  }
  solfit <- stats::nlm(aux, 1)
  scale0 <- solfit$estimate
  # Compute values for shape and scale with the initial values
  aux <- function(p) {
    -ltailw(tailweib, threshold = 1, scale = p[1], shape = p[2])
  }
  solfit <- stats::nlm(aux, c(scale0, shape0))
  fits <- list(scale =  solfit$estimate[1], shape = solfit$estimate[2])
  return(fits)
}
