#' Maximum Likelihood Estimation of weibull tails
#'
#' This function scales the input data w.r.t. the threshold and performs MLE with a weibull tail.
#' @param sample Sample data.
#' @keywords weibull
#' @export
#' @examples
#' #fittailweibull()
fittailweibull <- function(sample) {
  tailweib <- sample / min(sample)
  # Compute initial value for shape
  aux <- function(p) {
    -ltailweibull(tailweib, threshold = 1, scale = 1, shape = p)
  }
  solfit <- stats::nlm(aux, 1)
  shape0 <- solfit$estimate
  # Compute initial value for scale
  aux <- function(p) {
    -ltailweibull(tailweib, threshold = 1, scale = p, shape = 1)
  }
  solfit <- stats::nlm(aux, 1)
  scale0 <- solfit$estimate
  # Compute values for shape and scale with the initial values
  aux <- function(p) {
    -ltailweibull(tailweib, threshold = 1, scale = p[1], shape = p[2])
  }
  solfit <- stats::nlm(aux, c(scale0, shape0))
  fits <- list(scale =  solfit$estimate[1], shape = solfit$estimate[2])
  return(fits)
}
