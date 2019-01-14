library(MASS)
library(distributionTails)
library(ercv)

# Functions ---------------------------------------------------------------
r <- function(x, sigma){
  length(x)/sum(1 + x/sigma)
}

s <- function(x, sigma){
  length(x)/log(sum(1 + x/sigma))
}

lsigma <- function(x, alpha, sigma, rho){
  (length(x)*(1/sigma -(alpha - 1)*s(x, sigma) + rho*r(x, sigma)))^(2)
}

# Params ------------------------------------------------------------------
alpha <- 0.2
theta <- 0.5
rho <- 0.7
sigma <- theta/rho


# Sample ------------------------------------------------------------------
x <- rFTG(n = 1000, alpha = alpha, theta = theta, rho = rho)
y <- x/mean(x)


# MLE ---------------------------------------------------------------------

solfit <- stats::nlm(function(p) -lsigma(x, alpha = p[1], sigma = p[2], rho = p[3]), c(0.1, 0.1, 0.1))



# BLEH --------------------------------------------------------------------
fit <- fitpot(y)
theta0 <- unname(fit["psi"])
alpha0 <- unname(fit["evi"])
solfit <- stats::nlm(function(p) -lFTG(x, alpha = p[1], theta = p[2], rho = p[3]), c(alpha0, 1, rho0))
solfit
lFTG(x, alpha = alpha, theta = theta, rho = rho)
xseq <- seq(min(x), max(x), length.out = length(x))
d <- dFTG(xseq, alpha, theta, rho)
dalt <- dFTG(xseq, solfit$estimate[1], solfit$estimate[2]*mean(x), solfit$estimate[3])
hist(x, probability = T, breaks = "FD")
lines(xseq, d)
lines(xseq, dalt, col = "red")

