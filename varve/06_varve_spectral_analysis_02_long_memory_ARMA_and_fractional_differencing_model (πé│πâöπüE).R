setwd("//media//kswada//MyFiles//R//varve")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  varve
# ------------------------------------------------------------------------------

data(varve, package = "astsa")

str(varve)



# ------------------------------------------------------------------------------
# Long memory ARMA and Fractional Differencing:  ARFIMA(p, d, q)  (-0.5 < d < 0.5)
#   - Fitting a long memory model using frequency domain methods
#   - Whittle likelihood is especiallly useful for fitting long memory models
# ------------------------------------------------------------------------------

x <- log(varve)



# ----------
# initial value of d
d0 <- 0.1


n.per <- nextn(length(x))

m <- (n.per) / 2 - 1


# remove 0 freq and scale the periodogram
per <- Mod(fft(x - mean(x))[-1])^2

( per <- per / n.per )



# ----------
g <- 4 * (sin(pi * ((1:m) / n.per))^2)

whit.like <- function(d){
  g.d <- g^d
  sig2 <- (sum(g.d*per[1:m]) / m)  # approximate maxiimum likelihood estimator for the variance parameter
  log.like <- m * log(sig2) - d*sum(log(g)) + m  # log-likelihood (to be maximized)
  return(log.like)
}

( est <- optim(d0, whit.like, gr = NULL, method = "L-BFGS-B", hessian = TRUE, lower = -0.5, upper = 0.5, control = list(trace = 1, REPORT=1)) )



# ----------
# Whittle approximation gives = d = 0.380, with an estimated standard error of 0.028
cat("d.hat = ", est$par, "se(dhat) = ", 1 / sqrt(est$hessian), "\n")



# ----------
g.dhat <- g^est$par

sig2 <- sum(g.dhat * per[1:m]) / m

cat("sig2hat = ", sig2, "\n")



# ----------
# long memory spectral estimate
g <- 4 * (sin(pi * ((1:500)/2000))^2)

( fhat <- sig2 * g^{-est$par} )

