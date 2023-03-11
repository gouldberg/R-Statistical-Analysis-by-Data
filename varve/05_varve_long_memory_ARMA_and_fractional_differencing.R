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
#   - Long memory time series data tend to exhibit sample autocorrelations that are not necessarily large (as in the case of d = 1),
#     but persist for a long time.
#   - fracdiff() uses a truncated maximum likelihood procedure, which is a little more elaborate than simply zeroing out initial values.
#     The default trunctaion value is M = 100
# ------------------------------------------------------------------------------

# mean-adjusted time series
lvarve <- log(varve) - mean(log(varve))



# ----------
# fracdiff() calculate the maximum likelihood estimators of the parameters of a fractionally-differenced ARIMA(p, d, q) model.
# nar:  number of autoregressive parameters
# nma:  number of moving average parameters
# M:  number of terms in the likelihood approximation

library(fracdiff)


# Applying Gauss-Newton iterative procedure, starting with d = 0.1 and omitting the first 30 points from the computation
varve.fd <- fracdiff(lvarve, nar=0, nma=0, M=30)


# d = 0.384
varve.fd$d


# The standard error is questionable.
varve.fd$stderror.dpq



# ----------
# Coefficients of pi(j)(0.384) (j = 1,2,...,30) are calculated, with pi(0)(0.384) = 1
p <- rep(1,31)

for(k in 1:30){ p[k+1] <- (k - varve.fd$d) * p[k] / ( k + 1 ) }

par(mfrow=c(1,1))

plot(1:30, p[-1], ylab = expression(pi(d)), xlab = "Index", type = "h")



# ----------
# diffseries:  differenciate the time series data using the approximated binomial expression of the long-memory filter
# and an estimate of the memory parameter in the ARFIMA(p,d,q) model

# residuals
( res.fd <- diffseries(log(varve), varve.fd$d) )

( res.arima <- resid(arima(log(varve), order = c(1,1,1))) )



# ----------
par(mfrow=c(2,1))

plot(res.arima, type = "l")
plot(res.fd, type = "l")



# ----------
par(mfrow=c(2,1))

Acf(res.arima, 100, xlim = c(4, 97), ylim = c(-0.2, 0.2), main = "")

Acf(res.fd, 100, xlim = c(4, 97), ylim = c(-0.2, 0.2), main = "")



# -->
# The ACFs of 2 residual series are roughly comparable with the white noize model



# ----------
# error variance estimate: 0.2298
var(res.fd)

var(res.arima)


