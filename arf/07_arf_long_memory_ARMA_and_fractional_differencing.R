setwd("//media//kswada//MyFiles//R//arf")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  arf
# ------------------------------------------------------------------------------

data(arf, package = "astsa")

str(arf)

head(arf)



# ------------------------------------------------------------------------------
# Estimate difference order
# ------------------------------------------------------------------------------

# demean arf
# arf_deme <- arf - mean(arf)



# ----------
library(fracdiff)

# arf.fd <- fracdiff(arf_deme, nar = 0, nma = 0, M = 30)
# arf.fd <- fracdiff(arf_deme, nar = 1, nma = 0, M = 30)
arf.fd <- fracdiff(arf, nar = 0, nma = 0, M = 30)


arf.fd$d


# -->
# 0.4995791 < 0.5,  marginally called fractional noise, fractionally differenced series



# ----------
# arf.fd$ar
# arf.fd$ma



# ----------
arf.fd$stderror.dpq


# -->
# Questionable result !!....



# ------------------------------------------------------------------------------
# Compare fractional differenced series and arima residuals
# ------------------------------------------------------------------------------

p <- rep(1,31)

for(k in 1:30){ p[k+1] = (k - arf.fd$d) * p[k] / ( k + 1 ) }

plot(1:30, p[-1], ylab = expression(pi(d), xlab = "Index", type = "h"))



# ----------
# fractionally differenced series:
# diffseries:  differenciate the time series data using the approximated binomial expression of the long-memory filter
# and an estimate of the memory parameter in the ARFIMA(p,d,q) model
res.fd <- diffseries(arf, arf.fd$d)
# res.fd <- diffseries(arf, 0.4)
# res.fd <- diffseries(diff(arf), 0.4)


# arima residuals
res.arima <- resid(arima(arf, order = c(1,1,0)))


graphics.off()
par(mfrow = c(2,1))

acf(res.arima, 100, xlim = c(4, 97), ylim = c(-0.2, 0.8), main = "")
acf(res.fd, 100, xlim = c(4, 97), ylim = c(-0.2, 0.8), main = "")



# -->
# ??????  still auto-correlated
# The ACFs of 2 residual series are roughly comparable with the white noize model ???



# ----------
# error variance estimate: 0.2298
var(res.fd)

var(res.arima)

