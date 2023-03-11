# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/fred5")

packages <- c("dplyr", "AER", "tseries", "forecast", "dynlm", "stargazer")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  fred5
#  - Personal Consumtion and Personal Disposable Income already in logs over the period of 1986Q1 through 2015Q2
# ------------------------------------------------------------------------------
data("fred5", package = "POE5Rdata")


glimpse(fred5)

str(fred5)


# ----------
# convert to ts object
is.ts(fred5)

fred <- ts(fred5, start = c(1986, 1), end = c(2015, 2), frequency = 4)



# ------------------------------------------------------------------------------
# Visualize time series
# ------------------------------------------------------------------------------

graphics.off()

par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(2,1))

ts.plot(fred[,"consn"], fred[,"y"], type = "l", lty = c(1,2), col = c(1,1))
# legend("topleft", border = NULL, legend=c("Personal Consumtion", "Personal Disposable Income"), lty = c(1,2), col = c(1,1))

ts.plot(diff(fred[,"consn"]), diff(fred[,"y"]), type = "l", lty = c(1,2), col = c(1,1))
# legend("topleft", border = NULL, legend=c("First Differencing Personal Consumption", "First Differencing Personal Disposable Income"), lty = c(1,2), col = c(1,1))
