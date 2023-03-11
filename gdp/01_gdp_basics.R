# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/gdp")

packages <- c("dplyr", "AER", "tseries", "forecast", "dynlm", "stargazer")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gdp
#  - GDP series for Australia and USA for the period 1970Q1 to 2000Q4
# ------------------------------------------------------------------------------
data("gdp", package = "POE5Rdata")


glimpse(gdp)
str(gdp)


# ----------
# convert to ts object
is.ts(gdp)

gdp <- ts(gdp, start = c(1970, 1), end = c(2000, 4), frequency = 4)



# ------------------------------------------------------------------------------
# Visualize time series
# ------------------------------------------------------------------------------

graphics.off()

par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(2,1))

ts.plot(gdp[,"usa"], gdp[,"aus"], type = "l", lty = c(1,2), col = c(1,1))
# legend("topleft", border = NULL, legend=c("Real CDP (USA)", "Real GDP (AUS)"), lty = c(1,2), col = c(1,1))

ts.plot(diff(gdp[,"usa"]), diff(gdp[,"aus"]), type = "l", lty = c(1,2), col = c(1,1))
# legend("topleft", border = NULL, legend=c("First Differencing Real CDP (USA)", "First Differencing Real GDP (AUS)"), lty = c(1,2), col = c(1,1))
