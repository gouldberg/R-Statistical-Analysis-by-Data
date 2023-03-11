# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/cons_inc")

packages <- c("dplyr", "AER", "lmtest", "stargazer", "dynlm", "broom", "knitr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  cons_inc
#    - y:  permanent income, an infinite stream of income
#    - cons:  consumption
# ------------------------------------------------------------------------------
data("cons_inc", package = "POE5Rdata")

data <- cons_inc

glimpse(data)

str(data)



# ----------
# convert to ts object
is.ts(data)
data.ts <- ts(data[,c(2,4)], start = c(1959, 3), end = c(2016, 3), frequency = 4)



# ------------------------------------------------------------------------------
# Sample autocorrelation for unemployment by acf()
# ------------------------------------------------------------------------------
graphics.off()

acf(data.ts[,"y"], lag.max = 40, main = "Correlogram for Permanent Income")

acf(data.ts[,"cons"], lag.max = 40, main = "Correlogram for Consumption")

acf(diff(data.ts[,"cons"]), lag.max = 40, main = "Correlogram for Change in Consumption")


# -->
# 1st difference of Consumption still has autocorrelation through amost all lags



# ----------
# check by plot
plot.ts(diff(data.ts[,"cons"]))
lines(lowess(diff(data.ts[,"cons"])), col = "blue")



# ------------------------------------------------------------------------------
# Check order of integration
# ------------------------------------------------------------------------------

forecast::ndiffs(data.ts[,"y"])

forecast::ndiffs(data.ts[,"cons"])


# -->
# order of integration of both time series are 2

