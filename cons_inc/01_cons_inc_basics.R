# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/cons_inc")

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
# Visualize time series
# ------------------------------------------------------------------------------

plot.ts(data.ts)



# ----------
par(cex = 1.1, lwd = 1.8, mfrow=c(1,2))
plot.ts(data.ts[,"cons"], xlab = "Year", ylab = "Consumption")
plot.ts(data.ts[,"y"], xlab = "Year", ylab = "Permanent Income")


par(cex = 1.1, lwd = 1.8, mfrow=c(1,2))
plot.ts(diff(data.ts[,"cons"]), xlab = "Year", ylab = "Difference in Consumption")
plot.ts(diff(data.ts[,"y"]), xlab = "Year", ylab = "Difference in Permanent Income")


