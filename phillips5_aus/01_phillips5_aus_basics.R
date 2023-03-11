# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/phillips5_aus")

packages <- c("dplyr", "AER", "lmtest", "stargazer", "dynlm", "broom", "knitr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  phillips5_aus
#   - the cahnge in unemployment rate (du), inflation (inf), and the level of unemployment rate (u), in Australia,
#     over the period 1987Q1 through 2016Q1
#
# phillips curve
#  - inf(t) = alpha + beta0 * Difference(u(t)) + e(t)
# ------------------------------------------------------------------------------
data("phillips5_aus", package = "POE5Rdata")

data <- phillips5_aus

glimpse(data)

str(data)

dim(data)



# ----------
# convert to ts object
is.ts(data)
data.ts <- ts(data[,c(2,3,4)], start = c(1987, 1), end = c(2016, 1), frequency = 4)



# ------------------------------------------------------------------------------
# Visualize time series
# ------------------------------------------------------------------------------

plot.ts(data.ts)



# ----------
# almost same plot
par(cex = 1.1, lwd = 1.8, mfrow=c(3,1))
plot.ts(data.ts[,"inf"], xlab = "Year", ylab = "Inflation Rate")
plot.ts(data.ts[,"u"], xlab = "Year", ylab = "Unemployment Rate")
plot.ts(diff(data.ts[,"u"]), xlab = "Year", ylab = "Difference in Unemployment Rate")



# ----------
# any pattern ?
par(cex = 1.1, lwd = 1.8, mfrow=c(1,1))
plot(diff(data.ts[,"u"]), data.ts[,"inf"])



