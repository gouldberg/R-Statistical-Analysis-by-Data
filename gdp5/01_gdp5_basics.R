# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/gdp5")

packages <- c("dplyr", "Hmisc", "rms", "caret", "corrplot", "lattice", "ggplot2", "AER", "foreign", "car", "lmtest", "stargazer", "broom", "knitr", "plm", "systemfit", "tseries")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  
#  - gdp5:  U.S. GDP over the period of 1984Q1 through 2016Q1
#  - usdata5:  monthly data on the variables inflation, federal funds rate, and bond rate in the U.S. over the period of August 1954 through December 2016
# ------------------------------------------------------------------------------
data("gdp5", package = "POE5Rdata")
data("usdata5", package = "POE5Rdata")


is.ts(gdp5)
is.ts(usdata5)


head(gdp5)
head(usdata5)



# ----------
# convert to ts object
# Note that gdp5 and usdata5 have different frequency (quarterly and monthly)
gdp.ts <- ts(gdp5$gdp, start = c(1984, 1), end = c(2016, 4), frequency = 4)
usa.ts <- ts(usdata5, start = c(1954, 8), end = c(2016, 12), frequency = 12)



# ----------
# create data.frame from ts object
usa.ts.df <- data.frame(b = usa.ts[,2], f = usa.ts[,3], inf = usa.ts[,4])



# ------------------------------------------------------------------------------
# Chaeck the time series
# ------------------------------------------------------------------------------
graphics.off()
par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(4,2))

ts.plot(gdp.ts, xlab = "(a) Real GDP ($trillion)", ylab = "")
ts.plot(diff(gdp.ts, 1), xlab = "(b) Change in GDP", ylab = "")
ts.plot(usa.ts[,"infn"], xlab = "(c) Inflation Rate", ylab = "")
ts.plot(diff(usa.ts[,"infn"]), xlab = "(d) Change in Inflation", ylab = "")
ts.plot(usa.ts[,"ffr"], xlab = "(e) Change in Inflation", ylab = "")
ts.plot(diff(usa.ts[,"ffr"]), xlab = "(f) Change in Federal Funds", ylab = "")
ts.plot(usa.ts[,"br"], xlab = "(g) Three-Year Bond Rate, Percent", ylab = "")
ts.plot(diff(usa.ts[,"br"]), xlab = "(h) Change in the Bond Rate", ylab = "")

