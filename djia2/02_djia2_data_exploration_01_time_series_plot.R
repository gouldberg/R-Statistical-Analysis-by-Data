setwd("//media//kswada//MyFiles//R//djia2")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  djia2 (Dow JOnes Industrial Average)
#   - Daily returns (or percent change) of the Dow Jones Industrial Average (DJIA) from April 20, 2006 to April 20, 2016.
# ------------------------------------------------------------------------------

data(djia, package = "astsa")

str(djia)

djia


# library(TTR)
# djia <- getYahooData("^DJI", start = 20060420, end = 20160420, freq = "daily")



# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------

library(xts)

djiar <- diff(log(djia$Close))[-1]

plot(djiar, main = "DJIA Returns", type = "n")

lines(djiar)



# -->
# It is easy to spot the financial crisis of 2008.
# The mean of the series appears to be stable with an average return of approximately zero,
# however, highly volatile (variable) periods tend to be clustered together.


# ----------
forecast::ndiffs(djia$Close)



# ----------
mean(djiar)


