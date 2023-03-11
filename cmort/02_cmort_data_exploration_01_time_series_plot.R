setwd("//media//kswada//MyFiles//R//cmort")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  cmort (Pollution, Temperature and Mortality)
# ------------------------------------------------------------------------------

data(cmort, package = "astsa")
data(tempr, package = "astsa")
data(part, package = "astsa")


str(cmort)
str(tempr)
str(part)


cmort
tempr
part



# ------------------------------------------------------------------------------
# data exploration:  Check ts object
# ------------------------------------------------------------------------------


head(cmort)


head(tempr)


head(part)



# -->
# start = 1970, 1
# end = 1970, 6
# frequency = 52, weekly data



# ------------------------------------------------------------------------------
# data exploration:  time series plot
# ------------------------------------------------------------------------------

# This is weekly data.

par(mfrow = c(3,1), mar = c(2,2,2,2))

plot(cmort, main = "Cardiovascular Mortality", xlab = "", ylab = "")
abline(v = seq(1970, 1980, by = 1), col = "gray", lty = 2)

plot(tempr, main = "Temerature", xlab = "", ylab = "")
abline(v = seq(1970, 1980, by = 1), col = "gray", lty = 2)

plot(part, main = "Particulates", xlab = "", ylab = "")
abline(v = seq(1970, 1980, by = 1), col = "gray", lty = 2)




# -->
# Note the strong seasonal components in all of the series, corresponding to winter-summer variations and the downward trend
# in the cardiovascular mortality over the 10-year period.




# ----------
# all on same plot
par(mfrow = c(1,1))

ts.plot(cmort, tempr, part, col = 1:3)




# ----------
# only end part

par(mfrow = c(3,1))

plot(cmort[450:508], main = "Cardiovascular Mortality", xlab = "", ylab = "", type = "o")

plot(tempr[450:508], main = "Temerature", xlab = "", ylab = "", type = "o")

plot(part[450:508], main = "Particulates", xlab = "", ylab = "", type = "o")




# ------------------------------------------------------------------------------
# data exploration:  diff
# ------------------------------------------------------------------------------

forecast::ndiffs(cmort)

forecast::ndiffs(tempr)

forecast::ndiffs(part)



# -->
# only cmort has 1 diff



# ----------
graphics.off()

par(mfrow = c(3,1))

plot(diff(cmort), type = "l")

plot(diff(tempr), type = "l")

plot(diff(part), type = "l")



