setwd("//media//kswada//MyFiles//R//globtemp")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  globtemp
# ------------------------------------------------------------------------------


data(globtemp, package = "astsa")


str(globtemp)




# ------------------------------------------------------------------------------
# data exploration:  plot differenced data
# ------------------------------------------------------------------------------

par(mfrow = c(1, 1))

plot(diff(globtemp), type = "o")



# ------------------------------------------------------------------------------
# Estimate drift
# ------------------------------------------------------------------------------

# drift estimate = 0.008

( drift_est <- mean(diff(globtemp)) )

abline(h = drift_est, lty = 2, col = "blue")




# ------------------------------------------------------------------------------
# Compare linear regression and drift line
# ------------------------------------------------------------------------------

lmod <- lm(globtemp ~ time(globtemp))


par(mfrow = c(1, 1))

plot(globtemp, type = "o")
abline(coef(lmod)[1], coef(lmod)[2], col = "blue", lty = 2)
abline(globtemp[1] - drift_est * 1880, drift_est, col = "red", lty = 2)



# -->
# blue:  fitted regression line
# red:  drift line



# ------------------------------------------------------------------------------
# data exploration:  Number of differencing to be stationary
# ------------------------------------------------------------------------------

forecast::ndiffs(globtemp)

