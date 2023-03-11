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
# Regression with lagged variables with align lagged series
#   - preparation
# ------------------------------------------------------------------------------


temp <- tempr - mean(tempr)


temp2 <- temp ^ 2


trend <- time(cmort)


part7 <- stats::lag(part.pw, -7)




# ----------
dat2 <- ts.intersect(cmort.fil, trend, temp, temp2, part7, part.pw)


head(dat2)




# ------------------------------------------------------------------------------
# Regression with lagged variables with align lagged series
# ------------------------------------------------------------------------------


fit7 <- lm(cmort.fil ~ trend + temp + temp2 + part7 + part.pw, data = dat2)




# ----------
summary(fit7)


summary(fit5)



# -->
# fit7 is worse than fit5 in terms of Multiple R-squared



car::residualPlots(fit7)
