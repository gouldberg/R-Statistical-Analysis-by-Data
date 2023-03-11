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


# ----------
cons <- ts(cons_inc$cons, start = c(1959,3), end = c(2016,3), frequency = 4)
incm <- ts(cons_inc$y, start = c(1959,3), end = c(2016,3), frequency = 4)


# ----------
# windowed
cons_w <- window(cons, start = c(1985, 1), end = c(2016, 3))
incm_w <- window(incm, start = c(1985, 1), end = c(2016, 3))


# ----------
par(cex = 1.6, mar = c(4, 4, 1, 2), lwd = 1.5)
ts.plot(cons, ylab = "Consumption", xlab = NULL)
ts.plot(incm, ylab = "Disable Income", xlab = NULL)

par(cex = 1.6, mar = c(4, 4, 1, 2), lwd = 1.5)
ts.plot(cons_w, ylab = "Consumption", xlab = NULL)
ts.plot(incm_w, ylab = "Disable Income", xlab = NULL)



# ------------------------------------------------------------------------------
# Cointegration test for Income and Consumption
#  - Two series are cointegrated when their trends are not too far apart and are in some sense similar.
#  - Often economic theory suggests a stable long-run relationship (such as joint trending) between integrated variables which implies cointegration.
#  - Familiar examples are:  short- and long-term interest rates, household imcomes and expenditures, commodity prices (gold) in geographically separated markets, capital appropriations, and expenditures by business
#  - Certain theoretically related variables have an equilibrium relation between them and the equilibrating proess forces them to trend together (joint trending).
#    equilibrium error should not diverge over time, but rather should be stationary
#  - Cointegration implies that in the regression equation y(t) = beta0 + beta1 * x(t) + v(t) , the error term u does not have a unit root, while both y and x do.
#
# 
# dynlm() accepts 3 ways of including a trend term
#  - the argument trend(y):  constructs a time variable using the formula (1:N)/frequency(y)
#  - trend(series, scale = TRUE) constructs a time indx equal to 1:N
#  - the augument time(y): uses the original time index
# ------------------------------------------------------------------------------

cns.dyn <- dynlm(cons_w ~ trend(cons_w) + incm_w)

ehat <- residuals(cns.dyn)

dehat <- dynlm(d(ehat) ~ L(ehat) + L(d(ehat)) - 1)

tab <- tidy(dehat)

kable(tab, digits=3, align="c", caption = "Consumption -- Income Cointegration Test")


# -->
# The relevant statistis is -2.93  --> critical value is -3.42, cannot reject the H0 of non-cointegration





