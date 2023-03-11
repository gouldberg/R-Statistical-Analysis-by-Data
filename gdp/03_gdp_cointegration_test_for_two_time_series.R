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


u <- gdp$usa
a <- gdp$aus


# ----------
# convert to ts object
is.ts(gdp)

gdp <- ts(gdp, start = c(1970, 1), end = c(2000, 4), frequency = 4)



# ------------------------------------------------------------------------------
# Cointegration test of federal funds rate and bond rate
#  - Two series are cointegrated when their trends are not too far apart and are in some sense similar.
#  - Often economic theory suggests a stable long-run relationship (such as joint trending) between integrated variables which implies cointegration.
#  - Familiar examples are:  short- and long-term interest rates, household imcomes and expenditures, commodity prices (gold) in geographically separated markets, capital appropriations, and expenditures by business
#  - Certain theoretically related variables have an equilibrium relation between them and the equilibrating proess forces them to trend together (joint trending).
#    equilibrium error should not diverge over time, but rather should be stationary
#  - Cointegration implies that in the regression equation y(t) = beta0 + beta1 * x(t) + v(t) , the error term u does not have a unit root, while both y and x do.
#
# Cointegration Test
#  - test whether the residuals from regressin one series on the other one are stationary  (Dickey Fuller stationarity test on residuals)
#  - Phillips-Ouliaris test (po.test)
# ------------------------------------------------------------------------------

# aus(t) = beta1 * usa(t) + e(t)
# --> e(t) = aus(t) - beta1 * usa(t)

cint1.dyn <- dynlm(aus ~ usa - 1, data = gdp)

kable(tidy(cint1.dyn, digits = 3, caption = "The Results of the Cointegration Equation"))



# ----------
ehat <- resid(cint1.dyn)

cint2.dyn <- dynlm(d(ehat) ~ L(ehat) - 1)

tidy(cint2.dyn)



# -->
# Our test rejects the null hypothesis of no cointegration (the critical value is -2.76), meaning that the series are cointegrated.



# ----------
par(cex = 1.2, lwd = 1.6, mfrow=c(1,1))
plot(ehat)



# ------------------------------------------------------------------------------
# Cointegration test of federal funds rate and bond rate by Phillips-Ouliaris test (tseries::po.test)
# ------------------------------------------------------------------------------

axu <- as.matrix(cbind(a, u), demean = FALSE)

tseries::po.test(axu)


# -->
# The PO test does not reject the null of no conintegration, against our previous results ...

