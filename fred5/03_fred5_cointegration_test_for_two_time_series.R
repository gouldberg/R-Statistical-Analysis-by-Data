# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/fred5")

packages <- c("dplyr", "AER", "tseries", "forecast", "dynlm", "stargazer")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  fred5
#  - Personal Consumtion and Personal Disposable Income already in logs over the period of 1986Q1 through 2015Q2
# ------------------------------------------------------------------------------
data("fred5", package = "POE5Rdata")


glimpse(fred5)

str(fred5)


# ----------
# convert to ts object
is.ts(fred5)

fred <- ts(fred5, start = c(1986, 1), end = c(2015, 2), frequency = 4)



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

# consn(t) = beta1 * y(t) + e(t)
# --> e(t) = consn(t) - beta1 * y(t)

cointcy <- dynlm(consn ~ y - 1, data = fred5)

kable(tidy(cointcy, digits = 3, caption = "The Results of the Cointegration Equation"))



# ----------
ehat <- resid(cointcy)



# ----------
par(cex = 1.2, lwd = 1.6, mfrow=c(1,1))
plot(ehat, type = "l")



# ----------
adf.test(ehat)



# -->
# The series are both I(1) but they fail the cointegration test.




# ------------------------------------------------------------------------------
# Cointegration test of federal funds rate and bond rate by Phillips-Ouliaris test (tseries::po.test)
# ------------------------------------------------------------------------------

cxy <- as.matrix(cbind(fred5$consn, fred5$y), demean = FALSE)

tseries::po.test(cxy)


# -->
# The PO test reject the null of no conintegration  --> against our previous results...

