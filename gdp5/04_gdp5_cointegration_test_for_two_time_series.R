# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/gdp5")

packages <- c("dplyr", "AER", "tseries", "forecast", "dynlm", "stargazer")
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
graphics.off()
par(cex = 1.6, mar = c(4,4,2,2), lwd = 1.6, mfrow=c(2,1))

ts.plot(f)
ts.plot(b)



# ----------
# We reject the H0 that residuals have unit roots, therefore the series are conintegrated
fb.dyn <- dynlm(b ~ f)

ehat.fb <- resid(fb.dyn)

output <- dynlm(d(ehat.fb) ~ L(ehat.fb) + L(d(ehat.fb)) + L(d(ehat.fb), 2) - 1)

kable(tidy(output), digits = 4, alighn = "c", caption = "Cointegration Test Between ffr and br")


# -->
# The relevant statistic is -5.5261, wjocj os ;ess tjam tje ansp;ite va;ie pf -3.37,
# the relevant critical value for the cointegration test.

# We reject the null hypothesis that the residuals have unit roots, therefore the series are cointegrated.




# ------------------------------------------------------------------------------
# Cointegration test of federal funds rate and bond rate by Phillips-Ouliaris test (tseries::po.test)
# ------------------------------------------------------------------------------

bfx <- as.matrix(cbind(b,f), demean = FALSE)

tseries::po.test(bfx)


# -->
# The PO test rejects the null of no conintegration, confirming our previous results.

