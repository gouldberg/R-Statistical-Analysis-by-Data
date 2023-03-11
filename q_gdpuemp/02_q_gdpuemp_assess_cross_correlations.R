# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/q_gdpuemp")

packages <- c("dplyr", "AER", "tseries", "forecast", "dynlm", "stargazer")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  q_gdpuemp
# ------------------------------------------------------------------------------
da <- read.table(file = "/media/kswada/MyFiles/references/MultivariateTimeSeriesAnalysisWithRAndFinancialApplications/q-gdpunemp.txt", header = T)


glimpse(da)
str(da)


# ----------
# convert to ts object
is.ts(da)

da <- ts(da, start = c(1948, 1), end = c(2011, 4), frequency = 4)



# ------------------------------------------------------------------------------
# Check the order of integration
# ------------------------------------------------------------------------------
forecast::ndiffs(da[,"gdp"])

forecast::ndiffs(da[,"rate"])


# Order of integration of gdp = 2, order of integration of rate = 1


plot(diff(da[,c("rate","gdp")]))



# ------------------------------------------------------------------------------
# Cross Correlation of differenced data
# ------------------------------------------------------------------------------

x = cbind(diff(da[,"rate"]), diff(da[,"gdp"]))


# ----------
# sample cross correlation of first difference of gdp and unemployment rate by ccf()
ccf(diff(da[,"rate"]), diff(da[,"gdp"]))


# -->
# 1st difference of Unemployment rate has 1-3 lags (delay) to gdp



# ----------
# sample cross correlation of first difference of gdp and unemployment rate by MTS::ccm()
MTS::ccm(x, lag = 10)



# ----------
ccf(diff(da[,"gdp"]), diff(da[,"rate"]))



# ------------------------------------------------------------------------------
# Testing zero cross-correlation
#   - multivariate Ljung-Box test (multivariate Portmanteau test)
#   - Under the null hypothesis that lag l cross-covariance matrix  = 0 for l > 0 and the condition that y(t) is normally distributed,
#     Qk(m) (the test statistic) is asymptotically distributed as X^2(mk^2), that is, a chisquare distribution with mk^2 degrees of freedom.
#     rho(l): lag l cross-correlation matrix
#     H0: rho1 = rho2 = ... = rho10 = ... = 0 versus H1: rho(i) <> 0 for some i
# ------------------------------------------------------------------------------

# Compute Q(m) statistics
MTS::mq(x, lag = 10)


# -->
# the time plot of p-values of the Qk(m) statistic and dashed line of the plot denotes the type I error of 5%.
# all p-values are less than 5% (null hypothesis is rejected), confirming that the series has CCMs. (not zero CCMs)












