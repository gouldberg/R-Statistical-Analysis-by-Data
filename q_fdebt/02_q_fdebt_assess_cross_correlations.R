# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R/q_fdebt")

packages <- c("dplyr", "AER", "tseries", "forecast", "dynlm", "stargazer")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  q-fdebt
#   - U.S. quarterly federal debts held by (a) foreign and international investors, (b) federal reserve banks, and 
#     (c) the public.  --> but no (c) ?? in the data set.
#   - The data are from the Federal Reserve Bank of St. Louis, from 1970 to 2012 for 171 observations, and not seasonally adjusted.
#     The debts are in billions of dollars.
# ------------------------------------------------------------------------------

da <- read.table(file = "/media/kswada/MyFiles/references/MultivariateTimeSeriesAnalysisWithRAndFinancialApplications/q-fdebt.txt", header = T)


glimpse(da)
str(da)


# ----------
# take the log
( debt <- log(da[, 3:4]) )

# take 1st diffence
( zt <-diffM(debt) )


# time is converted to year
( tdx <- da[,1] + da[,2] / 12 )



# ------------------------------------------------------------------------------
# Check the order of integration
# ------------------------------------------------------------------------------
forecast::ndiffs(debt[,"hbfin"])
forecast::ndiffs(zt[,"hbfin"])


forecast::ndiffs(debt[,"hbfrbn"])
forecast::ndiffs(zt[,"hbfrbn"])




# ------------------------------------------------------------------------------
# Cross Correlation of differenced data
# ------------------------------------------------------------------------------

# sample cross correlation
graphics.off()
ccf(zt[,"hbfin"], zt[,"hbfrbn"])
ccf(debt[,"hbfin"], debt[,"hbfrbn"])



# ----------
# by MTS::ccm()
MTS::ccm(debt, lag = 10)

MTS::ccm(zt, lag = 10)




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

# There seems to be no cross-correlation

