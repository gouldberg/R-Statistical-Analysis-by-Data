setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\m_bnd")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  m_bnd
# ------------------------------------------------------------------------------

data <- read.table("m-bnd.txt", header = FALSE, sep = "")


str(data)



car::some(data)



# ----------
data <- data[,c(4:5)]


colnames(data) <- c("Aaa", "Baa")




# ------------------------------------------------------------------------------
# Cointegration test
# Johansen Procedure for VAR
# ------------------------------------------------------------------------------

library(forecast)


apply(data, 2, ndiffs)





# ----------
# Assuming that the bivariate series of bond yields follows a VAR model,
# we applied the information criteria to specify the order.
# select VAR order

m1 <- MTS::VARorder(data)



# -->
# p = 3 is selected by both BIC and HQ.



# ----------
pacf(data[,1])


pacf(data[,2])



# -->
# AR order for Aaa = 3
# AR order for Baa = 2




# ----------
library(fUnitRoots)


# lags = 3:  maximum number of lags used for error term correction = 3
# type = "nc" for a regression with no intercept (constant) nor time trend
# type = "c":  regression with an intercept (constant) but no time trend
# type = "ct":  regression with an intercept (constant) and a time trend

fUnitRoots::adfTest(data[,1], lags = 3, type = "c")

fUnitRoots::adfTest(data[,2], lags = 2, type = "c")



# -->
# tests fail to reject the null hypothesis of a unit root




# ----------
# Johansen Procedure for VAR
# ecdet = "none" for no intercept in cointegration
# K = 2:  lag order of the series in the VAR

# use maximum eigenvalue

m2 <- ca.jo(data, K = 2, ecdet = c("none"))


summary(m2)



# -->
# The two eigenvalues are 0.055 and 0.005, respectively.
# Compared with critical values, we reject r = 0, but cannot reject r = 1.

# Therefore, there is a cointegrating vector.




# ----------
# use the z(t-1) in ECM.

m3 <- ca.jo(data, K = 2, ecdet = c("none"), spec = "transitory")


summary(m3)




# ----------
# use trace test statistic

m4 <- ca.jo(data, K = 2, ecdet = c("none"), type = "trace", spec = "transitory")


summary(m4)



# -->
# again, the null hypothesis of a cointegration is not rejected.
# The cointegration series is w(t) = (1, -0.886) * z(t)




# ------------------------------------------------------------------------------
# Unit root test for cointegrating series
# ------------------------------------------------------------------------------


# cointegrating series

wt <- data[,1] - 0.886 * data[,2]



# ----------
fUnitRoots::adfTest(wt, lags = 3, type = "c")



# -->
# reject unit root in wt)








