setwd("C:\\Users\\kswad\\OneDrive\\デスクトップ\\技術力強化_統計解析\\51_解析スクリプト\\z_for_demo_uncompleted\\q_ungdp_4812")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  q_ungdp_4812
# ------------------------------------------------------------------------------

data <- read.table("q-ungdp-4812.txt", header = TRUE, sep = "")


str(data)



car::some(data)




# ------------------------------------------------------------------------------
# Test for unit root test by Augmented Dickey-Fuller Test
# ------------------------------------------------------------------------------

library(forecast)


apply(data[,c("unemp","gdp")], 2, ndiffs)




# ----------
# Find error-correction lags

astsa::acf2(diff(data$gdp))
sarima(diff(data$gdp), p = 3, d = 0, q = 0)


( m1 <- ar(diff(data$gdp), method = "mle") )


m1$order




# ----------
library(fUnitRoots)


# lags = 3:  maximum number of lags used for error term correction = 3
# type = "nc" for a regression with no intercept (constant) nor time trend
# type = "c":  regression with an intercept (constant) but no time trend
# type = "ct":  regression with an intercept (constant) and a time trend

fUnitRoots::adfTest(data$gdp, lags = 3, type = "nc")

fUnitRoots::adfTest(data$gdp, lags = 3, type = "c")

fUnitRoots::adfTest(data$gdp, lags = 3, type = "ct")



# -->
# all test fail to reject the null hypothesis of a unit root in the U.S. log GDP series.




# ------------------------------------------------------------------------------
# Test for unit root test by Phillps and Perron test
# ------------------------------------------------------------------------------


fUnitRoots::urppTest(data$gdp, type = "Z-alpha")

