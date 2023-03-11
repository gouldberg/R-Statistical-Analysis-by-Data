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
# Test for unit root test by Augmented Dickey-Fuller Test
# ------------------------------------------------------------------------------

library(forecast)


apply(data, 2, ndiffs)




# ----------
# Find error-correction lags

astsa::acf2(diff(data$Aaa))
sarima(diff(data$Aaa), p = 2, d = 1, q = 2)


# 1st differenced series is target
( m1 <- ar(diff(data$Aaa), method = "mle") )


m1$order




# ----------
library(fUnitRoots)


# lags = 3:  maximum number of lags used for error term correction = 3
# type = "nc" for a regression with no intercept (constant) nor time trend
# type = "c":  regression with an intercept (constant) but no time trend
# type = "ct":  regression with an intercept (constant) and a time trend

fUnitRoots::adfTest(diff(data$Aaa), lags = 12, type = "nc")

fUnitRoots::adfTest(diff(data$Aaa), lags = 12, type = "c")

fUnitRoots::adfTest(diff(data$Aaa), lags = 12, type = "ct")



# -->
# all test reject the null hypothesis of a unit root





# ------------------------------------------------------------------------------
# Test for unit root test by Phillps and Perron test
# ------------------------------------------------------------------------------


fUnitRoots::urppTest(data$gdp, type = "Z-alpha")

