setwd("//media//kswada//MyFiles//R//tse")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  tse
# ------------------------------------------------------------------------------

data("tse", package = "gamlss.data")


str(tse)

car::some(tse)



# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------

summary(tse[,c("ret", "currency", "tl")])


psych::describe(tse[,c("ret", "currency", "tl")])



# -->
# kurtosis < 3 for ret and tl



# ----------
par(mfrow=c(1,1), mar = c(2,2,2,2))

plot(tse$ret, type = "l")




# ----------
# returns time series are stationary
forecast::ndiffs(tse$ret)



# ----------
# Histogram of the Turkish stock exchange returns data
MASS::truehist(tse$ret, xlab = "TSE")



