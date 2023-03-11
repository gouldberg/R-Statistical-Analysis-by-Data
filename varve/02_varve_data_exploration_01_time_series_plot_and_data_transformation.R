setwd("//media//kswada//MyFiles//R//varve")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  varve
# ------------------------------------------------------------------------------

data(varve, package = "astsa")

str(varve)



# ------------------------------------------------------------------------------
# data exploration:  data transformation to logarithms
# ------------------------------------------------------------------------------

par(mfrow = c(3,1))

plot(varve, main = "varve", ylab = "")


# nonstationarity can be improved by transforming to logarithms
plot(log(varve), main = "log(varve)", ylab = "")


# some additional nonstationarity can be corrected by differencing the logarithms
# Note that the difference of logarithm is percentage change
plot(diff(log(varve)), main = "1st differencing of log(varve)", ylab = "")



# ----------
forecast::ndiffs(varve)




# ------------------------------------------------------------------------------
# Box Cox Transformation
# ------------------------------------------------------------------------------

caret::BoxCoxTrans(varve)


# -->
# best lambda is -0.1 (almost zero, meaning log transformation)



# ------------------------------------------------------------------------------
# sample variance over 1st half and 2nd half to assess stabilization by transforming
# ------------------------------------------------------------------------------

# sample variance over 1st half and 2nd half
var(varve[318:634]) / var(varve[1:317])

var(log(varve[318:634])) / var(log(varve[1:317]))



# ----------
par(mfrow = c(2,1))

hist(varve)

hist(log(varve))



# -->
# The transformation to logarithms stabilizes the variance over the series.

