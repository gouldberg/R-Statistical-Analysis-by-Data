setwd("//media//kswada//MyFiles//R//canadian_weather")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  canadian weather
# ------------------------------------------------------------------------------

data("CanadianWeather", package = "fda")

str(CanadianWeather)



# ----------
CanadianWeather$dailyAv[,,'Temperature.C']

CanadianWeather$dailyAv[,,'Precipitation.mm']

CanadianWeather$dailyAv[,,'log10precip']

CanadianWeather$place

CanadianWeather$province

CanadianWeather$coordinates

CanadianWeather$monthlyTemp

CanadianWeather$monthlyPrecip




# ------------------------------------------------------------------------------
# Functional analysis of variance:  climate region effects on temperature
# ------------------------------------------------------------------------------

dayOfYearShifted = c(182:365, 1:181)

tempShifted  = daily$tempav[dayOfYearShifted, ]



# ----------
# if we use nbasis = 65, gls model produces singular value error ...

# nbasis = 65
nbasis = 25
# tempbasis    = create.fourier.basis(rangeval = c(0, 365), nbasis = nbasis, axes = list('axesIntervals'))
tempbasis    = create.fourier.basis(rangeval = c(0, 365), nbasis = nbasis)

tempSmooth = smooth.basis(day.5, tempShifted, tempbasis)

tempfd       = tempSmooth$fd



# ----------
# Manually set up a model matrix and sum-to-zero constraints for 4 region coefficient functions
( modmat = cbind(1, model.matrix(~ factor(CanadianWeather$region) - 1)) )

constraints = matrix(c(0, 1, 1, 1, 1), 1)




# ------------------------------------------------------------------------------
# Penalized OLS, with smoothing parameter chosen by grid
# ------------------------------------------------------------------------------

lambda = 100 * 10:30

olsmod = fosr(fdobj = tempfd, X = modmat, con = constraints, method = "OLS")

olsmod2 = fosr(fdobj = tempfd, X = modmat, con = constraints, method = "OLS", lambda = lambda)




# ------------------------------------------------------------------------------
# Penalized GLS
# ------------------------------------------------------------------------------

glsmod = fosr(fdobj = tempfd, X = modmat, con = constraints, method = "GLS")




# ------------------------------------------------------------------------------
# plot function for intercept and region effect and its 95% confidence intervals
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow=c(3,5), mar = c(5,2,4,1))

plot(olsmod, set.mfrow = FALSE, titles = c("OLS: Intercept", levels(factor(CanadianWeather$region))), ylab = "", xlab = "Day")

plot(olsmod2, set.mfrow = FALSE, titles = c("OLS: Intercept", levels(factor(CanadianWeather$region))), ylab = "", xlab = "Day")

plot(glsmod, set.mfrow = FALSE, titles = c("GLS: Intercept", levels(factor(CanadianWeather$region))), ylab = "", xlab = "Day")




# -->
# Due to the spacial dependence between the temperature curves at neightboring locations,
# the error curves are not independent, they are correlated.
# Ordinaly least squared estimator is optimal if the error curves are uncorrelated.
# In canse of dependence, an estimator that takes the dependence into account may be better.
# Such estimators are generally referred to as generalized least squares (GLS) estimators.


# Here, The estimates are quite similar, but in this case GLS yields somewhat smoother coefficient functions than OLS.

