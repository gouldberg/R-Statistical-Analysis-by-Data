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
# Low-Dimensional Regression Coefficient Function beta
# ------------------------------------------------------------------------------

# Response
( annualprec   = log10(apply(daily$precav, 2, sum)) )



# ----------
# We will use 65 basis functions without roughness penalty.
# This number of basis functions has been found to be adequate for most purposes, and can, for example, capture the ripples observed
# in early spring in many weather stations.

tempbasis65  = create.fourier.basis(c(0,365), 65)

tempSmooth65 = smooth.basis(day.5, daily$tempav, tempbasis65)

tempfd65     = tempSmooth65$fd

templist      = vector("list",2)



# The intercept term is here set up as a constant function with 35 replications
( templist[[1]] = rep(1,35) )

( templist[[2]] = tempfd65 )



# ----------
# Low Dimensional Regression Coefficient Function beta

# constant function, the multiplier of the constant intercept covariate set up above.
conbasis   = create.constant.basis(c(0,365))

# We will work with the 5 Fourier basis functions for the regression coef beta multiplying the temperature profiles.
betabasis5 = create.fourier.basis(c(0,365),5)

betalist1  = vector("list",2)

( betalist1[[1]] = conbasis )

( betalist1[[2]] = betabasis5 )



# ----------
fRegressList1 = fRegress(annualprec, xfdlist = templist, betalist = betalist1)



# ----------
# beta for temperature
betaestlist1  = fRegressList1$betaestlist

tempbetafd1   = betaestlist1[[2]]$fd

plot(tempbetafd1, xlab="Day", ylab="Beta for temperature")



# ----------
# intercept term
coef(betaestlist1[[1]])



# ------------------------------------------------------------------------------
# Assess the quality of fit
# ------------------------------------------------------------------------------

annualprechat1 = fRegressList1$yhatfdobj

annualprecres1 = annualprec - annualprechat1

SSE1.1  = sum(annualprecres1^2)

SSE0    = sum((annualprec - mean(annualprec))^2)



# ----------
# Squared multiple correlation is 0.80
( RSQ1   = (SSE0 - SSE1.1) / SSE0 )



# ----------
# The corresponding F-ratio with 5 and 29 degrees of freedom is 22.6
( Fratio1 = ((SSE0-SSE1.1) / 5) / (SSE1.1 / 29))



# -->
# Suggesting a fit to the data is far better than we would expect by chance.

