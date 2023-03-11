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
# Coefficient beta Estimate Using a Roughness Penalty
# ------------------------------------------------------------------------------

# apply best lambda
lambda      = 10^12.5

betafdPar  = fdPar(betabasis35, harmaccelLfd, lambda)

betalist[[2]] = betafdPar



# ----------
annPrecTemp    = fRegress(annualprec, templist, betalist)



# degrees of freedom for this model, including the intercept, is 4.7
# somewhat below the value of 6 that we used for the simple model before
annPrecTemp$df



# ----------
betaestlist = annPrecTemp$betaestlist



# ----------
# beta for temperature
plot(betaestlist[[2]]$fd, xlab="Day", ylab="Beta for temperature")



# ----------
# intercept term
coef(betaestlist[[1]])



# ------------------------------------------------------------------------------
# Assess the quality of fit
# ------------------------------------------------------------------------------

annualprechat2 = annPrecTemp$yhatfdobj


SSE1.2 = sum((annualprec - annualprechat2)^2)


( RSQ2 = (SSE0 - SSE1.2) / SSE0 )


# -->
# The squared multiple correlation is 0.75
# a small drop from the value for the simple model, due to partly to using fewer degrees of freedom


# ----------
(Fratio2 = ((SSE0-SSE1.2)/3.7)/(SSE1.2/30.3))


# -->
# F-ratio is 25.1 with 3.7 and 30.3 degrees of freedom, and is even more significant than for hte simple model

# But we should note that because a smoothing penalty has been used,
# the F-distribution only represents an approximation to the null distribution for this model.



# ------------------------------------------------------------------------------
# Assess the quality of fit
# observed log annual precipitation values plotted against values predicted by function linear regression
# ------------------------------------------------------------------------------


graphics.off()
par(mfrow=c(1,2), mar = c(2,2,2,2))

plot(annualprechat1, annualprec, lwd=1, col = "black")
abline(0, 1, lty='dashed', lwd=2)
# abline(lm(annualprec ~ annualprechat1), lty='dashed', lwd=2)

plot(annualprechat2, annualprec, lwd=2, col = "blue", add = T)
abline(0, 1, lty='dashed', lwd=2)
# abline(lm(annualprec ~ annualprechat2), lty='dashed', lwd=2)
