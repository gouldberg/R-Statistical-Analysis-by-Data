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
# Regression coefficients and its confidence intervals
# ------------------------------------------------------------------------------

resid   = annualprec - annualprechat2

SigmaE = sum(resid ^ 2) / (35 - annPrecTemp$df)

SigmaE  = SigmaE * diag(rep(1, 35))

y2cMap  = tempSmooth65$y2cMap



# ----------
# estimate of sigmaE^2
stderrList = fRegress.stderr(annPrecTemp, y2cMap, SigmaE)

betafd         = betaestlist2[[2]]$fd

betastderrfd   = stderrList$betastderrlist[[2]]



# ----------
plot(betafd, xlab="Day", ylab="Temperature Reg. Coeff.", ylim=c(-6e-4, 1.2e-03), lwd=2)

lines(betafd + 2 * betastderrfd, lty=2, lwd=1)

lines(betafd - 2 * betastderrfd, lty=2, lwd=1)



# -->
# We note that, like the confidence intervals that we derived for probes, these intervals are given pointwise and do not take account of bias
# or of the choice of smoothing parameters.

# In order to provide tests for the overall effectiveness of the regression we resort to permutation tests


