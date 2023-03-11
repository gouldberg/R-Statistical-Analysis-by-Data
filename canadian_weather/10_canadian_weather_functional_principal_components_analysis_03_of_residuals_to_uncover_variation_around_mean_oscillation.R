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
# Smoothed residuals
# ------------------------------------------------------------------------------

logprecmat = eval.fd(day.5, logprec.fd)

logprecres = logprecav - logprecmat



# ----------
# residuals

logprecres.fd = smooth.basis(day.5, logprecres, fdParobj)$fd


plot(logprecres.fd, lwd=2, col=4, lty=1, cex=1.2,
     xlim=c(0,365), ylim=c(-0.07, 0.07),
     xlab="Day", ylab="Residual (log 10 mm)")



# -->
# While most of theese residual functions show fairly chaotic variation, three stations have large oscillations in summer and autumn.



# ------------------------------------------------------------------------------
# Functional principal components analysis for residuals
# ------------------------------------------------------------------------------

logprec.pca1 = pca.fd(logprecres.fd, 1)




# ----------
par(mfrow = c(1,2))

plot(logprec.pca1)


# need small expansion
plot(logprec.pca1, expand=0.01)



# -->
# It defines variation around the mean oscillation located in these months.
# Three stations have much larger scores on this component:  They are Kamloops, Victoria and Vancouver, all in southern British Columnbia.
# It seems that rainfall events come in cycles in this part of Canada at this time of the year, and
# there is interesting structure to be uncovered in these residuals.
