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
# Plot the scores and check clusters and outliers
# ------------------------------------------------------------------------------

rotpcascores = logprec.rotpcalist$scores



# ----------
graphics.off()

plot(rotpcascores[,1], rotpcascores[,2], type="p", pch="o",
     xlab="Rotated Harmonic I", ylab="Rotated Harmonic II")

text(rotpcascores[,1]+0.2, rotpcascores[,2]+0.2, labels = colnames(logprecav), cex = 0.7)



# -->
# There are some clusters and outliers.
# Most of the stations are contained within two clusters: the upper right with the Atlantic and central Canada stations
# and the lower left with the prairie and mid-Arctic stations.

# The outliers are the three west coast stations and Resolute in the high Arctic.

# Also, Vancouver and Victoria (Southern British Columbia) are unique stations.

