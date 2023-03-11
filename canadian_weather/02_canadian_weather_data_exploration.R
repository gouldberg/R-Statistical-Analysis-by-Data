setwd("//media//kswada//MyFiles//R//canadian_weather")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  canadian weather
#   - Daily temperature and precipitation at 35 different locations in Canada averaged over 1960 to 1994
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
# data exploration:  temperature time series of some stations
# ------------------------------------------------------------------------------

Stns = c('Montreal', 'Edmonton', 'Pr. Rupert', 'Resolute')


Temp = CanadianWeather$dailyAv[, Stns, 'Temperature.C']


car::some(Temp)



# ----------
matplot(Temp, type = "l", lty = 1)



# -->
# Montreal, with the warmest summer temperature, has a temperature pattern that appears to be nicely sinusoidal.

# Edmonton, with the next warmest summer temperature, seems to have some distinctive departures from sinusoidal variation
# that might call for explanation.

# The marine climate of Prince Rupert is evident in the small amount of annual variation in temperature.

# Resolute has bitterly cold but strongly sinusoidal temperatures.




# ----------
# Southern British Columnbia
Stns = c('Kamloops', 'Vancouver', 'Victoria')


Temp = CanadianWeather$dailyAv[, Stns, 'Temperature.C']


car::some(Temp)



# ----------
par(mfrow = c(1,1), mar = c(2,2,2,2))

matplot(Temp, type = "l", lty = 1)


