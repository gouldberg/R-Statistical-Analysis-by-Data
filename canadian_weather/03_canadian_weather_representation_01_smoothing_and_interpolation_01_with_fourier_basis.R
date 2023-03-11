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
# Represent temperature at 4 weather stations by smoothing and interpolation
# ------------------------------------------------------------------------------

Stns = c('Montreal', 'Edmonton', 'Pr. Rupert', 'Resolute')


Temp = CanadianWeather$dailyAv[, Stns, 'Temperature.C']



# ----------
# One expects temperature to be periodic and primarily sinusoidal in character and over the annual cycle.
Temp.fourier   = create.fourier.basis(c(0, 365), 13)



# Create a functional data object
# argvals:  a set of argument values
# day.5:  a numeric vector = dayOfYear - 0.5 = 0.5, 1.5, ..., 364.5
Temp.fd = Data2fd(argvals = day.5, y = Temp, basisobj = Temp.fourier)



Temp.fd$basis


Temp.fd$coefs




# ------------------------------------------------------------------------------
# plot smoothed temperature
# ------------------------------------------------------------------------------

monthIndex = rep(1:12, daysPerMonth)

monthAvTemp = matrix(NA, 12, 4, dimnames=list(month.abb, Stns))

for(i in 1:4) monthAvTemp[, i] = tapply(Temp[, i], monthIndex, mean)

StnLtrs = substring(Stns, 1, 1)



# ----------
graphics.off()

plot(Temp.fd, day.5, axes=FALSE, col=1, lwd=2, xlab='', ylab='Mean Temperature (deg C)')
axis(2, las=1)
axisIntervals(labels=monthLetters)

matpoints(monthMid, monthAvTemp, pch=StnLtrs, lwd=2, col=1)

legend('bottom', paste(Stns, ' (', StnLtrs, ')', sep=''), lty=1:4, col=1, lwd=2, cex = 0.8)




# ------------------------------------------------------------------------------
# Plot the precipitation for Prince Rupert and Resolute with smoothing by fourier basis
# ------------------------------------------------------------------------------

Precip = CanadianWeather$dailyAv[, c('Pr. Rupert', 'Resolute'), 'Precipitation.mm']



# One expects precipitation to be periodic and primarily sinusoidal in character and over the annual cycle.
Precip.fourier   = create.fourier.basis(c(0, 365), 13)


# Create a functional data object
Precip.fd = Data2fd(argvals = day.5, y = Precip, basisobj = Precip.fourier)




# ----------
graphics.off()
par(mfrow=c(1,2))

matplot(Precip, type = "l", lty = 1, main = "original")

plot(Precip.fd, day.5, axes=FALSE, col=1, lwd=2, xlab='', ylab='Mean Precipitation mm', main = "smoothed")
axis(2, las=1)
axisIntervals(labels=monthLetters)

