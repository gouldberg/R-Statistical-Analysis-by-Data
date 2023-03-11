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
# Plot the precipitation for Prince Rupert and Resolute with smoothing with roughness penalty
# ------------------------------------------------------------------------------

# set up the harmonic acceleration linear differential operator
yearRng      = c(0,365)

Lbasis       = create.constant.basis(yearRng, axes=list("axesIntervals"))

Lcoef        = matrix(c(0,(2*pi/365)^2,0),1,3)

bfdobj       = fd(Lcoef, Lbasis)

bwtlist      = fd2list(bfdobj)

harmaccelLfd = Lfd(3, bwtlist)




# ----------
# a simpler method:
# The first argument is a vector of coefficients for the operator, and the second argument is the range over which
# the operator is defined.
harmaccelLfd = vec2Lfd(Lcoef, yearRng)



# ----------
# amooth.basisPar(): Smooth data with defined roughness penalty
Prec.fd_p = smooth.basisPar(day.5, Precip, Precip.fourier, harmaccelLfd, lambda=10^7)$fd



Prec.fd_p$basis


Prec.fd_p$coefs



# ----------
graphics.off()
par(mfrow=c(2,2))

matplot(Precip, type = "l", lty = 1, main = "original")

plot(Precip.fd, day.5, axes=FALSE, col=1, lwd=2, xlab='', ylab='Mean Precipitation mm', main = "smoothed")
axis(2, las=1)
axisIntervals(labels=monthLetters)


plot(Prec.fd_p, day.5, axes=FALSE, pch='.', cex=2, xlab="", ylab="Preciipitation (mm)", main = "smoothed with roughness penalty")
axis(2, las=1)
axisIntervals(labels=monthLetters)



# -->
# The point indicate average daily rainfall at Prince Rupert on the northern coast of British Columnbia. (together with Resolute)
# The curve was fit to these data using a roughness penalty method.

# A smooth of precipitation should logically never be negative.
# There is no danger of this happening for a station as moist as Prince Rupert, but a smooth of the data in Resolute,
# the driest place can easily violate this constant.

# We need to impose constraints.

