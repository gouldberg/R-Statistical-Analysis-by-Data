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
# Harmonic accelerations of temperature curves
# ------------------------------------------------------------------------------

Stns = c('Montreal', 'Edmonton', 'Pr. Rupert', 'Resolute')

Temp = CanadianWeather$dailyAv[, Stns, 'Temperature.C']



# ----------
# set up the harmonic acceleration linear differential operator
yearRng      = c(0,365)

Lbasis       = create.constant.basis(yearRng, axes=list("axesIntervals"))

Lcoef        = matrix(c(0,(2*pi/365)^2,0),1,3)

bfdobj       = fd(Lcoef, Lbasis)

bwtlist      = fd2list(bfdobj)



# ----------
# Lfd(): define linear differential operator object
# nderiv: a nonnegative integer specifying the order $m$ of the highest order derivative in the operator

harmaccelLfd = Lfd(nderiv = 3, bwtlist)



# ----------
# a simpler method:
# The first argument is a vector of coefficients for the operator, and the second argument is the range over which
# the operator is defined.
harmaccelLfd = vec2Lfd(Lcoef, yearRng)



# ----------
######  EVALUATE THE HARMONIC ACCELERATION CURVES  #####
Lfd.Temp = deriv(Temp.fd, harmaccelLfd)



Lfd.Temp$basis


Lfd.Temp$coefs



# ----------
graphics.off()
par(mfrow = c(1,1))

plot(Lfd.Temp, day.5, axes=FALSE, xlab='', ylab='L-Temperature', col=1:4, lwd=2)
axis(2, las=1)
axisIntervals(labels=monthLetters)

legend('bottomleft', paste(Stns, ' (', StnLtrs, ')', sep=''), lty=1:4, lwd=2, cex = 0.7, col = 1:4)




# -->
# If a temperature function is truly sinusoidal, then Lfd.Temp should be zero.
# But here display systematic features that are especially strong in the summer and autumn months.
# Temperature at a particular weather station can be described as the solution of the non-homogeneous differential equation corresponding to Lfd.Temp = u,
# where the forcing funciton u can be viewed as input from outside of the system, or as an exogeneous influence.

# Meteorologists suggest, for example, that these spring and autumn effects are partly due to the change in the reflectance of land when snow or ice melts,
# and this would be consistent with the fact that the least sinusoidal records are associated with continental stations well separated from
# large bodies of water.


# -->
# Here important is that it is interesing to remove effects of a simple character by applying a differential operator,
# rather than by simply subtracting them.
# This exploits the intrinsic smoothness in the process.
