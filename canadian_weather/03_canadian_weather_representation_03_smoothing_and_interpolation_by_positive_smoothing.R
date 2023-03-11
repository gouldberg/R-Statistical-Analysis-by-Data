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
# Positive smoothing
#   - yj = exp[w(tj)] + error = ex[phi(tj)*c] + error
#     fitting function is guaranteed to be positive
# ------------------------------------------------------------------------------

yearRng  = c(0,365)

nbasis   = 365

daybasis = create.fourier.basis(yearRng, nbasis)



# ----------
# set up the harmonic acceleration operator
Lcoef        = c(0, (2 * pi / diff(yearRng))^2, 0)

harmaccelLfd = vec2Lfd(Lcoef, yearRng)



# ----------
lambda      = 1e3

# fdPar():  Define a functional parameter object
WfdParobj   = fdPar(daybasis, harmaccelLfd, lambda)


# we smooth Vancouver's mean daily precipitation data, which can have zero but not negative values
VanPrec     = CanadianWeather$dailyAv[dayOfYearShifted, 'Vancouver', 'Precipitation.mm']


# Positive smoothing
VanPrecPos  = smooth.pos(day.5, VanPrec, WfdParobj)


# not apply positive smoothing
VanPrecPosNot = smooth.basisPar(day.5, VanPrec, daybasis, harmaccelLfd, lambda=lambda)$fd



# ----------
# estimated log precipitation fro positive smoothing
Wfd         = VanPrecPos$Wfdobj

precfit_pos = exp(eval.fd(day.5, Wfd))

precfit_posnot = eval.fd(day.5, VanPrecPosNot)


Wfd$fdnames = list("Day (July 1 to June 30)",
                   "Weather Station" = CanadianWeather$place,
                   "Log 10 Precipitation (mm)")




# ----------
plot(day.5, VanPrec, type="p", cex=1,
     xlab="Day (July 1 to June 30)",
     ylab="Millimeters",
     main="Vancouver's Precipitation", pch = 20)

lines(day.5, precfit_pos, lwd=2, col = "blue")

lines(day.5, precfit_posnot, lwd=1, lty = 2, col = "red")

