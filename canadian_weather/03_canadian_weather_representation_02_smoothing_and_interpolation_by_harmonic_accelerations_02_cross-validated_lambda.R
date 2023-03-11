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
# Find best smoothing parameter lambda values
#   - Try a range of smoothing parameter lambda values and examine the degrees of freedom and values of the 
#     generalized cross-validation coefficient GCV associated with each value of lambda.
# ------------------------------------------------------------------------------
# CanadianWeather data includes the base 10logarithms of the average annual precipitation in millimeters (after replacing zeros with 0.05)
# for each day of the year at 35 different weather stations.


# organize data to have winter in the center of the plot
dayOfYearShifted = c(182:365, 1:181)


logprecav = CanadianWeather$dailyAv[dayOfYearShifted, , 'log10precip']




# ----------
# set up a saturated basis: as many basis functions as observations
nbasis   = 365

daybasis = create.fourier.basis(yearRng, nbasis)



# ----------
# set up the harmonic acceleration operator
Lcoef        = c(0, ( 2 * pi / diff(yearRng) )^ 2, 0)

harmaccelLfd = vec2Lfd(Lcoef, yearRng)



# ----------
# df: degrees of freedom   gcv:  GCV values
# step through values of log(lambda)
loglam        = seq(4, 9, 0.25)
nlam          = length(loglam)
dfsave        = rep(NA, nlam)
names(dfsave) = loglam
gcvsave       = dfsave


for (ilam in 1:nlam) {
  cat(paste('log10 lambda =',loglam[ilam],'\n'))
  lambda        = 10^loglam[ilam]
  fdParobj      = fdPar(daybasis, harmaccelLfd, lambda)
  smoothlist    = smooth.basis(day.5, logprecav, fdParobj)
  dfsave[ilam]  = smoothlist$df
  
  # The GCV values have to be summed, since function smooth.basis returns a vector of GCV values, one for each replicate
  gcvsave[ilam] = sum(smoothlist$gcv)
}



# ----------
plot(loglam, gcvsave, type='b', lwd=2, ylab='GCV Criterion',
     xlab=expression(log[10](lambda)) )


# -->
# Minimum at log10(lambda) = 6.



# ------------------------------------------------------------------------------
# Smoothing with minimizing value of lambda
# ------------------------------------------------------------------------------

# set best lambda
lambda      = 1e6

fdParobj    = fdPar(daybasis, harmaccelLfd, lambda)

logprec.fit = smooth.basis(day.5, logprecav, fdParobj)

logprec.fd  = logprec.fit$fd

fdnames     = list("Day (July 1 to June 30)",
                   "Weather Station" = CanadianWeather$place,
                   "Log 10 Precipitation (mm)")

logprec.fd$fdnames = fdnames



# ----------
plot(logprec.fd, lwd=2)




# ------------------------------------------------------------------------------
# Plot raw data and fitted curve
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(3,3))

# plotfit.fd:  Pauses between plots
plotfit.fd(logprecav, day.5, logprec.fd, lwd=2)
