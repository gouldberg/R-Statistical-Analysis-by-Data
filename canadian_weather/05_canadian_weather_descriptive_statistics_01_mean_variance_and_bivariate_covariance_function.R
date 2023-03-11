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
# Smooth data
# ------------------------------------------------------------------------------

# organize data to have winter in the center of the plot
# dayOfYearShifted:  a nmeric vector = c(182:365, 1:181) with names jul01 to jun30
logprecav = CanadianWeather$dailyAv[dayOfYearShifted, , 'log10precip']


# ----------

# set up a saturated basis: as many basis functions as observations
yearRng  = c(0,365)
daybasis = create.fourier.basis(yearRng, 365)



# ----------
#  define the harmonic acceleration operator
Lcoef        = c(0, (2 * pi / diff(yearRng))^2,0)
harmaccelLfd = vec2Lfd(Lcoef, yearRng)



# ----------
# smooth data with lambda that minimizes GCV
lambda     = 1e6
fdParobj   = fdPar(daybasis, harmaccelLfd, lambda)
logprec.fd = smooth.basis(day.5, logprecav, fdParobj)$fd



# ----------
plot(logprec.fd)



# ------------------------------------------------------------------------------
# elementary pointwise mean and standard deviation
# ------------------------------------------------------------------------------

meanlogprec   = mean(logprec.fd)

stddevlogprec = std.fd(logprec.fd)

meanlogprec$coefs


# -->
# The distribution of precipitation is strongly skewed, and by logging these data,
# we effectively work with the geometric mean of precipitation as a more appropriate measure of location in the presence of substantial skewness.



stddevlogprec$coefs


# -->
# The functional standard deviation focuses on the intrinsic variability between observations, e.g., Canadian weather stations,
# after removing variations that are believed to represent measurement and replication error not attributable to the variability between observations.



# ------------------------------------------------------------------------------
# The Bivariate Covariance Function v(s; t)
# ------------------------------------------------------------------------------

# variance, covariance, and correlation surfaces for functional data object
logprecvar.bifd = var.fd(logprec.fd)

logprecvar.bifd$coefs


weektime        = seq(0, 365, length = 53)
logprecvar_mat  = eval.bifd(weektime, weektime, logprecvar.bifd)



# ----------
persp(weektime, weektime, logprecvar_mat,
      theta=-45, phi=25, r=3, expand = 0.5,
      ticktype='detailed',
      xlab="Day (July 1 to June 30)",
      ylab="Day (July 1 to June 30)",
      zlab="variance(log10 precip)")


# -->
# There is much more variation in precipitation in the winter months, positioned in this plot
# in the middle of the surface, because the frigid atmosphere near polar stations like Resolute has almost no capacity to carry moisture,
# while marine stations like Prince Rupert are good for a soaking all year round.



# ----------
contour(weektime, weektime, logprecvar_mat,
        xlab="Day (July 1 to June 30)",
        ylab="Day (July 1 to June 30)")



# ----------
# not week but daily version, but almost same
day5time = seq(0, 365, 5)

logprec.varmat = eval.bifd(day5time, day5time, logprecvar.bifd)

contour(day5time, day5time, logprec.varmat,
        xlab="Day (July 1 to June 30)",
        ylab="Day (July 1 to June 30)", lwd=2,
        labcex=1)



# -->
# We see that variance across weather stations is about five times as large in the winter than it is in the summer.


