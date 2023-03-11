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
# Preparation
# ------------------------------------------------------------------------------

logprecav = CanadianWeather$dailyAv[dayOfYearShifted, , 'log10precip']


dayrange  = c(0, 365)
daybasis  = create.fourier.basis(dayrange, 365)

Lcoef        = c(0, (2*pi/diff(dayrange))^2, 0)
harmaccelLfd = vec2Lfd(Lcoef, dayrange)

lambda      = 1e6
fdParobj    = fdPar(daybasis, harmaccelLfd, lambda)
logprec.fit = smooth.basis(day.5, logprecav, fdParobj)
logprec.fd  = logprec.fit$fd



# ----------
plot(logprec.fd, cex = 1.2, xlim = c(0, 365), xlab = "Day", ylab = "log 10 mm")




# ------------------------------------------------------------------------------
# Functional principal components analysis
# ------------------------------------------------------------------------------

# only 2 principla components
nharm = 2

logprec.pcalist = pca.fd(logprec.fd, nharm, centerfns = TRUE)



# ----------
# eigen values
print(logprec.pcalist$values[1:4])

logprec.pcalist$values[1] / sum(logprec.pcalist$values)


# -->
# the 1st harmonic accounts for 88$ of the variation



# ------------------------------------------------------------------------------
# plot two principal components
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(2,2))

plot.pca.fd(logprec.pcalist)


# The expansion supplied by the function is too large, and here we supply a smaller value, 0.5
# plot(logprec.pcalist)
plot(logprec.pcalist, expand=.5)




# -->
# Shows the two principal component functions by displaying the mean curve along +'s and -'s indicating the consequences of adding and subtracting
# a small amount of each principal component.
# Principal component represents variation around the mean, and therefore is naturally plotted as such.
# We see that the first harmonic, accounting for 87.4% of the variation, represents a relative constant vertical shift in the mean,
# and that the second shows essentially a contrast between winter and summer precipitation levels.

# It is in fact usual for unrotated functional principal components to display the same sequence of variation no matter what is being analyzed.
# The first will be a constant shift, the second a linear contrast between the first and second half with a single crossing of zero,
# the third a quadratic pattern, and so on.

# That is, we tend to see the sequence of orthogonal polynomials.
# However, for periodic data, where only periodic harmonics are possible, the linear contrast is suppressed.



# ------------------------------------------------------------------------------
# Rotated (VARIMAX) functional principal components analysis
# ------------------------------------------------------------------------------

logprec.rotpcalist = varmx.pca.fd(logprec.pcalist)

plot.pca.fd(logprec.rotpcalist, expand=.5)



# -->
# 1st component contains the strongest component, with variation primarily in the midwinter.
# 2nd component shows primarily summer variation.

