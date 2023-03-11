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
# Smooth Temperature.C
# ------------------------------------------------------------------------------

tempav = CanadianWeather$dailyAv[dayOfYearShifted, , 'Temperature.C']


# ----------
lambda   = 1e2 

fdParobj = fdPar(daybasis, harmaccelLfd, lambda)

temp.fd  = smooth.basis(day.5, tempav, fdParobj)$fd

temp.fd$fdnames = list("Day (July 2 to June 30)",
                       "Weather Station",
                       "Mean temperature (deg. C)")


# ----------
plot(temp.fd)



# ------------------------------------------------------------------------------
# Explore covariation between daily temperature and log precipitation
#   - As in PCA, we can compute nonincreasing series of squared canonical correlations by constraining successive canonical probe values to be orthogonal
#
#   - being careful to avoid the greediness pitfall by placing very heavy penalties on roughness of the canonical weight functions
#     as measured by the size of their second derivatives
# ------------------------------------------------------------------------------

ccafdPar = fdPar(daybasis, 2, 5e6)


# ncan:  the number of canonical variables and weight functions to be computed
ccalist  = cca.fd(temp.fd, logprec.fd, ncan = 3, ccafdPar, ccafdPar)



# ----------
graphics.off()
par(mfrow=c(2,2))
plot(ccalist)




# ----------
# The canonical weight functional data objects and the corresponding 3 squared canonical correlations
# are extracted from the list object ccalist
ccawt.temp    = ccalist$ccawtfd1

ccawt.logprec = ccalist$ccawtfd2

corrs         = ccalist$ccacorr



# The squared canonical correlations
print(corrs[1:3])



# -->
# There is a dominant pair of modes of variation that correlates at a high level, and then two subsequent pairs
# with modest but perhaps interesting correlations.



# ------------------------------------------------------------------------------
# Plot canonical weight function
# ------------------------------------------------------------------------------

ccawtmat.temp    = eval.fd(day.5, ccawt.temp)

ccawtmat.logprec = eval.fd(day.5, ccawt.logprec)



# ----------
par(mfrow=c(1,1), mar=c(2,2,2,2))
plot(day.5, ccawtmat.temp[,1], type='l', lwd=2, cex=2,
     xlab="Day (July 1 to June 30)",
     ylab="Canonical Weight Functions")
lines(day.5, ccawtmat.logprec[,1], lty=2, lwd=2)
lines(yearRng, c(0, 0), lty=3)
legend("bottomleft", c("Temp.", "Log Prec."), lty=c(1,2))



# -->
# The temperature canonical weight function resembles a sinusoid with period 365/2 and having zeros in July, October, January and April.
# But the log precipitation counterpart is close to a sinusoid with period 365 and zeros in July and January.

# The temperature curve seems primarily to contrast spring and autumn temperatures with winter temperature;
# while the corresponding log precipitation contrast is between rainfall in the spring and autumn.

# A station will score high on both canonical variables if it is cool in winter relative to its temperatures in spring and autumn,
# and at the same time has more precipitation in the spring than in the fall.



# ------------------------------------------------------------------------------
# Plot scores for the 1st log precipitation canonical variable scores against their temperature conuterparts for selected weather stations.
# ------------------------------------------------------------------------------
ccascr.temp    = ccalist$ccavar1

ccascr.logprec = ccalist$ccavar2

placeindex = c(35,30,31,19,33,25,24,17,16,8,14,12,15,10,27,6,1,29)

plot(ccascr.temp[,1], ccascr.logprec[,1], type="p", pch="*", cex=2,
     xlim=c(-40,80),
     xlab="Temperature Canonical Weight", 
     ylab="Log Precipitation Canonical Weight")
text(ccascr.temp[placeindex,1]+10, ccascr.logprec[placeindex,1], 
     CanadianWeather$place[placeindex])


# -->
# We see a near-perfect ordering with respect to latitude, although favoring eastern stations over western stations at the same latitudes
# so that Vancouver and Victoria wind up at the bottom left.

# Certainly Resolute's temperatures are cold in winter, and what precipitation it gets comes more in the spring than at another time,
# so that it ears it's place in the upper right of the plot.

# The marine weather stations, Prince Rupert and St. John's, on the other hand,
# are actually relatively warm in the winter and get more precipitation in the fall than in the winter,
# and therefore anchor the lower left of the plot.

# Note, though, that the linear order mises Kamloops by a noticeable amount.
# The position of this interior British Columbia city deep in a valley, where relatively little rain or snow falls at any time of the year,
# causes it to be anomalous in many types of analysis.



