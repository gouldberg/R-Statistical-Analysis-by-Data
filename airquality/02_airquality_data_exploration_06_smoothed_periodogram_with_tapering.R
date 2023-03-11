setwd("//media//kswada//MyFiles//R//airquality")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  airquality
# ------------------------------------------------------------------------------

data("airquality")


str(airquality)

car::some(airquality)




# ------------------------------------------------------------------------------
# Spectral analysis:  smoothed periodogram by modified Daniell kernel
# ------------------------------------------------------------------------------

# We use m = 3 for both times
# This yields Lh = 1 / weights^2 = 1 / { h(-m)^2 + h(-m+1)^2 ...+ h(m-1)^2 + h(m)^2 } = 9.232, close to the value of L = 9
# The bandwidth = 9.232 / 480 = 0.019
# modified degrees of freedom is 2 * Lh * 453 / 480 = 17.43

# obtain coefficients of modefied Daniell kernel

( ker <- kernel("modified.daniell", c(3,3)) )

plot(ker)



# ----------
# Lh = 1 / sum(hk^2)  k = -6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6  --> 9.24, which is close to the value of L = 9
1 / sum(ker$coef[c(6,5,4,3,2,1,2,3,4,5,6)]^2)



# ----------
# taper:  generally have a shape that enhances the center of the data relative to the extremities, such as a cosine bell of the form
#  - taper = 0.1:  tapering the upper and lower 10% of the data
graphics.off()
par(mfrow=c(2,2))

wind.smo <- astsa::mvspec(airquality$Wind, kernel = ker, taper = 0.1, log = "no")

windd.smo <- astsa::mvspec(diff(airquality$Wind), kernel = ker, taper = 0.1, log = "no")

temp.smo <- astsa::mvspec(airquality$Temp, kernel = ker, taper = 0.1, log = "no")

tempd.smo <- astsa::mvspec(diff(airquality$Temp), kernel = ker, taper = 0.1, log = "no")


# -->
# There have been many attempts at dealing with the problem of smoothing the periodogram in a automatic way;
# an early reference is Wahba.
# It is apparent that the smoothing bandwidth for the broadband El Nino behavior (near the 4 year cycle),
# should be much larger than the bandwidth for the annual cycle (the 1 year cycle).
# Consequently, it is perhaps better to perform automatic adaptive smoothing for estimating the spectrum.
# We refer interested readers to Fan and Kreutzberger and the numerous references within.



# ------------------------------------------------------------------------------
# bandwidth
# ------------------------------------------------------------------------------

# bandwidth = Lh / 480 = 9.24 / 480 = 0.1925 ??
soi.smo$bandwidth

rec.smo$bandwidth



# ------------------------------------------------------------------------------
# Modified degrees of freedom
# ------------------------------------------------------------------------------

# df:  modified degrees of freedom = 2 * Lh * 453 / 480 ~ 17.44  ??
( df <- soi.smo$df )



# ------------------------------------------------------------------------------
# Tapered spectrum and non-tapered spectrum:  tapering effect and leakage
#   - For example, a large dynamic range for the values in the spectrum introduces spectra in contiguous frequency intervals
#     several orders of magnitude greater than the value in the interval of interest.
#     This effect is called "leakage"
# ------------------------------------------------------------------------------

# no taper
s0 <- astsa::mvspec(soi, spans = c(7, 7), plot=FALSE)


# full taper
s50 <- astsa::mvspec(soi, spans = c(7, 7), taper = 0.5, plot=FALSE)



# ----------
par(mfrow=c(1,1))

# full taper
plot(s50$freq, s50$spec, log="y", type = "l", ylab = "spectrum", xlab = "frequency")

# no taper
lines(s0$freq, s0$spec, lty = 2)



# -->
# Notice that the tapered spectrum does a better job in separating the yearly cycle (omega = 1) and
# the El Nino cycle (omega = 1/4)

