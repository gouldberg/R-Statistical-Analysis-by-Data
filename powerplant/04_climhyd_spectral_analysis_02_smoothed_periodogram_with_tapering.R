setwd("//media//kswada//MyFiles//R//climhyd")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  climhyd
# ------------------------------------------------------------------------------

data(climhyd, package = "astsa")


str(climhyd)

head(climhyd)


inf <- log(climhyd$Inflow)

prec <- sqrt(climhyd$Precip)



# ------------------------------------------------------------------------------
# Spectral analysis:  smoothed periodogram by modified Daniell kernel
# ------------------------------------------------------------------------------

# We use m = 3 for both times
# This yields Lh = 1 / weights^2 = 1 / { h(-m)^2 + h(-m+1)^2 ...+ h(m-1)^2 + h(m)^2 } = 9.232, close to the value of L = 9
# The bandwidth = 9.232 / 480 = 0.019
# modified degrees of freedom is 2 * Lh * 454 / 480 = 17.48

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
par(mfrow=c(1,1))

20 / (480 / 2)

12 / (480 / 2)


temp.smo <- astsa::mvspec(climhyd$Temp, kernel = ker, taper = 0.1, log = "yes")
abline(v = c(0.0833, 1), lty = 2, col = "blue")
abline(v = c(0.05, 1), lty = 2, col = gray(0.3))

dewpt.smo <- astsa::mvspec(climhyd$DewPt, kernel = ker, taper = 0.1, log = "yes")
abline(v = c(0.0833, 1), lty = 2, col = "blue")
abline(v = c(0.05, 1), lty = 2, col = gray(0.3))

cldcvr.smo <- astsa::mvspec(climhyd$CldCvr, kernel = ker, taper = 0.1, log = "yes")
abline(v = c(0.0833, 1), lty = 2, col = "blue")
abline(v = c(0.05, 1), lty = 2, col = gray(0.3))

wndspd.smo <- astsa::mvspec(climhyd$WndSpd, kernel = ker, taper = 0.1, log = "yes")
abline(v = c(0.0833, 1), lty = 2, col = "blue")
abline(v = c(0.05, 1), lty = 2, col = gray(0.3))

precip.smo <- astsa::mvspec(climhyd$Precip, kernel = ker, taper = 0.1, log = "yes")
abline(v = c(0.0833, 1), lty = 2, col = "blue")
abline(v = c(0.05, 1), lty = 2, col = gray(0.3))

precip.smo <- astsa::mvspec(prec, kernel = ker, taper = 0.1, log = "yes")
abline(v = c(0.0833, 1), lty = 2, col = "blue")
abline(v = c(0.05, 1), lty = 2, col = gray(0.3))

inflow.smo <- astsa::mvspec(climhyd$Inflow, kernel = ker, taper = 0.1, log = "yes")
abline(v = c(0.0833, 1), lty = 2, col = "blue")
abline(v = c(0.05, 1), lty = 2, col = gray(0.3))

inflow.smo <- astsa::mvspec(inf, kernel = ker, taper = 0.1, log = "yes")
abline(v = c(0.0833, 1), lty = 2, col = "blue")
abline(v = c(0.05, 1), lty = 2, col = gray(0.3))


# -->
# Notice that smoothed periodogram with log = "yes" shows more subtle spectrum other than 0.0833 frequency



# ------------------------------------------------------------------------------
# Tapered spectrum and non-tapered spectrum:  tapering effect and leakage
#   - For example, a large dynamic range for the values in the spectrum introduces spectra in contiguous frequency intervals
#     several orders of magnitude greater than the value in the interval of interest.
#     This effect is called "leakage"
# ------------------------------------------------------------------------------

# no taper
s0 <- astsa::mvspec(climhyd$Temp, spans = c(7, 7), plot=FALSE)


# full taper
s50 <- astsa::mvspec(climhyd$Temp, spans = c(7, 7), taper = 0.5, plot=FALSE)



# ----------
par(mfrow=c(1,1))

# full taper
plot(s50$freq, s50$spec, log="y", type = "l", ylab = "spectrum", xlab = "frequency")

# no taper
lines(s0$freq, s0$spec, lty = 2)



# -->
# Notice that the tapered spectrum does a better job in separating various cycles



# ------------------------------------------------------------------------------
# Spectral analysis:  Display all the smoothed periodogram in one display
# ------------------------------------------------------------------------------


inf <- log(climhyd$Inflow)

prec <- sqrt(climhyd$Precip)

dat <- cbind(climhyd, inf, prec)


# Display all the smoothed periodogram
plot(mvspec(dat, kernel = ker, taper = 0.1, log = "yes"))


