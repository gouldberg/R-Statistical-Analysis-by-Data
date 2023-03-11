setwd("//media//kswada//MyFiles//R//beamd")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  beamd
# ------------------------------------------------------------------------------

data(beamd, package = "astsa")

str(beamd)

head(beamd)


sns1 <- beamd$sensor1
sns2 <- beamd$sensor2
sns3 <- beamd$sensor3



# ------------------------------------------------------------------------------
# Spectral analysis:  smoothed periodogram by modified Daniell kernel
# ------------------------------------------------------------------------------

m <- 3

( ker <- kernel("modified.daniell", c(m, m)) )

plot(ker)



# ----------
# taper:  generally have a shape that enhances the center of the data relative to the extremities, such as a cosine bell of the form
#  - taper = 0.1:  tapering the upper and lower 10% of the data
graphics.off()
par(mfrow=c(3,1))

sns1.smo <- astsa::mvspec(sns1, kernel = ker, taper = 0.1, log = "no")

sns2.smo <- astsa::mvspec(sns2, kernel = ker, taper = 0.1, log = "no")

sns3.smo <- astsa::mvspec(sns3, kernel = ker, taper = 0.1, log = "no")



# ------------------------------------------------------------------------------
# bandwidth and modified degrees of freedom
# ------------------------------------------------------------------------------

sns1.smo$bandwidth

sns1.smo$df



# ------------------------------------------------------------------------------
# Tapered spectrum and non-tapered spectrum:  tapering effect and leakage
#   - For example, a large dynamic range for the values in the spectrum introduces spectra in contiguous frequency intervals
#     several orders of magnitude greater than the value in the interval of interest.
#     This effect is called "leakage"
# ------------------------------------------------------------------------------

# no taper
s0 <- astsa::mvspec(sns1, spans = c(7, 7), plot=FALSE)


# full taper
s50 <- astsa::mvspec(sns1, spans = c(7, 7), taper = 0.5, plot=FALSE)



# ----------
par(mfrow=c(1,1))

# full taper
plot(s50$freq, s50$spec, log="y", type = "l", ylab = "spectrum", xlab = "frequency")

# no taper
lines(s0$freq, s0$spec, lty = 2)



# -->
# NOTICE that full tapered spectrum > no taper spectrum ...
# This is reversed result of other time series ...

