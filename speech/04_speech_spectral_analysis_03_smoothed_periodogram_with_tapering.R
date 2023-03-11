setwd("//media//kswada//MyFiles//R//speech")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  speech
# ------------------------------------------------------------------------------

data(speech, package = "astsa")

str(speech)


head(speech)



# ------------------------------------------------------------------------------
# Spectral analysis:  smoothed periodogram by modified Daniell kernel
# ------------------------------------------------------------------------------

m <- 3

# obtain coefficients of modefied Daniell kernel
( ker <- kernel("modified.daniell", c(m, m)) )

plot(ker)


# ----------
# taper:  generally have a shape that enhances the center of the data relative to the extremities, such as a cosine bell of the form
#  - taper = 0.1:  tapering the upper and lower 10% of the data
graphics.off()
par(mfrow=c(2,1))

speech.smo <- astsa::mvspec(speech, kernel = ker, taper = 0.1, log = "no")

speech.smo <- astsa::mvspec(speech, kernel = ker, taper = 0.1, log = "yes")


# -->
# smoothed spectrum does better job to represent speech data spectrum



# ------------------------------------------------------------------------------
# bandwidth and degrees of freedom
# ------------------------------------------------------------------------------

speech.smo$bandwidth


speech.smo$df



# ------------------------------------------------------------------------------
# Tapered spectrum and non-tapered spectrum:  tapering effect and leakage
#   - For example, a large dynamic range for the values in the spectrum introduces spectra in contiguous frequency intervals
#     several orders of magnitude greater than the value in the interval of interest.
#     This effect is called "leakage"
# ------------------------------------------------------------------------------

# no taper
s0 <- astsa::mvspec(speech, spans = c(7, 7), plot=FALSE)


# full taper
s50 <- astsa::mvspec(speech, spans = c(7, 7), taper = 0.5, plot=FALSE)



# ----------
par(mfrow=c(1,1))

# full taper
plot(s50$freq, s50$spec, log="y", type = "l", ylab = "spectrum", xlab = "frequency")

# no taper
lines(s0$freq, s0$spec, lty = 2)


