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
# Correlation analysis:  autocorrelation and partial autocorrelation
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow = c(1, 1))

# acf2:  produces a simultaneous plot (and a printout) of the sample ACF and PACF
# The zero lag value of the ACF is removed.
astsa::acf2(speech, 250, main = "Speech")



# -->
# The original series appears to contain a sequence of repeating short signals.
# The ACF confirms this behaviour, showing repeating peaks spaced at about 106 - 109 points.
# Autocorrelation functions of the short signals appear, spaced at the intervals mentioned above.
# The distance between the repeating signals is known as "pitch period" and is a fundamental parameter of interest in systems
# that encode and decipher speech.
# Because the series is sampled at 10,000 points per second, the pitch period appears to be between 0.0106 and 0.0109 seconds

# The ACF and PACF are consistent with the behaviour of an ARMA(4,0) or ARMA(4,4) ?


