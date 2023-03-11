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
# data exploration:  time series plot
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(2,1))
plot(speech, ylab = "", xlab = "", main = "Speech")
plot(diff(speech), ylab = "", xlab = "", main = "Diff of Speech")



# -->
# One can immediately notice the rather regular repetition of small wavelets.

# 1 large peak in around 110 points and 9 or 10 peaks in 110 points

# Separation between the packets is known as the pitch period and represents the response of the vocal tract filter
# to a periodic sequence of pulses stimulated by the opening and closing of the signals




# ---------
forecast::ndiffs(speech)