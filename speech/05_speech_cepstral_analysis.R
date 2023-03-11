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
# Extract logged raw spectrum
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(1,1))

speech.per <- astsa::mvspec(speech, log = "yes")

speech.per$detrend



# ------------------------------------------------------------------------------
# Extract cepstrum
#   -  Bogart et al. proposed treating the detrended log spectrum as a pseudo time series and 
#      calculating its spectrum (or cepstrum), which should show a peak at quefrency correspondin to 1/D
# ------------------------------------------------------------------------------

############ NOT YET COMPLETED !!! *################


# Detrended log spectrum
speech_sspec <- resid(lm(speech.per$spec ~ time(speech.per$spec)))


plot(speech_sspec, type = "l")



# ----------
# cepstrum:  calculating spectrum of detrended log spectrum
par(mfrow = c(1,1))
cep <- astsa::mvspec(speech.per$spec, log = "no")



# original spectrum
speech.per <- astsa::mvspec(speech, log = "no")




# ----------
# largest spectrum at 53rd points (1st point is excluded)
( idx <- which.max(cep$spec[2:length(cep$spec)]) + 1 )

cep$spec[idx]





