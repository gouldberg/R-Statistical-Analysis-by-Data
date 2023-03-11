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
# Spectral analysis:  Raw periodogram
#   - It is often convenient to calculate the DFTs(periodogram) using the fast Fourier transformation algorithm (FFT)
#     FFT utilizes a number of redendancies in the calculation of the DFT when n is highly composite, that is, an integer with many factors of 2,3,5,...,
#     the best case being when n = 2^p
#     To accomodate this property, we can pad the centered (or detrended) data of length n to the next highly composite integer by adding zeros.
#     speech data has 1020 observartions, and 1024 points will be used in the spectral analyses by default
# ------------------------------------------------------------------------------

nextn(length(speech))


par(mfrow=c(2,1))

# log = "no":  periodogram is plotted on a log10 scale by default and want not scaling
speech.per <- astsa::mvspec(speech, log = "no")

speech.per <- astsa::mvspec(speech, log = "yes")



# ----------
speech.per$bandwidth

speech.per$bandwidth * nextn(length(speech))

nextn(length(speech)) * 2 * 0.01

nextn(length(speech)) * 2 * 0.06



