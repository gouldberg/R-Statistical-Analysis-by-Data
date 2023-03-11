setwd("//media//kswada//MyFiles//R//star")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  star
# ------------------------------------------------------------------------------

data(star, package = "astsa")


str(star)

head(star)



# ------------------------------------------------------------------------------
# Spectral analysis:  Raw periodogram
#   - It is often convenient to calculate the DFTs(periodogram) using the fast Fourier transformation algorithm (FFT)
#     FFT utilizes a number of redendancies in the calculation of the DFT when n is highly composite, that is, an integer with many factors of 2,3,5,...,
#     the best case being when n = 2^p
#     To accomodate this property, we can pad the centered (or detrended) data of length n to the next highly composite integer by adding zeros.
#     star data has 600 observartions, and exactly 600 days will be used in the spectral analyses by default
# ------------------------------------------------------------------------------

par(mfrow=c(2,1))

# log = "no":  periodogram is plotted on a log10 scale by default and want not scaling
star.per <- astsa::mvspec(star, log = "no")
star.per <- astsa::mvspec(star, log = "yes")


nextn(length(star))

