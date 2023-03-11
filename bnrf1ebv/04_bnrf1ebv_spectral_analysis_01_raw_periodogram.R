setwd("//media//kswada//MyFiles//R//bnrf1ebv")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  bnrf1ebv
# ------------------------------------------------------------------------------

data(bnrf1ebv, package = "astsa")

bnrf1ebv



# ------------------------------------------------------------------------------
# Spectral analysis:  Raw periodogram
#   - It is often convenient to calculate the DFTs(periodogram) using the fast Fourier transformation algorithm (FFT)
#     FFT utilizes a number of redendancies in the calculation of the DFT when n is highly composite, that is, an integer with many factors of 2,3,5,...,
#     the best case being when n = 2^p
#     To accomodate this property, we can pad the centered (or detrended) data of length n to the next highly composite integer by adding zeros.
#     bnrf1ebv data has 3954 observartions, and 4000 months will be used in the spectral analyses by default
# ------------------------------------------------------------------------------

nextn(length(bnrf1ebv))


par(mfrow=c(1,1))

# log = "no":  periodogram is plotted on a log10 scale by default and want not scaling
bnr.per <- astsa::mvspec(bnrf1ebv, log = "no")


