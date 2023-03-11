setwd("//media//kswada//MyFiles//R//so2")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  so2
# ------------------------------------------------------------------------------

data(so2, package = "astsa")

str(so2)

so2



# ------------------------------------------------------------------------------
# Spectral analysis:  Raw periodogram
#   - It is often convenient to calculate the DFTs(periodogram) using the fast Fourier transformation algorithm (FFT)
#     FFT utilizes a number of redendancies in the calculation of the DFT when n is highly composite, that is, an integer with many factors of 2,3,5,...,
#     the best case being when n = 2^p
#     To accomodate this property, we can pad the centered (or detrended) data of length n to the next highly composite integer by adding zeros.
#     so2 data has 508 observartions, and 512 weeks will be used in the spectral analyses by default
# ------------------------------------------------------------------------------

nextn(length(so2))



# -->
# actually data is only 508 points
# now the data is padded and recognized as a series of length 512



par(mfrow=c(2,1))


# log = "no":  periodogram is plotted on a log10 scale by default and want not scaling
so2.per <- astsa::mvspec(so2, log = "no")


so2_d.per <- astsa::mvspec(diff(so2), log = "no")


so2.per$bandwidth

so2_d.per$bandwidth




# -->
# around 21 frequency is strong.
# frequency bandwidth = 0.1016:  1 unit along frequency axis has 1 / 0.1016 = 9.85 points
# 21 * 9.85 / 52 = almost 4 years cycle



