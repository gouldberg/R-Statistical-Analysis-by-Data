setwd("//media//kswada//MyFiles//R//birth")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  birth
# ------------------------------------------------------------------------------

data(birth, package = "astsa")

str(birth)

head(birth)



# ------------------------------------------------------------------------------
# Spectral analysis:  Raw periodogram
#   - It is often convenient to calculate the DFTs(periodogram) using the fast Fourier transformation algorithm (FFT)
#     FFT utilizes a number of redendancies in the calculation of the DFT when n is highly composite, that is, an integer with many factors of 2,3,5,...,
#     the best case being when n = 2^p
#     To accomodate this property, we can pad the centered (or detrended) data of length n to the next highly composite integer by adding zeros.
#     birth data has 373 observartions, and 375 months will be used in the spectral analyses by default
# ------------------------------------------------------------------------------

nextn(length(birth))


par(mfrow=c(2,1))

# log = "no":  periodogram is plotted on a log10 scale by default and want not scaling
bir.per <- astsa::mvspec(birth, log = "no")



# ----------
# log = "no":  periodogram is plotted on a log10 scale by default and want not scaling
bir.per <- astsa::mvspec(diff(log(birth)), log = "no")

une2.per <- astsa::mvspec(diff(log(UnempRate)), log = "no")



# -->
# 60 months cycle has largest spectrum
