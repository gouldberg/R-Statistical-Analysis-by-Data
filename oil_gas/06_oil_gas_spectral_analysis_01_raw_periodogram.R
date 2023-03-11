setwd("//media//kswada//MyFiles//R//oil_gas")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Oil and Gas
# ------------------------------------------------------------------------------

data(oil, package = "astsa")
data(gas, package = "astsa")

str(oil)
str(gas)

oil
gas



# ------------------------------------------------------------------------------
# Spectral analysis:  Raw periodogram
#   - It is often convenient to calculate the DFTs(periodogram) using the fast Fourier transformation algorithm (FFT)
#     FFT utilizes a number of redendancies in the calculation of the DFT when n is highly composite, that is, an integer with many factors of 2,3,5,...,
#     the best case being when n = 2^p
#     To accomodate this property, we can pad the centered (or detrended) data of length n to the next highly composite integer by adding zeros.
#     oil and gas data has 545 observartions, and 576 weeks will be used in the spectral analyses by default
# ------------------------------------------------------------------------------

nextn(length(oil))
nextn(length(gas))


par(mfrow=c(2,1))

# log = "no":  periodogram is plotted on a log10 scale by default and want not scaling
oil.per <- astsa::mvspec(oil, log = "no")

gas.per <- astsa::mvspec(gas, log = "no")



# ----------
# log = "no":  periodogram is plotted on a log10 scale by default and want not scaling
oil.per <- astsa::mvspec(diff(log(oil)), log = "no")

gas.per <- astsa::mvspec(diff(log(gas)), log = "no")

