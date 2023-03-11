setwd("//media//kswada//MyFiles//R//sp500w")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  sp500w
#   - weekly S&P 500 returns from to Jan. 2003 to Sept. 2012
# ------------------------------------------------------------------------------

data(sp500w, package = "astsa")


# this is xts object
str(sp500w)



# ------------------------------------------------------------------------------
# Spectral analysis:  Raw periodogram
#   - It is often convenient to calculate the DFTs(periodogram) using the fast Fourier transformation algorithm (FFT)
#     FFT utilizes a number of redendancies in the calculation of the DFT when n is highly composite, that is, an integer with many factors of 2,3,5,...,
#     the best case being when n = 2^p
#     To accomodate this property, we can pad the centered (or detrended) data of length n to the next highly composite integer by adding zeros.
#     sp500W data has 509 observartions, and 512 weeks will be used in the spectral analyses by default
# ------------------------------------------------------------------------------

nextn(length(sp500w))


graphics.off()
par(mfrow=c(1,1))

# log = "no":  periodogram is plotted on a log10 scale by default and want not scaling
sp5.per <- astsa::mvspec(sp500w, log = "no")



# -->
# Does not show predominant spectrum, and need to smooth


