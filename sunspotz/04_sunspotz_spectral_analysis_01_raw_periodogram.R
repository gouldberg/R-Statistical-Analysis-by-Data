setwd("//media//kswada//MyFiles//R//sunspotz")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  sunspotz
# ------------------------------------------------------------------------------

data(sunspotz, package = "astsa")

str(sunspotz)

head(sunspotz)



# ------------------------------------------------------------------------------
# Spectral analysis:  Raw periodogram
#   - It is often convenient to calculate the DFTs(periodogram) using the fast Fourier transformation algorithm (FFT)
#     FFT utilizes a number of redendancies in the calculation of the DFT when n is highly composite, that is, an integer with many factors of 2,3,5,...,
#     the best case being when n = 2^p
#     To accomodate this property, we can pad the centered (or detrended) data of length n to the next highly composite integer by adding zeros.
#     sunspotz data has 459 observartions, and 480 months will be used in the spectral analyses by default
# ------------------------------------------------------------------------------

nextn(length(sunspotz))


par(mfrow=c(2,1))


# log = "no":  periodogram is plotted on a log10 scale by default and want not scaling
sun.per <- astsa::mvspec(sunspotz, log = "no")

abline(v = 6/480, lty = 2, lwd = 1)
abline(v = 44/480, lty = 2, lwd = 1)
abline(v = 48/480, lty = 2, lwd = 1)



# ------------------------------------------------------------------------------
# Value of raw periodogram
# ------------------------------------------------------------------------------

sun.per$spec[3]
sun.per$spec[22]
sun.per$spec[24]



# ------------------------------------------------------------------------------
# Confidence intervals
# ------------------------------------------------------------------------------

# Periodgram is not a consistent estimator of the spectral density, so the estimator is susceptible to large uncertainties.
df <- 2
U <- qchisq(0.025, df)
L <- qchisq(0.975, df)

df * sun.per$spec[3] / L
df * sun.per$spec[3] / U

df * sun.per$spec[22] / L
df * sun.per$spec[22] / U

df * sun.per$spec[24] / L
df * sun.per$spec[24] / U
