setwd("//media//kswada//MyFiles//R//eqcount")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  eqcount
# ------------------------------------------------------------------------------

data(EQcount, package = "astsa")


str(EQcount)


EQcount




# ------------------------------------------------------------------------------
# Spectral analysis:  Raw periodogram
#   - It is often convenient to calculate the DFTs(periodogram) using the fast Fourier transformation algorithm (FFT)
#     FFT utilizes a number of redendancies in the calculation of the DFT when n is highly composite, that is, an integer with many factors of 2,3,5,...,
#     the best case being when n = 2^p
#     To accomodate this property, we can pad the centered (or detrended) data of length n to the next highly composite integer by adding zeros.
#     EQcount data has 107 observartions, and 108 years will be used in the spectral analyses by default
# ------------------------------------------------------------------------------

nextn(length(EQcount))


graphics.off()
par(mfrow=c(2,1))

# log = "no":  periodogram is plotted on a log10 scale by default and want not scaling
eqc.per <- astsa::mvspec(EQcount, log = "no")
eqc.per2 <- astsa::mvspec(EQcount, log = "yes")



# ----------
eqc.per$spec

eqc.per$freq[4]
eqc.per$freq[17]
eqc.per$freq[29]


nextn(length(EQcount)) / 2 * eqc.per$freq[4]
nextn(length(EQcount)) / 2 * eqc.per$freq[17]
nextn(length(EQcount)) / 2 * eqc.per$freq[29]



# -->
# 2 year, 8.5 year, and 14.5 years cycle has large spectrum


