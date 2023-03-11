setwd("//media//kswada//MyFiles//R//climhyd")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  climhyd
# ------------------------------------------------------------------------------

data(climhyd, package = "astsa")


str(climhyd)

head(climhyd)


inf <- log(climhyd$Inflow)

prec <- sqrt(climhyd$Precip)



# ------------------------------------------------------------------------------
# Spectral analysis:  Raw periodogram
#   - It is often convenient to calculate the DFTs(periodogram) using the fast Fourier transformation algorithm (FFT)
#     FFT utilizes a number of redendancies in the calculation of the DFT when n is highly composite, that is, an integer with many factors of 2,3,5,...,
#     the best case being when n = 2^p
#     To accomodate this property, we can pad the centered (or detrended) data of length n to the next highly composite integer by adding zeros.
#     climhyd data has 454 observartions, and 480 months will be used in the spectral analyses by default
# ------------------------------------------------------------------------------


nextn(length(climhyd$Temp))


# -->
# actually data is only 454 points (37 years and 10 months)
# now the data is padded and recognized as a series of length 480



# ----------

graphics.off()

par(mfrow=c(3,3))


# log = "no":  periodogram is plotted on a log10 scale by default and want not scaling
temp.per <- astsa::mvspec(climhyd$Temp, log = "no")

dewpt.per <- astsa::mvspec(climhyd$DewPt, log = "no")

cldcvr.per <- astsa::mvspec(climhyd$CldCvr, log = "no")

wndspd.per <- astsa::mvspec(climhyd$WndSpd, log = "no")

precip.per <- astsa::mvspec(climhyd$Precip, log = "no")
precip.per <- astsa::mvspec(prec, log = "no")

inflow.per <- astsa::mvspec(climhyd$Inflow, log = "no")
inflow.per <- astsa::mvspec(inf, log = "no")



# -->
# frequency bandwidth = 0.00208:  1 unit along frequency axis has 1 / 0.00208 = 480 points
# frequency axis "0.1" is 480 * 0.1 = 48 months




# ----------
inflow.per$freq[which.max(inflow.per$spec)]


# -->
# Except for Wind Speed,
# variables have very strong spectrum around 0.0833 frequency (480 / 2 * 0.0833 = 20 months)

