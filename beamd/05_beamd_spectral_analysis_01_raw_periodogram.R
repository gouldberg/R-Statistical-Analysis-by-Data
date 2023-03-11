setwd("//media//kswada//MyFiles//R//beamd")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  beamd
# ------------------------------------------------------------------------------

data(beamd, package = "astsa")

str(beamd)

head(beamd)


sns1 <- beamd$sensor1
sns2 <- beamd$sensor2
sns3 <- beamd$sensor3



# ------------------------------------------------------------------------------
# Spectral analysis:  Raw periodogram
#   - It is often convenient to calculate the DFTs(periodogram) using the fast Fourier transformation algorithm (FFT)
#     FFT utilizes a number of redendancies in the calculation of the DFT when n is highly composite, that is, an integer with many factors of 2,3,5,...,
#     the best case being when n = 2^p
#     To accomodate this property, we can pad the centered (or detrended) data of length n to the next highly composite integer by adding zeros.
#     beamd data has 2048 observartions, and 2048 points (204.8 secs) will be used in the spectral analyses by default
# ------------------------------------------------------------------------------

nextn(length(sns1))


par(mfrow=c(3,1))


# log = "no":  periodogram is plotted on a log10 scale by default and want not scaling
sns1.per <- astsa::mvspec(sns1, log = "no")
sns2.per <- astsa::mvspec(sns2, log = "no")
sns3.per <- astsa::mvspec(sns3, log = "no")



# ----------
sns1.per <- astsa::mvspec(sns1, log = "yes")
sns2.per <- astsa::mvspec(sns2, log = "yes")
sns3.per <- astsa::mvspec(sns3, log = "yes")


