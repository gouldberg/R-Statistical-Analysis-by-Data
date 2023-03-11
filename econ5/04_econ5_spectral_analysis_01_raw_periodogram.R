setwd("//media//kswada//MyFiles//R//econ5")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  econ5
# ------------------------------------------------------------------------------

data(econ5, package = "astsa")

str(econ5)


car::some(econ5)




# ----------
# data transformation
# First logging and then removing the linear trend

U <- resid(lm(log(econ5$unemp) ~ time(log(econ5$unemp))))

G <- resid(lm(log(econ5$gnp) ~ time(log(econ5$gnp))))

C <- resid(lm(log(econ5$consum) ~ time(log(econ5$consum))))


x <- cbind(G, U, C)



# ----------
Ud <- diff(econ5$unemp)

Gd <- diff(econ5$govinv)

Cd <- diff(econ5$consum)



dat <- cbind(Gd, Ud, Cd)




# ------------------------------------------------------------------------------
# Spectral analysis:  Raw periodogram
#   - It is often convenient to calculate the DFTs(periodogram) using the fast Fourier transformation algorithm (FFT)
#     FFT utilizes a number of redendancies in the calculation of the DFT when n is highly composite, that is, an integer with many factors of 2,3,5,...,
#     the best case being when n = 2^p
#     To accomodate this property, we can pad the centered (or detrended) data of length n to the next highly composite integer by adding zeros.
#     SOI data has 453 observartions, and 480 months will be used in the spectral analyses by default
# ------------------------------------------------------------------------------


nextn(nrow(dat))



# -->
# actually data is only 160 points
# now the data is padded and recognized as a series of length 160 (no padding)




# ----------
graphics.off()
par(mfrow=c(2,2))


# log = "no":  periodogram is plotted on a log10 scale by default and want not scaling

Ud.per <- astsa::mvspec(Ud, log = "no")
Gd.per <- astsa::mvspec(Gd, log = "no")
Cd.per <- astsa::mvspec(Cd, log = "no")



Ud.per$bandwidth

Ud.per$df

data.frame(Ud.per$details) %>% arrange(desc(spectrum)) %>% head()



# -->
# frequency bandwidth = 0.00625:  1 unit along frequency axis has 1 / 0.00625 = 160 points
# frequency axis "0.1" is 16 quarters (2 years)

# at frequency = 0.1062, largest spectrum 2.09 (9.4 quarter / cycle)




# ------------------------------------------------------------------------------
# Confidence intervals
# ------------------------------------------------------------------------------

# Periodgram is not a consistent estimator of the spectral density, so the estimator is susceptible to large uncertainties.

df <- 2

U <- qchisq(0.025, df)

L <- qchisq(0.975, df)


df * Ud.per$spec[17] / L

df * Ud.per$spec[17] / U
