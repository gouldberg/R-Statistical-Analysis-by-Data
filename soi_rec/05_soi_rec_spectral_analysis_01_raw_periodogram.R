setwd("//media//kswada//MyFiles//R//soi_rec")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  soi
# data:  rec
# ------------------------------------------------------------------------------


data(soi, package = "astsa")

data(rec, package = "astsa")




# ------------------------------------------------------------------------------
# Spectral analysis:  Raw periodogram
# ------------------------------------------------------------------------------


nextn(length(soi))

# 480 = 256 + 128 + 64 + 32


# -->
# actually data is only 453 points (37 years and 9 months)
# now the data is padded and recognized as a series of length 480




# ----------
par(mfrow=c(2,1))


# log = "no":  periodogram is plotted on a log10 scale by default and want not scaling
soi.per <- astsa::mvspec(soi, log = "no")
abline(v = 1/4, lty = 2)


rec.per <- astsa::mvspec(rec, log = "no")
abline(v = 1/4, lty = 2)



# -->
# frequency bandwidth = 0.025:  1 unit along frequency axis has 1 / 0.025 = 40 points
# the data is 12 months in 1 year --> fequency axis is labeled in multiples of 1/12

# frequency axis "1" is 1 year cycle,  "6" is 6 cycles per 1 year
# frequency 1/4 --> 1 cycle per 4 years

# the 4-year (48 month) cycle omega = 1/4 * 1/12 = 1/48, representing a possible El Nino effect.
# The wide band activity suggests that the possibe El Nino cycle is irregular, but tends to be around four years on average.




# ------------------------------------------------------------------------------
# Value of raw periodogram
# ------------------------------------------------------------------------------

# "1" =  1 / 0.025(bandwidth) = 40  or  1/12 * 1 * 480 = 40

soi.per$spec[40]

soi.per$spec[10]




# ------------------------------------------------------------------------------
# Confidence intervals
# ------------------------------------------------------------------------------

# Periodgram is not a consistent estimator of the spectral density, so the estimator is susceptible to large uncertainties.

df <- 2

U <- qchisq(0.025, df)

L <- qchisq(0.975, df)


df * soi.per$spec[40] / L

df * soi.per$spec[40] / U

df * soi.per$spec[10] / L

df * soi.per$spec[10] / U

