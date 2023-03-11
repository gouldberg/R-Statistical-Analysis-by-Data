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
# Correlation analysis:  autocorrelation and partial autocorrelation
#   - autocorrelation function (ACF)
#   - partial autocorrelation (PACF)
#     regressed the values of the time series at all shorter lags
#     It contrasts with the autocorrelation function, which does not control for other lags
# ------------------------------------------------------------------------------


graphics.off()


# acf2:  produces a simultaneous plot (and a printout) of the sample ACF and PACF
# The zero lag value of the ACF is removed.

astsa::acf2(soi, 48, main = "SOI")




# ----------
astsa::acf2(rec, 48, main = "Recruitment")


# -->
# The ACF and PACF are consistent with the behaviour of an AR(2)
# The ACF has cycles corresponding roughly to a 12 month period, and the PACF has large values for h = 1, 2
# and then is essentially zero for higher order lags.




# -->
# BUT ALSO NOTE that
# SOI:  up to lag 15, large negative PACFs  --> later we check by parametric spectral estimation
# REC:  up to lag 13, large negative PACFs  --> later we check by parametric spectral estimation



# parametric spectral estimation
spec.ar(soi, log = "no")

spec.ar(rec, log = "no")
