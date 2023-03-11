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
# Fit ARMA model to SOI time series
# ------------------------------------------------------------------------------

mod_soi0 <- sarima(soi, p = 1, d = 0, q = 3)


mod_soi0




# ----------
# excluding MA term

mod_soi1 <- sarima(soi, p = 1, d = 0, q = 0)


mod_soi1




# ----------
# try AR(15) suggested by parametric spectral estimation

spec.ar(soi, log = "no")

mod_soi2 <- sarima(soi, p = 15, d = 0, q = 0)


mod_soi2




# ----------
mod_soi0$AICc


mod_soi1$AICc


mod_soi2$AICc



# ------------------------------------------------------------------------------
# Fit ARMA model to rec time series
# ------------------------------------------------------------------------------


mod_rec0 <- sarima(rec, p = 2, d = 0, q = 7, no.constant = TRUE)


mod_rec0




# ----------
# cut off MA term to q = 2


mod_rec1 <- sarima(rec, p = 2, d = 0, q = 2, no.constant = TRUE)


mod_rec1




# ----------
# cut off AR term to p = 1


mod_rec2 <- sarima(rec, p = 1, d = 0, q = 2, no.constant = TRUE)


mod_rec2




# ----------
# try AR(13) suggested by parametric spectral estimation

spec.ar(rec, log = "no")

mod_rec3 <- sarima(rec, p = 13, d = 0, q = 0)


mod_rec3




# ----------
mod_rec0$AICc


mod_rec1$AICc


mod_rec2$AICc


mod_rec3$AICc




# ------------------------------------------------------------------------------
# Check the order of difference to be stationary  (if the value > 0, try ARIMA model)
# ------------------------------------------------------------------------------


forecast::nsdiffs(soi)


forecast::nsdiffs(rec)




