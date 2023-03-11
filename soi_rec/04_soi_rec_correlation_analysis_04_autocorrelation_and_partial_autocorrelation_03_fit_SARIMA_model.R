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
# Fit SARIMA (Seasonal ARIMA) model to SOI time series
# ------------------------------------------------------------------------------


# check monthplot
monthplot(soi)



# take 12 months difference
dsoi <- diff(soi, 12)


plot(dsoi, type = "l")



# check ACF and PACF
acf2(dsoi)




# ----------
# take difference of dsoi
ddsoi <- diff(dsoi)


plot(ddsoi, type = "l")


acf2(ddsoi)




# ----------
# ARIMA(1,0,0) * (3,1,1)12

mod_soi_s0 <- sarima(soi, p = 1, d = 0, q = 3, P = 3, D = 1, Q = 1, S = 12)


mod_soi_s0



# -->
# sar1 coefficient is not significant ...




# ------------------------------------------------------------------------------
# Fit SARIMA (Seasonal ARIMA) model to rec time series
# ------------------------------------------------------------------------------



# check monthplot
monthplot(rec)



# check ACF and PACF of original rec
acf2(rec)



# take 18 months difference
drec <- diff(rec, 18)


plot(drec, type = "l")


# check ACF and PACF
acf2(drec)




# ----------
# take difference of drec
ddrec <- diff(drec)


plot(ddrec, type = "l")


acf2(ddrec)




# ----------
# ARIMA(1,0,1) * (1,1,1)18

mod_rec_s0 <- sarima(rec, p = 2, d = 0, q = 1, P = 1, D = 1, Q = 1, S = 18, no.constant = TRUE)


mod_rec_s0



