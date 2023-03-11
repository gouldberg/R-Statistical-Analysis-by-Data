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
# Detrending SOI and check ACF and PACF
# ------------------------------------------------------------------------------

# Detrended SOI
soi.d <- resid(lm(soi ~ time(soi), na.action = NULL))


acf2(soi.d)



# -->
# From PACF, an autoregressive series with p = 1 will do a reasonable job



# ------------------------------------------------------------------------------
# prewhiten SOI
# ------------------------------------------------------------------------------

# model detrended residuals by AR(1)
fit <- arima(soi.d, order = c(1, 0, 0))

fit



# ----------
# soi.pw:  prewhitened detrended SOI series

soi.pw <- resid(fit)



# ----------
# plot prewhitened SOI and its autocorrelation
par(mfrow = c(2,1))

plot(soi)

plot(soi.pw)



acf2(soi)

acf2(soi.pw)



# -->
# Still some irregular correlations are remained but short-term correlations are dissappeared.




# ------------------------------------------------------------------------------
# Extract filter coefficients
# ------------------------------------------------------------------------------

( ar1 <- as.numeric(coef(fit)[1]) )


# -->
# phi = 0.588 and sigma^2 = 0.092




# ------------------------------------------------------------------------------
# Filtered rec by ar1 coefficient
# ------------------------------------------------------------------------------


# rec.fil:  the filtered (transformed) Recruitment series
# we apply the operator (1 - 0.588 * B) also to REC


rec.fil <- stats::filter(rec, filter = c(1, -ar1), sides = 1)




# ------------------------------------------------------------------------------
# Check cross-correlation
# ------------------------------------------------------------------------------

# Sample CCF of the prewhitened, detrended SOI and the similarly transformed Recruitmenet series
par(mfrow = c(1,1))

ccf(soi.pw, rec.fil, ylab = "CCF", na.action = na.omit, panel.first = grid())



# -->
# soi.pw leads to rec.fill by 5 lags


