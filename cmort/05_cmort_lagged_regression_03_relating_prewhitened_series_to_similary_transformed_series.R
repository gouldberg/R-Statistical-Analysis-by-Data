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
# prewhiten particulate
# ------------------------------------------------------------------------------


acf2(part)


fit <- arima(part, order = c(4, 0, 0))

fit




# ----------
# part.pw:  prewhitened part

part.pw <- resid(fit)



# ----------
# plot prewhitened SOI and its autocorrelation
par(mfrow = c(2,1))

plot(part)

plot(part.pw)



acf2(part)

acf2(part.pw)



# -->
# Still some irregular correlations are remained but short-term correlations are dissappeared.




# ------------------------------------------------------------------------------
# Extract filter coefficients
# ------------------------------------------------------------------------------

ar1 <- as.numeric(coef(fit)[1])

ar2 <- as.numeric(coef(fit)[2])

ar3 <- as.numeric(coef(fit)[3])

ar4 <- as.numeric(coef(fit)[4])




# ------------------------------------------------------------------------------
# Filtered cmort by ar1 coefficient
# ------------------------------------------------------------------------------


cmort.fil <- stats::filter(cmort, filter = c(1, -ar1, -ar2, -ar3, -ar4), sides = 1)




# ------------------------------------------------------------------------------
# Check cross-correlation
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

ccf(part.pw, cmort.fil, ylab = "CCF", na.action = na.omit, panel.first = grid())



