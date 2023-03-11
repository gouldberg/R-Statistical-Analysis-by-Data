setwd("//media//kswada//MyFiles//R//soi_rec")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  soi
# data; rec
# ------------------------------------------------------------------------------

data(soi, package = "astsa")

data(rec, package = "astsa")




# ------------------------------------------------------------------------------
# Correlation analysis:  sample autocorrelation function (ACF) of SOI for small lags
# ------------------------------------------------------------------------------

par(mfrow = c(1,1))

acf(soi, lag.max = 6, plot=TRUE)



# -->
# 1 lag = 12 months  -->  0.5 lags = 6 months



( r <- round(acf(soi, lag.max = 6, plot=FALSE)$acf[-1], 3) )


# -->
# rho(1) = 0.604,  rho(6) = -0.187




# ------------------------------------------------------------------------------
# data exploration:  scatterplot of lagged time series
# ------------------------------------------------------------------------------

# lag operation
head(soi, 10)

# the values are one month delay
head(stats::lag(soi, -1))



# ----------
# stat::lag operation

soi[1:10]

stats::lag(soi, -1)[1:10]


soi[2:11]

stats::lag(soi, -1)[2:11]




# ----------
par(mfrow=c(1,2))

plot(soi, stats::lag(soi, -1), xlim = c(-1, 1), ylim = c(-1, 1))
legend("topleft", legend = r[1])


plot(soi, stats::lag(soi, -6), xlim = c(-1, 1), ylim = c(-1, 1))
legend("topleft", legend = r[6])




# -->
# estimated correlation is displayed in the box




# ------------------------------------------------------------------------------
# Correlation analysis:  sample autocorrelation function (ACF) of SOI for large lags range
# ------------------------------------------------------------------------------


par(mfrow=c(1,1))

forecast::Acf(soi, lag.max = 48 * 3, main = "Southern Oscilllation Index")




# -->
# the autocorrelation is cyclic ...


