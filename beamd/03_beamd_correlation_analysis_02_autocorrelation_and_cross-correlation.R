setwd("//media//kswada//MyFiles//R//beamd")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  beamd
# ------------------------------------------------------------------------------

data(beamd, package = "astsa")

str(beamd)

head(beamd)



# ------------------------------------------------------------------------------
# Correlation analysis:  autocorrelation and cross-correlation
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow = c(3, 1))

acf(beamd$sensor1, lag = 1000, main = "Sensor 1")

acf(beamd$sensor2, lag = 1000, main = "Sensor 2")

acf(beamd$sensor3, lag = 1000, main = "Sensor 3")



# ----------
sns1 <- beamd$sensor1
sns2 <- beamd$sensor2
sns3 <- beamd$sensor3

ccf(sns1, sns2, lag = 100, main = "Sensor1 vs. Sensor2", ylab = "CCF")
ccf(sns1, sns2, lag = 1000, main = "Sensor1 vs. Sensor2", ylab = "CCF")

ccf(sns2, sns3, lag = 100, main = "Sensor2 vs. Sensor3", ylab = "CCF")
ccf(sns2, sns3, lag = 1000, main = "Sensor2 vs. Sensor3", ylab = "CCF")

ccf(sns1, sns3, lag = 100, main = "Sensor1 vs. Sensor3", ylab = "CCF")
ccf(sns1, sns3, lag = 1000, main = "Sensor1 vs. Sensor3", ylab = "CCF")



# ----------
graphics.off()
par(mfrow = c(3, 1))
ccf(sns1, sns2, lag = 200, main = "Sensor1 vs. Sensor2", ylab = "CCF")
ccf(sns2, sns3, lag = 200, main = "Sensor2 vs. Sensor3", ylab = "CCF")
ccf(sns1, sns3, lag = 200, main = "Sensor1 vs. Sensor3", ylab = "CCF")



# ----------
# All in one panel
acf(cbind(sns1, sns2, sns3), lag.max = 500)


