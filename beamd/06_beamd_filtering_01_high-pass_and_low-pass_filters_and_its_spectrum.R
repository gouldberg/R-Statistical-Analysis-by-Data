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
# Original time series + 1st difference (high-pass filter) and low-pass filter
#   - differencing:  example of a high-pass filter because it retains or passes the higher frequencies
#   - moving average:  low-pass filter because it passes the lower or slower frequencies
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(3,1), mar=c(3,3,1,1), mgp=c(1.6, 0.6, 0))

plot(sns1, main = "Sensor 1", type = "l")

plot(diff(sns1), main = "First Difference", type = "l")



# symmetric moving average filter
( k <- kernel("modified.daniell", 50) )
( snsf <- kernapply(sns1, k) )
plot(snsf, main = "Moving Average", type = "l")



# ------------------------------------------------------------------------------
# Spectrum analysis of the low-pass filtered series
# ------------------------------------------------------------------------------

spectrum(snsf, spans = 9, log = "no")


