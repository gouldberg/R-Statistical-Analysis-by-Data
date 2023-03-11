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
# Estimating Signal via Optimal Filters
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(3,1), mar=c(3,3,1,1), mgp=c(1.6, 0.6, 0))

plot(sns1, main = "Sensor 1", type = "l")

plot(diff(sns1), main = "First Difference", type = "l")



# symmetric moving average filter
( k <- kernel("modified.daniell", 50) )
( snsf <- kernapply(sns1, k) )
plot(snsf, main = "Moving Average", type = "l")



# -->
# Moving average filter shows a good deal of higher frequency chatter riding on the smoothed version,
# which has been introduced by the higher frequencies that leak through in the squared frequency response.




# ----------
# Performs signal extraction and optimal filtering
# L:  degree of smoothing
# M:  number of terms used in the lagged regression approximation
# max.freq:  truncation frequency, which must be larger than 1/M
astsa::SigExtract(sns1, L = 9, M = 200, max.freq = 0.2)


# -->
# Smooth extracted signals conveys the essence of the signal ..?


