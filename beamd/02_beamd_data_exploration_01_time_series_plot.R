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
# data exploration:  multivariate time series plot
# ------------------------------------------------------------------------------

graphics.off()
par(cex = 1.4, mar = c(4,4,2,1), lwd = 1.6, mfrow=c(1,1))
MTSplot(beamd)



# -->
# These signals appear at slightly different times.
# Because of the way signals propagate, a plane wave signal of this kind, from a given source,
# traveling at a given velocity, will arrive at elements in the array at predictable time delays.
# Here, the delays were approximated by computing the cross-correlation between elements and simply reading off the time delay
# corresponding to the maximum.




# ----------
par(mfrow=c(3,1))
plot(diff(beamd$sensor1), ylab = "", xlab = "", main = "Sensor1", type = "l")
plot(diff(beamd$sensor2), ylab = "", xlab = "", main = "Sensor2", type = "l")
plot(diff(beamd$sensor3), ylab = "", xlab = "", main = "Sensor3", type = "l")



# ----------
apply(beamd, MARGIN = 2, FUN = forecast::ndiffs)
