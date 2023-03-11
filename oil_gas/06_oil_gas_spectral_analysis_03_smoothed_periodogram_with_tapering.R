setwd("//media//kswada//MyFiles//R//oil_gas")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Oil and Gas
# ------------------------------------------------------------------------------

data(oil, package = "astsa")
data(gas, package = "astsa")

str(oil)
str(gas)

oil
gas



# ------------------------------------------------------------------------------
# Spectral analysis:  smoothed periodogram by modified Daniell kernel
# ------------------------------------------------------------------------------

# obtain coefficients of modefied Daniell kernel
m <- 1

( ker <- kernel("modified.daniell", c(m, m)) )

plot(ker)



# ----------
# taper:  generally have a shape that enhances the center of the data relative to the extremities, such as a cosine bell of the form
#  - taper = 0.1:  tapering the upper and lower 10% of the data
graphics.off()
par(mfrow=c(2,1))

oil.smo <- astsa::mvspec(oil, kernel = ker, taper = 0.1, log = "no")

gas.smo <- astsa::mvspec(gas, kernel = ker, taper = 0.1, log = "no")



# ----------
graphics.off()
par(mfrow=c(2,1))

oil.smo <- astsa::mvspec(diff(log(oil)), kernel = ker, taper = 0.1, log = "no")

gas.smo <- astsa::mvspec(diff(log(gas)), kernel = ker, taper = 0.1, log = "no")


