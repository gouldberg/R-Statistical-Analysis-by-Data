setwd("//media//kswada//MyFiles//R//birth")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  birth
# ------------------------------------------------------------------------------

data(birth, package = "astsa")

str(birth)

head(birth)



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
par(mfrow=c(1,1))

bir.smo <- astsa::mvspec(birth, kernel = ker, taper = 0.1, log = "no")



# ----------
graphics.off()
par(mfrow=c(1,1))

bir.smo <- astsa::mvspec(diff(log(birth)), kernel = ker, taper = 0.1, log = "no")
