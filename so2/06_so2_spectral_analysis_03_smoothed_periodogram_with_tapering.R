setwd("//media//kswada//MyFiles//R//so2")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  so2
# ------------------------------------------------------------------------------

data(so2, package = "astsa")

str(so2)

so2



# ------------------------------------------------------------------------------
# Spectral analysis:  smoothed periodogram by modified Daniell kernel
# ------------------------------------------------------------------------------

m <- 3

( ker <- kernel("modified.daniell", c(m, m)) )

plot(ker)




# ----------
# taper:  generally have a shape that enhances the center of the data relative to the extremities, such as a cosine bell of the form
#  - taper = 0.1:  tapering the upper and lower 10% of the data
graphics.off()

par(mfrow=c(2,1))

so2.smo <- astsa::mvspec(so2, kernel = ker, taper = 0.1, log = "no")

so2.smo <- astsa::mvspec(diff(so2), kernel = ker, taper = 0.1, log = "no")

