setwd("//media//kswada//MyFiles//R//bnrf1ebv")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  bnrf1ebv
# ------------------------------------------------------------------------------

data(bnrf1ebv, package = "astsa")

bnrf1ebv



# ------------------------------------------------------------------------------
# Spectral analysis:  smoothed periodogram by modified Daniell kernel
# ------------------------------------------------------------------------------

m <- 11

# obtain coefficients of modefied Daniell kernel
( ker <- kernel("modified.daniell", c(11, 11)) )

plot(ker)



# ----------
# taper:  generally have a shape that enhances the center of the data relative to the extremities, such as a cosine bell of the form
#  - taper = 0.1:  tapering the upper and lower 10% of the data
graphics.off()
par(mfrow=c(2,1))

bnr.smo <- astsa::mvspec(bnrf1ebv, kernel = ker, taper = 0.1, log = "no")

bnr.smo <- astsa::mvspec(rec, kernel = ker, taper = 0.1, log = "yes")



# ------------------------------------------------------------------------------
# bandwidth and degrees of freedom
# ------------------------------------------------------------------------------

bnr.smo$bandwidth


bnr.smo$df

