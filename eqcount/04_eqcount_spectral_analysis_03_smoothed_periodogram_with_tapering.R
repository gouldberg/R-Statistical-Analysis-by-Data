setwd("//media//kswada//MyFiles//R//eqcount")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  eqcount
# ------------------------------------------------------------------------------

data(EQcount, package = "astsa")


str(EQcount)


EQcount



# ------------------------------------------------------------------------------
# Spectral analysis:  smoothed periodogram by modified Daniell kernel
# ------------------------------------------------------------------------------

# obtain coefficients of modefied Daniell kernel
m <- 1

( ker <- kernel("modified.daniell", c(m,m)) )

plot(ker)



# ----------
# taper:  generally have a shape that enhances the center of the data relative to the extremities, such as a cosine bell of the form
#  - taper = 0.1:  tapering the upper and lower 10% of the data
graphics.off()
par(mfrow=c(1,1))

eqc.smo <- astsa::mvspec(EQcount, kernel = ker, taper = 0.1, log = "no")



# -->
# There have been many attempts at dealing with the problem of smoothing the periodogram in a automatic way;
# an early reference is Wahba.
# Consequently, it is perhaps better to perform automatic adaptive smoothing for estimating the spectrum.
# We refer interested readers to Fan and Kreutzberger and the numerous references within.



