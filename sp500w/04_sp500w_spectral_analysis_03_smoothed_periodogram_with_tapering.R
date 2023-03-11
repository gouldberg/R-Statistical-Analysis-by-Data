setwd("//media//kswada//MyFiles//R//sp500w")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  sp500w
#   - weekly S&P 500 returns from to Jan. 2003 to Sept. 2012
# ------------------------------------------------------------------------------

data(sp500w, package = "astsa")


# this is xts object
str(sp500w)



# ------------------------------------------------------------------------------
# Spectral analysis:  smoothed periodogram by modified Daniell kernel
# ------------------------------------------------------------------------------

# obtain coefficients of modefied Daniell kernel
L1 <- 3
L2 <- 26*1+1
L3 <- 26*2+1
L4 <- 26*4+1
L5 <- 26*8+1

m1 <- (L1 - 1) / 2
m2 <- (L2 - 1) / 2
m3 <- (L3 - 1) / 2
m4 <- (L4 - 1) / 2
m5 <- (L5 - 1) / 2


( ker1 <- kernel("modified.daniell", c(m1, m1)) )
( ker2 <- kernel("modified.daniell", c(m2, m2)) )
( ker3 <- kernel("modified.daniell", c(m3, m3)) )
( ker4 <- kernel("modified.daniell", c(m4, m4)) )
( ker5 <- kernel("modified.daniell", c(m5, m5)) )


plot(ker5)



# ----------
# taper:  generally have a shape that enhances the center of the data relative to the extremities, such as a cosine bell of the form
#  - taper = 0.1:  tapering the upper and lower 10% of the data
graphics.off()
par(mfrow=c(2,3))

astsa::mvspec(sp500w, kernel = ker1, taper = 0.1, log = "no")
astsa::mvspec(sp500w, kernel = ker2, taper = 0.1, log = "no")
astsa::mvspec(sp500w, kernel = ker3, taper = 0.1, log = "no")
astsa::mvspec(sp500w, kernel = ker4, taper = 0.1, log = "no")
astsa::mvspec(sp500w, kernel = ker5, taper = 0.1, log = "no")



# -->
# There have been many attempts at dealing with the problem of smoothing the periodogram in a automatic way;
# an early reference is Wahba.
# Consequently, it is perhaps better to perform automatic adaptive smoothing for estimating the spectrum.
# We refer interested readers to Fan and Kreutzberger and the numerous references within.



