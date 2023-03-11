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
# Smoothing in time series context
# Kernel Smoothing
# ------------------------------------------------------------------------------

# ksmooth:  the kernels are scaled so that their quartiles (viewed as probability densities) are at +- 0.25 * bandwidth.
# For the standard normal distribution, the quartiles are +- 0.674.

# we are smoothing over time, which is of the form t / 52 for the so2 time series
# b = 1:  corresponding to approximately smoothing a little over one year

par(mfrow=c(1,1))

plot(so2)

lines(ksmooth(time(so2), so2, "normal", bandwidth = 1), lwd=2, col = 4)



# ----------
gauss <- function(x){ 1/sqrt(2*pi) * exp(-(x^2)/2) }

x <- seq(from = -3, to = 3, by = 0.001)

plot(x, gauss(x), type = "l", ylim = c(-0.02, 0.45), xaxt = 'n', yaxt = 'n', ann = FALSE)
