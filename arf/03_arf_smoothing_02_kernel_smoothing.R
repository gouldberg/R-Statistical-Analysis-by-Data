setwd("//media//kswada//MyFiles//R//arf")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  arf
# ------------------------------------------------------------------------------

data(arf, package = "astsa")

str(arf)

head(arf)



# ------------------------------------------------------------------------------
# Smoothing in time series context
# Kernel Smoothing
# ------------------------------------------------------------------------------

# ksmooth:  the kernels are scaled so that their quartiles (viewed as probability densities) are at +- 0.25 * bandwidth.
# For the standard normal distribution, the quartiles are +- 0.674.

par(mfrow=c(1,1))

plot(arf)
lines(ksmooth(time(arf), arf, "normal", bandwidth = 50), lwd=2, col = 4)


# ----------
gauss <- function(x){ 1/sqrt(2*pi) * exp(-(x^2)/2) }
x <- seq(from = -3, to = 3, by = 0.001)
plot(x, gauss(x), type = "l", ylim = c(-0.02, 0.45), xaxt = 'n', yaxt = 'n', ann = FALSE)
