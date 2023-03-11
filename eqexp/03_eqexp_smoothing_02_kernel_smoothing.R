setwd("//media//kswada//MyFiles//R//eqexp")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  eqexp
# ------------------------------------------------------------------------------

data(eqexp, package = "astsa")


head(eqexp)

str(eqexp)


attach(eqexp)


# ----------
P <- 1:1024

S <- P + 1024


x <- cbind(EQ5[P], EQ6[P], EX5[P], EX6[P], NZ[P], EQ5[S], EQ6[S], EX5[S], EX6[S], NZ[S])

x.name <- c("EQ5-P", "EQ6-P", "EX5-P", "EX6-P", "NZ-P", "EQ5-S", "EQ6-S", "EX5-S", "EX6-S", "NZ-S")

colnames(x) <- x.name



# ------------------------------------------------------------------------------
# Smoothing in time series context
# Kernel Smoothing
# ------------------------------------------------------------------------------

# ksmooth:  the kernels are scaled so that their quartiles (viewed as probability densities) are at +- 0.25 * bandwidth.
# For the standard normal distribution, the quartiles are +- 0.674.

obj_ts <- EQ5


par(mfrow=c(1,1))

plot(obj_ts, type = "l")
lines(ksmooth(time(obj_ts), obj_ts, "normal", bandwidth = 25), lwd=2, col = 4)



# ----------
gauss <- function(x){ 1/sqrt(2*pi) * exp(-(x^2)/2) }
x <- seq(from = -3, to = 3, by = 0.001)
plot(x, gauss(x), type = "l", ylim = c(-0.02, 0.45), xaxt = 'n', yaxt = 'n', ann = FALSE)
