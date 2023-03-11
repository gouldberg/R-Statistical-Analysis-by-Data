setwd("//media//kswada//MyFiles//R//soi_rec")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  soi
# data:  rec
# ------------------------------------------------------------------------------

data(soi, package = "astsa")

data(rec, package = "astsa")




# ------------------------------------------------------------------------------
# Spectral analysis:  smoothed periodogram by modified Daniell kernel
# ------------------------------------------------------------------------------

# We use m = 3 for both times
# This yields Lh = 1 / weights^2 = 1 / { h(-m)^2 + h(-m+1)^2 ...+ h(m-1)^2 + h(m)^2 } = 9.232, close to the value of L = 9
# The bandwidth = 9.232 / 480 = 0.019
# modified degrees of freedom is 2 * Lh * 453 / 480 = 17.43

# obtain coefficients of modefied Daniell kernel

( ker <- kernel("modified.daniell", c(3,3)) )

plot(ker)




# ----------
# Lh = 1 / sum(hk^2)  k = -6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6  --> 9.24, which is close to the value of L = 9
1 / sum(ker$coef[c(6,5,4,3,2,1,2,3,4,5,6)]^2)



graphics.off()
par(mfrow=c(2,1))

soi.smo <- astsa::mvspec(soi, kernel = ker, log = "no")
abline(v = c(0.25, 1), lty = 2)

rec.smo <- astsa::mvspec(rec, kernel = ker, log = "no")
abline(v = c(0.25, 1), lty = 2)





# ------------------------------------------------------------------------------
# Tapered spectrum and non-tapered spectrum:  tapering effect and leakage
#   - For example, a large dynamic range for the values in the spectrum introduces spectra in contiguous frequency intervals
#     several orders of magnitude greater than the value in the interval of interest.
#     This effect is called "leakage"
# ------------------------------------------------------------------------------

# no taper
s0 <- astsa::mvspec(soi, kernel = ker, plot=FALSE)

# 0.1 taper
s51 <- astsa::mvspec(soi, kernel = ker, taper = 0.1, plot=FALSE)

# full taper
s55 <- astsa::mvspec(soi, kernel = ker, taper = 0.5, plot=FALSE)



# ----------
par(mfrow=c(1,1))

plot(s55$freq, s55$spec, log="y", type = "l", ylab = "spectrum", xlab = "frequency", col = "blue", lwd = 2)
lines(s51$freq, s51$spec, lty = 1, col = "black")
lines(s0$freq, s0$spec, lty = 2)




# -->
# Notice that the tapered spectrum does a better job in separating the yearly cycle (omega = 1) and
# the El Nino cycle (omega = 1/4)





############################################
# full taper = 0.5

n5 <- length(soi)

t_hat5 <- floor((n5 + 1) / 2)

ax5 <- ( 1:n5 - t_hat5 ) / n5



# ----------
# a full cosine bell taper by Blackman and Tukey

ht5 <- 0.5 * {1 + cos(2 * pi * ax5)}


graphics.off()
par(mfrow = c(2,1))
plot(ht5 ~ ax5, type = "l", ylim = c(0, 1.0))




# ----------
# replaced time series

soi_rep5 <- soi * ht5

par(mfrow = c(2,2))
plot(soi, main = "original soi", ylim = c(-1, 1))
plot(soi_rep5, type = "l", main = "replaced soi taper = 0.5", col = "blue", lwd = 2, ylim = c(-1, 1))



graphics.off()
par(mfrow = c(2,2))
astsa::mvspec(soi, kernel = ker, log = "yes")
astsa::mvspec(soi_rep5, kernel = ker, log = "yes")


