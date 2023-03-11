setwd("//media//kswada//MyFiles//R//globtemp")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  globtemp
# ------------------------------------------------------------------------------

data(globtemp, package = "astsa")


str(globtemp)




# ------------------------------------------------------------------------------
# Filtering and Smoothing:  apply a priori parameter
# ------------------------------------------------------------------------------

# filter and smooth  (Ksmooth0 does both)
# Ksmooth0:  time invariant model without inputs


# Ksmooth0 calls Kfilter0 for the filtering part.
# These scripts use a Cholesky-type decomposition of Q and R;  they are denoted by cQ and cR


# A: time-invariant observation matrix
# mu0: initial state mean vector
# Sigma0:  initial state covariance matrix
# Phi:  state transition matrix
# cQ:  Cholesky-type decomposition of state error covariance matrix Q
# cR:  Cholesky-type decomposition of observation error covariance matrix R


num <- length(globtemp)


ks <- astsa::Ksmooth0(num, y = globtemp, A = 1, mu0 = globtemp[1], Sigma0 = 1, Phi = 1, cQ = 1, cR = 1)



# letters p, f, s denote prediction, filter, and smooth, respectively

names(ks)




# ----------
graphics.off()

par(mfrow = c(1, 1))

Time <- 1:num


plot(Time, globtemp, main = "Precdict, Filter, Smooth", ylim = c(-0.6, 1))
lines(ks$xp, col = "blue", lwd = 2)
# lines(ks$xp + 2 * sqrt(ks$Pp), lty = 2, col = 4)
# lines(ks$xp - 2 * sqrt(ks$Pp), lty = 2, col = 4)

lines(ks$xf, col = "red", lwd = 2, lty = 2)
# lines(ks$xf + 2 * sqrt(ks$Pf), lty = 2, col = 4)
# lines(ks$xf - 2 * sqrt(ks$Pf), lty = 2, col = 4)

lines(ks$xs, col = 3, lwd = 1, lty = 2)
# lines(ks$xs + 2 * sqrt(ks$Ps), lty = 2, col = 4)
# lines(ks$xs - 2 * sqrt(ks$Ps), lty = 2, col = 4)




# -->
# Note that one-step-aheaad prediction is more uncertain than the corresponding filtered value,
# which, in turn, is more uncertain than the corresponding smoother value
# Also, in each case, the error variances stabilize quickly



# ----------
# Initial value info.

ks$x0n

sqrt(ks$P0n)

