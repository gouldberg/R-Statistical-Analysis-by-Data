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
# plot estimated states on time series
# ------------------------------------------------------------------------------

layout(matrix(c(1,2,1,3), 2))


# Earthquake count data and estimated states
par(mar = c(3,3,1,1), mgp = c(1.6, 0.6, 0))

plot(EQcount, main = "", ylab = "EQcount", type = "h", col = gray(0.7))

text(EQcount, col = 6 * posterior(fm)[,1] - 2, labels = posterior(fm)[,1], cex = 0.9)



# ----------
# Smoothing probabilities of state 2
plot(ts(posterior(fm)[,3], start = 1900), ylab  = expression(hat(pi)[~2] * '(t|n)'))

abline(h = 0.5, lty = 2)



# ----------
# Histogram of the data with the two estimated Poisson densities superimposed
xvals <- seq(1, 45)
u1 <- pi1 * dpois(xvals, lams[1])
u2 <- pi2 * dpois(xvals, lams[2])


hist(EQcount, breaks = 30, prob = TRUE, main = "")
lines(xvals, u1, col = 4)
lines(xvals, u2, col = 2)



