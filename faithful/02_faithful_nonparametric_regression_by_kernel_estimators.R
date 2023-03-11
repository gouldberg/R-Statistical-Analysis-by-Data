setwd("//media//kswada//MyFiles//R//faithful")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  faithful
# ------------------------------------------------------------------------------

data(faithful, package="faraway")

str(faithful)


# ----------
# for comparison
# exa:  true function is f(x) = sin^3(e * pi * x^3)
# exb:  constant zero

data(exa);  data(exb)

str(exa)

str(exb)



# ------------------------------------------------------------------------------
# plot data
# ------------------------------------------------------------------------------

par(mfrow=c(1,3))

plot(y ~ x, exa, main = "Example A", pch = ".")
lines(m ~ x, exa)

plot(y ~ x, exb, main = "Example B", pch = ".")
lines(m ~ x, exb)

plot(waiting ~ eruptions, faithful, main = "Old Faithful", pch = ".")



# ------------------------------------------------------------------------------
# Non-parametric regresssion by Nadaraya-Watson estimator
#  - Yi = f(xi) + e
#  - f(xi) = sum(wj * Yj) / sum(wj)
#  - wj = K((x - xj) / lambda) / lambda
#  - K(x) = 3/4(1 - x^2) (if |x| < 1), 0 (otherwise):  Epanechnikov Kernel  (for some optimal choice under some standard assumptions)
#
#
#  - The simplest form of f(xi) is just a moving average estimator, but if the xs are apaced very unevenly, the moving average estimator can give poor results.
#    This problem is somewhat ameliorated by the Nadaraya-Watson estimator
# ------------------------------------------------------------------------------

# set bandwidth
lambda <- c(0.1, 0.2, 0.5, 0.8, 1, 2)


par(mfrow=c(2,3))

for(i in lambda){
  plot(waiting ~ eruptions, faithful, main = paste0("bandwidht = ", i), col = gray(0.75))
  lines(ksmooth(faithful$eruptions, faithful$waiting, "normal", i))
}


# -->
# lambda = 0.5 is best choice of the three from subjective judgement.



# ------------------------------------------------------------------------------
# Cross-validagted choice of bandwidth
#
#  - In cases where the fitted curve will be used to make numerical predictions of future values, the choice of the amount of smoothing has an
#    immediate effect on the outcome.
#    Even here subjective methods may be used.  If this method of selecting the amount of smoothing seems disturbingly subjective,
#    we should also understand that the selection of a family of parametric models for the same data would also involve a great deal of 
#    subjective choice although this is often not eplicitly recognized.
#  - Cross validation to choose optimal bandwidth that minimizes the GCV-errors often work well, but sometimes produce estimates that are 
#    clearly at odds with the amount of smoothing that contextual knowledge would suggest.
#    So recommended is using CV as a starting point for a possible interactive exploration of the appropriate amount of smoothing.
# ------------------------------------------------------------------------------

# sm package uses a Gaussian kernel where the smoothing parameter is the standard deviation of the kernel.
library(sm)


# ----------
par(mfrow=c(1,1))
hm <- hcv(faithful$eruptions, faithful$waiting, display = "liines")

summary(hm)


# -->
# The minimum occurs at a value of 0.4243



# ----------
# regression
with(faithful, sm.regression(eruptions, waiting, h = h.select(eruptions, waiting)))
  
sm.regression(faithful$eruptions, faithful$waiting, h = hm, xlab = "eruptions", ylab = "waiting")



# ----------
# for exa data

hm <- hcv(exa$x, exa$y, display = "lines")
summary(hm)

sm.regression(exa$x, exa$y, h = hm, xlab = "x", ylab = "y")



# ----------
# for exb data
# We find that the CV choice is at the lower boundary of suggested bandwidths.
# However, bandwidths this small represent windows that include only a single point, making cross-validation impractical.
# This small bandwidth gices us a dramatic undersmooth because the regression exactly fits the data
hm <- hcv(exb$x, exb$y, display = "lines")
summary(hm)

sm.regression(exb$x, exb$y, h = hm, xlab = "x", ylab = "y")

sm.regression(exb$x, exb$y, h = 0.005, xlab = "x", ylab = "y")


