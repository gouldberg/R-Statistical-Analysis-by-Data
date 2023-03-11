setwd("//media//kswada//MyFiles//R//squid_norway")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Squid Norway
# ------------------------------------------------------------------------------
Squid <- read.table(file = "//media//kswada//MyFiles//references//ZuurBeginnersGuideToGeneralizedAdditiveModelsWithR//SquidNorway.txt", header = TRUE)


str(Squid)



# ------------------------------------------------------------------------------
# Applying GAM using mgcv applied on the squid data
# ------------------------------------------------------------------------------
# for comparison with MCMC approach later, we standardize the covariate

Mystd <- function(x) {(x-mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)}

Squid$Lat.std <- Mystd(Squid$Lat)
Squid$ML.std  <- Mystd(Squid$ML)



# ----------
# use the standardised covariate, and usr cubic regression spline
M3 <- gam(d15N ~ Lat.std + s(ML.std, bs = "cr"), data = Squid)


par(mfrow=c(1,1), mar = c(5,5,2,2))
plot(M3, resid = TRUE, pch = 16, cex = 0.7, cex.lab = 1.5)


# -->
# The resulting smoother shows an upward length effect, which reaches a plateau.


# ----------
summary(M3)


# -->
# The amount of smoothing is presented as the effective degrees of freedom (edf). In this case the edf for ML.std is 2.57.
# An edf of 2.57 indicates a weak nonlinear effect.



# ------------------------------------------------------------------------------
# Linear spline regression with one internal knot at standardized ML = 0.8
# ------------------------------------------------------------------------------
rhs <- function(x, TH) ifelse(x >= TH, x - TH, 0)

M4  <- lm(d15N ~ ML.std  + rhs(ML.std, 0.8) + Lat.std, data = Squid)

summary(M4)


#Figure 1.4
range(Squid$ML.std)
NewData <- data.frame(ML.std = seq(from = -1.48, to = 3.03, length = 100), Lat.std = 0)
P4  <- predict(M4, newdata = NewData)

par(mar = c(5,5,3,3))
plot(x = Squid$ML.std, y = Squid$d15N, pch = 16, xlab = "Standardized ML", ylab = "d15N", cex.lab = 1.5)
abline(v = 0.83, lty = 2, lwd = 2)     
lines(x = NewData$ML.std, y = P4, lwd = 5)



# ------------------------------------------------------------------------------
# Linear spline regression with more knots
#   - internal knots are at the 25%, 50%, and 75% quartiles
# ------------------------------------------------------------------------------
probs <- seq(0, 1, length = 5)
QD    <- quantile(Squid$ML.std, probs)
QD

M5  <- lm(d15N ~ Lat.std  + ML.std  + rhs(ML.std, -0.7121410) +  rhs(ML.std, -0.1667513) + rhs(ML.std, 0.6419299), data = Squid)

summary(M5)


NewData <- data.frame(ML.std = seq(from = -1.48, to = 3.03, length = 100), Lat.std = 0)
P5  <- predict(M5, newdata = NewData)

par(mar = c(5,5,3,3))
plot(x = Squid$ML.std, y = Squid$d15N,  pch = 16,  xlab = "Standardized ML", ylab = "d15N", cex.lab = 1.5)
lines(x = NewData$ML.std, y = P5, lwd = 5)
abline(v = -0.7121410, lty = 2, lwd = 2)     
abline(v = -0.1667513, lty = 2, lwd = 2)     
abline(v = 0.6419299, lty = 2, lwd = 2) 


