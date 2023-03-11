setwd("//media//kswada//MyFiles//R//heger_priede")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Heger Priede
# ------------------------------------------------------------------------------
BL <- read.table(file = "//media//kswada//MyFiles//references//ZuurBeginnersGuideToGeneralizedAdditiveModelsWithR//HegerPriede.txt", header = TRUE)


str(BL)

names(BL)



# ------------------------------------------------------------------------------
# relationship between Sources and Depth
# ------------------------------------------------------------------------------

# The data were sampled at 14 tstations, but in the beginning our analysis we will ignore any potential station effects.

# scatterplot of depth in metres versus bioluminescence counts per cubic meter (called "Sources" in the data file)

# Scale Depth
BL$DepthOriginal <- BL$Depth
BL$Depth <- BL$Depth/max(BL$Depth)

par(mfrow=c(1,2))
plot(x = BL$DepthOriginal, y = BL$Sources, xlab = "Depth", ylab ="Sources",  cex.lab = 1.5)
plot(x = BL$Depth, y = BL$Sources, xlab = "Scaled depth", ylab ="Sources",  cex.lab = 1.5, cex = 0.7,  pch = 16, col = grey(0.5))



# ------------------------------------------------------------------------------
# fit quadratic spline regression
#   - We tried linear spline regression, but this technique resulted in a curve with peaky connections at the knots
# ------------------------------------------------------------------------------

rhs2 <- function(x, TH) ifelse(x >= TH, (x-TH)^2,0)

M5 <- lm(Sources ~ Depth  + I(Depth^2) +
           rhs2(Depth, 0.159) +
           rhs2(Depth, 0.220) +
           rhs2(Depth, 0.281) +
           rhs2(Depth, 0.344) +
           rhs2(Depth, 0.410) +
           rhs2(Depth, 0.490) +
           rhs2(Depth, 0.567) +
           rhs2(Depth, 0.664) +
           rhs2(Depth, 0.787),
         data = BL)



print(summary(M4), digits = 2, signif.stars = FALSE)
print(summary(M5), digits = 2, signif.stars = FALSE)



# ----------
P5 <- predict(M5, newdata = MD)

par(mfrow=c(1,1))
plot(x = BL$Depth, y = BL$Sources, xlab = "Scaled depth", ylab ="Sources",  cex.lab = 1.5, cex = 0.7,  pch = 16, col = grey(0.8))
lines(x = MD$Depth, y = P4, lwd = 1, col = "black")
lines(x = MD$Depth, y = P5, lwd = 3, col = "red")


# --> 
# compared to linear regression spline, the connection at the knots is indeed considerably smoother.



# ----------
# compare models
AIC(M1, M2, M3, M4, M5)

# but M5 has one more parameter than M4, so AIC is not smaller than M4


# Also note that none of the rhs2 temrs are significant.
# This is most likely caused by the high collinearity among these terms.
# Software routines such as the function gam in the package mgcv apply more advanced smoothers, and some of these incorporate
# special procedures to avoid the high collinearity.



# ------------------------------------------------------------------------------
# fit cubic regression splines
# ------------------------------------------------------------------------------

# To allow for even smoother connections at the knots, cubic terms can be used instead of quadratic terms.
# The basis of this model contains one more term than in the quadratic regression spline.
plot(x = BL$Depth, y = BL$Sources, xlab = "Scaled depth", ylab ="Sources",  cex.lab = 1.5, cex = 0.7,  pch = 16,  col = grey(0.5))

for (i in 1:11){ abline(v = QD[i], lty = 2) }

for (i in 2:11){
  M2 <- lm(Sources ~ Depth + I(Depth^2) + I(Depth^3), data = BL, subset = BL$Depth > QD[i-1] & BL$Depth <= QD[i])
  MD <- data.frame(Depth = seq(QD[i-1], QD[i], length =100))
  P2 <- predict(M2, newdata = MD)
  lines(MD$Depth, P2, lwd = 5)
}



# ----------
# Wood (2006) and Zuur et al. (2009a), along with many others, used a cubic regression spline with a different basis from
# that of the cubic regression spline presented here.
rk <- function(x,z) ((z-0.5)^2-1/12)*((x-0.5)^2-1/12)/4 -((abs(x-z)-0.5)^4 -0.5*(abs(x-z)-0.5)^2 +7/240)/24

spl.X <- function(x,xk){
  q <- length(xk) + 2  # including outer knots (first and last)
  n <- length(x)
  X <- matrix(1, n, q)
  X[,2] <- x
  X[,3:q] <- outer(x, xk, FUN=rk)
  X
}



# the covariates for the cubic regression spline
X  <- spl.X(BL$Depth, QD[2:10])
print(head(X), digits=2)



# ----------
# We must ensure that no intercept is used by the function lm, as X already contains a column with 1 used for the intercept.
M6 <- lm(Sources ~ X - 1, data = BL)
summary(M6)



# ----------
plot(x = BL$Depth, y = BL$Sources, xlab = "Scaled depth", ylab = "Sources",  cex.lab = 1.5, cex = 0.7,  pch = 16,  col = grey(0.8))

for (i in 1:11){ abline(v = QD[i], lty = 2) }
MD <- data.frame(Depth = seq(QD[1], QD[11], length =100))
Xp <- spl.X(MD$Depth, QD[2:10])
lines(MD$Depth, Xp %*% coef(M6), lwd = 5, col = "red")
lines(x = MD$Depth, y = P4, lwd = 1, col = "blue")
lines(x = MD$Depth, y = P5, lwd = 1, col = "orange")


# -->
# there is little detectable difference between smoothers obtained with different bases.


# -----------
AIC(M4, M5, M6)


