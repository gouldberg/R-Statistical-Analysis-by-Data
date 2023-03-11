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
# fit a linear regression
# ------------------------------------------------------------------------------
M1 <- lm(Sources ~ Depth, data = BL)

print(summary(M1), digits = 2, signif.stars = FALSE)


par(mfrow=c(1,1))
plot(x = BL$Depth, y = BL$Sources, xlab = "Scaled depth", ylab ="Sources",  cex.lab = 1.5, cex = 0.7,  pch = 16, col = grey(0.5))
abline(M1, lwd = 5)


# -->
# The model explains 48% of the variation in the density data, and the regression parameter for depth is highly significant.


# ----------
# There is a clear violation of homogeneity and also a patten of clustered residuals 
E1 <- rstandard(M1)
F1 <- fitted(M1)
par(mfrow=c(1,2), mar = c(5,5,2,2))
plot(x = F1, y = E1, xlab = "Fitted values",  ylab = "Residuals",  cex.lab = 1.5)
abline(h = 0, v = 0)
plot(x = BL$Depth, y = E1,  xlab = "Depth",  ylab = "Residuals",  cex.lab = 1.5)
abline(h = 0)



# ------------------------------------------------------------------------------
# fit polynomial regression model
# ------------------------------------------------------------------------------
# cubic polynomicla regression

# M2 <- lm(Sources ~ Depth + I(Depth^2) + I(Depth^3), data = BL)
M2 <- lm(Sources ~ poly(Depth, 3), data = BL)

print(summary(M2), digits = 2, signif.stars = FALSE)


MD <- data.frame(Depth = seq(0.1, 1, length = 100))
par(mfrow=c(1,1), mar = c(5,5,2,2))
plot(x = BL$Depth, y = BL$Sources, xlab = "Scaled depth", ylab ="Sources",  cex.lab = 1.5, cex = 0.7,  pch = 16, col = grey(0.5))
P2 <- predict(M2, newdata = MD)
lines(x = MD$Depth, y = P2, lwd = 5)


# The fit of the cubic polynomial is better than that of the bivariate linear regression model, 
# but there is still some misfit, especially at the right end of the gradient.
# This is a common phenomenon with cubic polynomials; they may provide a good fit for short gradients, but for longer gradients
# they tend to misfit the data.



