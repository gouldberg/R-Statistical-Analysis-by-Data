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
# fit linear spline regression:  1 knot
#   - Linear spline regression means that the x-axis (depth gradient) is separated into segments and a bivariate linear regression model
#     is fitted on the data of each segment.
# ------------------------------------------------------------------------------

par(mfrow=c(1,2))
plot(x = BL$DepthOriginal, y = BL$Sources, xlab = "Depth", ylab ="Sources",  cex.lab = 1.5)
plot(x = BL$Depth, y = BL$Sources, xlab = "Scaled depth", ylab ="Sources",  cex.lab = 1.5, cex = 0.7,  pch = 16, col = grey(0.5))


# -->
# scrutiny of the scatterplot ives the impression that there is a change in relationship between sources and depth at around 500m,
# which corresponds to the value 0.2 at scaled depth.


# ----------
rhs <- function(x, TH) ifelse(x >= TH, x-TH,0)
M3 <- lm(Sources ~ Depth  + rhs(Depth, 0.2), data = BL)

head(model.matrix(M3))

print(summary(M3), digits = 2, signif.stars = FALSE)



# ----------
P3 <- predict(M3, newdata = MD)

par(mfrow=c(1,1))
plot(x = BL$Depth, y = BL$Sources, xlab = "Scaled depth", ylab ="Sources",  cex.lab = 1.5, cex = 0.7,  pch = 16, col = grey(0.5))
lines(x = MD$Depth, y = P3, lwd = 5)



# ----------
# Compare models
AIC(M1, M2, M3)


# AIC of model M3 is the lowest; hence it is deemed the best of these 3 models.



# ------------------------------------------------------------------------------
# fit linear spline regression:  use quantiles to decide on Knots
# ------------------------------------------------------------------------------
probs <- seq(0, 1, length = 11)
QD    <- quantile(BL$Depth, probs)

QD


# use 9 inner knots and upper and lower limits of data, total of 11 knots
M4 <- lm(Sources ~ Depth  +
           rhs(Depth, 0.159) +
           rhs(Depth, 0.220) +
           rhs(Depth, 0.281) +
           rhs(Depth, 0.344) +
           rhs(Depth, 0.410) +
           rhs(Depth, 0.490) +
           rhs(Depth, 0.567) +
           rhs(Depth, 0.664) +
           rhs(Depth, 0.787),
         data = BL)


head(model.matrix(M4))

print(summary(M4), digits = 2, signif.stars = FALSE)



# ----------
P4 <- predict(M4, newdata = MD)
plot(x = BL$Depth, y = BL$Sources, xlab = "Scaled depth", ylab = "Sources",  cex.lab = 1.5, cex = 0.7,  pch = 16,  col = grey(0.5))
lines(x = MD$Depth, y = P4, lwd = 5)



# ----------
# Compare models
AIC(M1, M2, M3, M4)


# --> M4 uses more paramters, but AIC of M4 model is the lowest

