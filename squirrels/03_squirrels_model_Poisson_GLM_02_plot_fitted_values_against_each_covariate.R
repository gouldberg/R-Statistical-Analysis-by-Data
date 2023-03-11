setwd("//media//kswada//MyFiles//R//squirrels")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Red Squirrels
# ------------------------------------------------------------------------------

SQ <- read.table(file = "RedSquirrels.txt", header = TRUE, dec = ".")


str(SQ)


# ----------
# remove large DBH
SQ2 <- subset(SQ, DBH < 0.6)



# ------------------------------------------------------------------------------
# Scaling covariate  (to compare with MCMC result)
# ------------------------------------------------------------------------------

SQ2$Ntrees.std      <- as.numeric(scale(SQ2$Ntrees))

SQ2$TreeHeight.std  <- as.numeric(scale(SQ2$TreeHeight))

SQ2$CanopyCover.std <- as.numeric(scale(SQ2$CanopyCover))



# ------------------------------------------------------------------------------
# Plot fitted values against each covariate
#   - Predicted number of stripped cones plotted versus one covariate.
#     In each panel one covariate varies and the other two are set to their mean value (which is 0, as they are standardized).
#   - Dotted lines show 95% confidence intervals for the mean.
# ------------------------------------------------------------------------------

par(mfrow = c(1,3), mar = c(5,5,2,2))

plot(x=SQ2$Ntrees.std , y = SQ2$SqCones,
     xlab = "Standardized number of trees",
     ylab = "Number of stripped cones",
     cex.lab = 2,
     pch = 16, type = "n")


# ----------
range(SQ2$Ntrees.std)

MyData <- data.frame(Ntrees.std = seq(-1.1, 4, length = 25),
                     TreeHeight.std  = 0,
                     CanopyCover.std = 0)
P1 <- predict(M1, newdata = MyData, type = "link", se = TRUE)
lines(x = MyData$Ntrees.std, y = exp(P1$fit), lwd = 3)
lines(x = MyData$Ntrees.std, y = exp(P1$fit + 2*P1$se.fit), lwd = 3, lty = 2)
lines(x = MyData$Ntrees.std, y = exp(P1$fit - 2*P1$se.fit), lwd = 3, lty = 2)


# ----------
plot(x=SQ2$TreeHeight.std , y = SQ2$SqCones,
     xlab = "Standardized tree height",
     ylab = "Number of stripped cones",
     cex.lab = 2,
     pch = 16, type = "n")

range(SQ2$TreeHeight.std)
MyData <- data.frame(Ntrees.std = 0,
                     TreeHeight.std  = seq(-3, 1.5, length = 25),
                     CanopyCover.std = 0)
P1 <- predict(M1, newdata = MyData, type = "link", se = TRUE)
lines(x = MyData$TreeHeight.std, y = exp(P1$fit), lwd = 3)
lines(x = MyData$TreeHeight.std, y = exp(P1$fit + 2*P1$se.fit), lwd = 3, lty = 2)
lines(x = MyData$TreeHeight.std, y = exp(P1$fit - 2*P1$se.fit), lwd = 3, lty = 2)


# ----------
plot(x=SQ2$CanopyCover.std , y = SQ2$SqCones,
     xlab = "Standardized canopy cover",
     ylab = "Number of stripped cones",
     cex.lab = 2,
     pch = 16, type = "n")

range(SQ2$CanopyCover.std)
MyData <- data.frame(Ntrees.std = 0,
                     TreeHeight.std  = 0,
                     CanopyCover.std = seq(-4, 1, length = 25))
P1 <- predict(M1, newdata = MyData, type = "link", se = TRUE)
lines(x = MyData$CanopyCover.std, y = exp(P1$fit), lwd = 3)
lines(x = MyData$CanopyCover.std, y = exp(P1$fit + 2*P1$se.fit), lwd = 3, lty = 2)
lines(x = MyData$CanopyCover.std, y = exp(P1$fit - 2*P1$se.fit), lwd = 3, lty = 2)
