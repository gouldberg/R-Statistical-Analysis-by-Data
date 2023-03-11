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
# Model Negative Binomial GLM
# ------------------------------------------------------------------------------
library(MASS)

M2 <- glm.nb(SqCones ~ Ntrees.std + TreeHeight.std + CanopyCover.std, data = SQ2)




# ----------
# Assess overdispersion
# "+1" is due to the parameter k (var(SqCones) = mu + mu^2 / k)

E2 <- resid(M2, type = "pearson")

p <- length(coef(M2)) + 1

sum(E2^2) / (nrow(SQ2) - p)


# -->
# There is a small amount of under-dispersion



# ----------
summary(M2)


# -->
# theta is the k from the variance function and is estimated as 1.08.



# ----------
( M2$null.deviance - M2$deviance ) / M2$null.deviance


# -->
# The model explains 23.5% and is smilar to that obtained by Poisson GLM ...



# ------------------------------------------------------------------------------
# Model Negative Binomial GLM:  Model selection by likelihood ratio test
# ------------------------------------------------------------------------------

drop1(M2, test = "Chi")


# -->
# The Z-statistic gives a p-value of 0.3351 for Tree Height, whereas the likelihood ratio test gives a p-value of 0.3297.
# Dropping TreeHeight.std gives a model with a deviance of 60.02

# NOTE THAT !!! the drop1 function uses the parameter k from the full model in the sub-model,
# so we can not compare other model by anova() !!!



# ----------
# Dropping TreeHeight.std
M2A <- glm.nb(SqCones ~ Ntrees.std + CanopyCover.std, data = SQ2)

anova(M2, M2A, test = "Chi")


# -->
# NOTE THAT !!! the drop1 function uses the parameter k from the full model in the sub-model,
# so we can not compare other model by anova() !!!
# The resulted deviance is a slightly different values



# ------------------------------------------------------------------------------
# Plot fitted values against each covariate, setting other covariates to its mean (= 0 here, since standardized)
# ------------------------------------------------------------------------------

par(mfrow = c(1,3), mar = c(5,5,2,2))

plot(x = SQ2$Ntrees.std, 
     y = SQ2$SqCones,
     xlab = "Standardized number of trees",
     ylab = "Number of stripped cones",
     cex.lab = 2,
     pch = 16, type = "n")

range(SQ2$Ntrees.std)
MyData <- data.frame(Ntrees.std = seq(-1.1, 4, length = 25),
                     TreeHeight.std  = 0,
                     CanopyCover.std = 0)
P1 <- predict(M2, newdata = MyData, type = "link", se = TRUE)
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
P1 <- predict(M2, newdata = MyData, type = "link", se = TRUE)
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
P1 <- predict(M2, newdata = MyData, type = "link", se = TRUE)
lines(x = MyData$CanopyCover.std, y = exp(P1$fit), lwd = 3)
lines(x = MyData$CanopyCover.std, y = exp(P1$fit + 2*P1$se.fit), lwd = 3, lty = 2)
lines(x = MyData$CanopyCover.std, y = exp(P1$fit - 2*P1$se.fit), lwd = 3, lty = 2)


