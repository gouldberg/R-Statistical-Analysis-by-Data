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
# Model Poisson GLM
# ------------------------------------------------------------------------------

M1 <- glm(SqCones ~ Ntrees.std + TreeHeight.std + CanopyCover.std, family = "poisson", data = SQ2)


print(summary(M1), digits=3, signif.stars = FALSE)



# ----------
( M1$null.deviance - M1$deviance ) / M1$null.deviance


# -->
# The model explains only 26.6% of the variation in the number of stripped cones.



