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
# Fitting negative binomial GLM model in JAGS
# Model interpretation
# ------------------------------------------------------------------------------

vars <- c("beta[1]", "beta[2]","beta[3]", "beta[4]", "size") 

MyBUGSHist(K2$BUGSoutput, vars)



# -->
# beta[3], which is regression parameter for Tree Height.
# This covariate is not significantly related to the number of cones stripped by red squirrels in a model

# The intercept is the parameter beta[1] and is highly significant.

# There is a significant Canopy Cover effect (beta[4]).
# The covariate Number of Trees has a weak significant effect (beta[2])


