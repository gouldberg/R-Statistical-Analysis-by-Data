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
# Model Negative Binomial GLM by msme::nbinomial()
# ------------------------------------------------------------------------------
library(msme)


nb2 <- nbinomial(SqCones ~ Ntrees.std + TreeHeight.std + CanopyCover.std, data = SQ2)


summary(nb2)



# -->
# Dispersion is 0.8955, indicating that the negative binomial is slightly under-dispersed.
# (intercept)_s value is 0.927 instead of 1.0791 as reported for theta (= 1 / 1.0791)



# ------------------------------------------------------------------------------
# Scaled standard errors
# ------------------------------------------------------------------------------
# standard errors should be scaled by square root of dispersion parameter

# intercept
0.1420 * sqrt(0.8955484)


# regression parameter of Ntrees.std
0.1609 * sqrt(0.8955484)


# regression parameter of TreeHeight.std
0.1564 * sqrt(0.8955484)


# regression parameter of CanopyCover.std
0.1790 * sqrt(0.8955484)



# ------------------------------------------------------------------------------
# p-value for regression parameter by Scaled standard errors
# ------------------------------------------------------------------------------

# intercept
2 * pnorm(2.618 / (0.1420 * sqrt(0.8955484)), lower.tail = FALSE)


# regression parameter of Ntrees.std  --> now significant predictor of the response at the 0.05 significance level.
2 * pnorm(0.306 / (0.1609 * sqrt(0.8955484)), lower.tail = FALSE)


# regression parameter of TreeHeight.std
2 * pnorm(0.155 / (0.1564 * sqrt(0.8955484)), lower.tail = FALSE)


# regression parameter of CanopyCover.std
2 * pnorm(0.536 / (0.1790 * sqrt(0.8955484)), lower.tail = FALSE)


