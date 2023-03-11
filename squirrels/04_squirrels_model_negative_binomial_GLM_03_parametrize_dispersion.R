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
# Additional feature of the nbinomial function is 
# it allows for the dispersion parameter alpha to be parameterized
#   - SqCones ~ NB(mu, alpha)
#   - E(SqCones) = mu,  var(SqCones) = mu + alpha + mu^2
#   - log(mu) = beta1 + beta2 * NTrees + beta3 * TreeHeight + beta4 * CanopyCover
#   - log(alpha) = gamma1 + gamma2 * NTrees + gamma3 * TreeHeight + gamma4 * CanopyCover
# ------------------------------------------------------------------------------

library(msme)


nbH <- nbinomial(SqCones ~ Ntrees.std + TreeHeight.std + CanopyCover.std,
                 formula2 =~ Ntrees.std + TreeHeight.std + CanopyCover.std,
                 family = "negBinomial",
                 mean.link = "log", scale.link = "log_s", data = SQ2)


summary(nbH)



# -->
# estimated gamma1, gamma2, gamma3 and gamma4 are -0.217, -0.384, 0.325, 0.273
# None of these regression parameters are significant.

# THis is not surprising since the negative binomial dispersion statistic is close to 1 (= 1.08)


