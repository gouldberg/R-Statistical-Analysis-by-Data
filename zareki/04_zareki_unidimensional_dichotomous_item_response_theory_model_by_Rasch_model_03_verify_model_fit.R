setwd("//media//kswada//MyFiles//R//harvardpsych")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  zareki
# ------------------------------------------------------------------------------

data("zareki", package = "MPsychoR")

str(zareki)



# We consider eight binary subtraction items only
zarsub <- zareki[, grep("subtr", colnames(zareki))]

str(zarsub)



# ----------
# compute tetrachoric correlation matrix

library(psych)

tetcor <- tetrachoric(zarsub)

tetcor

tetcor$rho



# ------------------------------------------------------------------------------
# Unidimensional Dichotomous IRT Models:  The Rasch Model
# Verify model fits by LR test
# ------------------------------------------------------------------------------

# Rasch measurement implies that the item parameters have to be invariant across person subgroups
# Therefore, for the Rasch model to fit, the item parameters based on separate subgroup fits have to be approximately the same.
# We split the sample according to a median or mean split based on the number of items solved or, even better perform the split according to one or
# multiple binary covariates.

timecat <- factor(zareki$time <= median(zareki$time), labels = c("fast", "slow"))

timecat


fitLR <- LRtest(fitrasch1, timecat)

fitLR



# -->
# Siginificant, implying that the likelihoods differ across the two groups. The model does not fit.
# It has been shown in simulation studies that the LR-test works well for detecting violations of unidimensionality and parallel ICC violations.



# ------------------------------------------------------------------------------
# Wald test
# ------------------------------------------------------------------------------

Waldtest(fitrasch1, timecat)



# -->
# Here we can use the magnitudes of the p-values to judge the degree of misfit and come to the conclusion that subtr5 is most responsible for the misfit.



# ------------------------------------------------------------------------------
# Plot goodness of ift
# ------------------------------------------------------------------------------

plotGOF(fitLR, ctrline = list(col = "gray"), conf = list())



# -->
# If the parameters were exactly the same across the two subsamples, they would like on the diagonal, and the model would fit.
# The gray lines reflect the confidence bands around the diagonal.
# The 95% confidence ellipses for the item parameters are shown in red.

# We see that subtr5 is clearly outside the confidence band, and we have good evidence for eliminating it.
