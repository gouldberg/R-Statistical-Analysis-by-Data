setwd("//media//kswada//MyFiles//R//zareki")

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
# Item Factor Analysis (IFA)
#   - IFA is a variant of Exploratory Factor Analysis suited for categorical data
# ------------------------------------------------------------------------------

library(mirt)


# We fit a one-factor model and a two-factor model
fitifa1 <- mirt(zarsub, 1)

fitifa2 <- mirt(zarsub, 2, TOL = 0.001)

fitifa1



# ----------
# We compare these nested fits via a likelihood-ratio (LR) test, in addition to the usual AIC/BIC criteria.
anova(fitifa1, fitifa2)



# -->
# THe nonsignificant result of the LR-test suggests that the 2D fit is not superior to the 1D fit.
# AIC and BIC are (slightly) lower for the 1D solution.


# -->
# So far, we conclude that there is no drastic unidimensionality violation in these data.
# Still, we obtained some hints that it might be slightly violated.
# We do not have to exclude any items at this point since for simple dichotomous IRT models various tests are sensitive to unidimensionality violations.

# If we would have detected two clearly separated sets of items, we could fit two separate unidimensional IRT models, one for each itemset.

