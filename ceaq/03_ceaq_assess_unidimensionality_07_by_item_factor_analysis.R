setwd("//media//kswada//MyFiles//R//ceaq")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  CEAQ
# ------------------------------------------------------------------------------

data("CEAQ", package = "MPsychoR")


str(CEAQ)


car::some(CEAQ)



# ----------
# set the lowest category to zero in order to make it eRm compaatible
itceaq <- CEAQ[,1:16] - 1



# ----------
library(psych)

polcor <- polychoric(itceaq)


polcor

polcor$rho



# ------------------------------------------------------------------------------
# Item Factor Analysis (IFA)
#   - IFA is a variant of Exploratory Factor Analysis suited for categorical data
# ------------------------------------------------------------------------------

library(mirt)


# We fit a one-factor model and a two-factor model
fitifa1 <- mirt(itceaq, 1)


fitifa2 <- mirt(itceaq, 2, TOL = 0.001)


fitifa3 <- mirt(itceaq, 3, TOL = 0.001)


fitifa1




# ----------
# We compare these nested fits via a likelihood-ratio (LR) test, in addition to the usual AIC/BIC criteria.
anova(fitifa1, fitifa2)


anova(fitifa2, fitifa3)



# -->
# 2 factors is better than 1 factor
# 3 factors is NOT significantly better than 2 factors



# -->
# So far, we conclude that there is no drastic unidimensionality violation in these data.
# Still, we obtained some hints that it might be slightly violated.
# We do not have to exclude any items at this point since for simple dichotomous IRT models various tests are sensitive to unidimensionality violations.



# ----------
plot(fitifa2)

