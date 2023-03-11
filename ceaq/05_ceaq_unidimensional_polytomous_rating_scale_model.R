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
# Rating Scale Model
#   - The rating scale model (RSM) is one of the simplest IRT models for polytomous data.
#   - The RSM expresses the probability that a person v scores category h on item i:
#     P(X(vi) = h) = exp(h * (theta(v) + beta(i)) + omega(h)) / sum{ exp(l * (theta(v) + beta(i)) + omega(l)) }
#     As in the Rasch model, theta(v) denotes the person parameter and beta(i) the item location parameter, here again
#     written as easiness parameter in order to be consistent with the specification used in the eRm package.
#   - Each category h gets category parameter omega(h), constant across items.
#     This means that item differences are solely reflected by the shifts in beta(i) across items
# ------------------------------------------------------------------------------


library(eRm)


fitrsm <- RSM(itceaq)


fitrsm



# ----------
# The item parameters
head(coef(fitrsm))



#   - The RSM expresses the probability that a person v scores category h on item i:
#     P(X(vi) = h) = exp(h * (theta(v) + beta(i)) + omega(h)) / sum{ exp(l * (theta(v) + beta(i)) + omega(l)) }

