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
# Explanatory Factor Analaysis
# ------------------------------------------------------------------------------


resFA <- fa(polcor$rho, nfactors = 2, rotate = "none", fm = "ml")


summary(resFA)




# ------------------------------------------------------------------------------
# Check factor loadings
# ------------------------------------------------------------------------------

print(resFA$loadings)




# ----------
# blanking out lodings < 0.2

print(resFA$loadings, cutoff = 0.2)




# ------------------------------------------------------------------------------
# Communalities
#   - Portion of variances (sum pf squared loadings) of the manifest variables accounted by the common factor effects
#   - squared multiple correlations explained by the common factors
# ------------------------------------------------------------------------------


round(resFA$communality, 2)



# -->
# note that communalities for ceaq4, 10, 15 < 0.20



# ----------
# here the Princals solution

plot(prin, main = "CEAQ Loadings")




