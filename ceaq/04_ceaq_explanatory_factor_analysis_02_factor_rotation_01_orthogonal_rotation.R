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
# set the lowest category to zero in order to make it eRm compatible
itceaq <- CEAQ[,1:16] - 1



# ----------
library(psych)

polcor <- polychoric(itceaq)


polcor

polcor$rho




# ------------------------------------------------------------------------------
# Factor Rotation:  Orthogonal rotaion  (factors are independent from each other)
#
#   - for orthogonal rotation, we impose restriction on rotaion matrix T:  T T' = I
#     This keeps the orthogonal factor structure intact, the loadings are altered, but fit remains unchanged.
#
#   - "varimax" rotation:  most popular orthogonal rotation
# ------------------------------------------------------------------------------


resFA2 <- fa(polcor$rho, nfactors = 2, rotate = "varimax", fm = "ml")




# ------------------------------------------------------------------------------
# Check factor loadings
# ------------------------------------------------------------------------------

print(resFA$loadings, cutoff = 0.2)

print(resFA2$loadings, cutoff = 0.2)




# -->
# Note that ML1 and ML2 for some item are balanced compared to unrotated solution




# ------------------------------------------------------------------------------
# Communalities
#   - Portion of variances (sum pf squared loadings) of the manifest variables accounted by the common factor effects
#   - squared multiple correlations explained by the common factors
# ------------------------------------------------------------------------------

# note that communality (fit) remain unchanged

round(resFA$communality, 4)

round(resFA2$communality, 4)


