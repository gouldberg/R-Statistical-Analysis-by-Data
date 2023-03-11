setwd("//media//kswada//MyFiles//R//tension")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  tension
# ------------------------------------------------------------------------------

data("tension", package = "MPsychoR")


str(tension)


dim(tension)



# -----------
tens <- t(as.matrix(tension[, 1:800]))



# ------------------------------------------------------------------------------
# standardize individual series
# ------------------------------------------------------------------------------


std <- diag(1 / sqrt(diag(cov(tens))))


tens <- as.matrix(tens) %*% std




# ------------------------------------------------------------------------------
# compute correlation matrix
# ------------------------------------------------------------------------------

polcor <- cor(tens)


polcor



# ------------------------------------------------------------------------------
# Factor Rotation:  Un-Orthogonal rotaion ("oblique rotation")  (factors are NOT independent from each other)
#
#   - for un-orthogonal rotation, not any more rotation matrix T:  T T' != I
#
#   - Popular un-orthogonal ("oblique") rotation approaches:  "oblimin" and "promax"
#   - In practive, EFA with oblique rotation is often used prior to a CFA in order to explore
#     whether the underlying latent structure theory is reflected by the data.
# ------------------------------------------------------------------------------


resFA3 <- fa(polcor, nfactors = 3, rotate = "oblimin", fm = "ml")


resFA4 <- fa(polcor, nfactors = 4, rotate = "oblimin", fm = "ml")




# ------------------------------------------------------------------------------
# Check factor loadings
# ------------------------------------------------------------------------------

print(resFA3$loadings, cutoff = 0.2)


print(resFA4$loadings, cutoff = 0.2)



# -->
# Unorthogonal solutions are better match to Princal solutions !!



# ----------
fa.diagram(resFA2)

fa.diagram(resFA3)

fa.diagram(resFA4)




# ------------------------------------------------------------------------------
# Communalities
#   - Portion of variances (sum pf squared loadings) of the manifest variables accounted by the common factor effects
#   - squared multiple correlations explained by the common factors
# ------------------------------------------------------------------------------

# note that communality (fit) remain unchanged for 2 factors but changed for 3 factors

round(resFA2$communality, 4)

round(resFA3$communality, 4)

round(resFA4$communality, 4)




# ------------------------------------------------------------------------------
# Factor correlation matrix
# ------------------------------------------------------------------------------

# since the rotation is un-orthogonal, factors are correlated

round(resFA3$Phi, 3)

round(resFA4$Phi, 4)



# -->
# factors are not strongly correalted






