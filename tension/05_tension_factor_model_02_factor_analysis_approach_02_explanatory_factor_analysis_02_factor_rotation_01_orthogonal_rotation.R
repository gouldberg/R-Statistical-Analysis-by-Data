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
# Factor Rotation:  Orthogonal rotation  (factors are independent from each other)
#
#   - for orthogonal rotation, we impose restriction on rotaion matrix T:  T T' = I
#     This keeps the orthogonal factor structure intact, the loadings are altered, but fit remains unchanged.
#
#   - "varimax" rotation:  most popular orthogonal rotation
# ------------------------------------------------------------------------------


resFA2 <- fa(polcor, nfactors = 3, rotate = "varimax", fm = "ml")




# ------------------------------------------------------------------------------
# Check factor loadings
# ------------------------------------------------------------------------------

print(resFA$loadings, cutoff = 0.2)


print(resFA2$loadings, cutoff = 0.2)




# ----------
# for comparison
plot(prin)




# ------------------------------------------------------------------------------
# Communalities
#   - Portion of variances (sum pf squared loadings) of the manifest variables accounted by the common factor effects
#   - squared multiple correlations explained by the common factors
# ------------------------------------------------------------------------------

# note that communality (fit) remain unchanged

round(resFA$communality, 4)

round(resFA2$communality, 4)


