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
# Explanatory Factor Analaysis
# ------------------------------------------------------------------------------

library(psych)

resFA <- fa(polcor, nfactors = 3, rotate = "none", fm = "ml")


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





# ----------
# here the Princals solution

plot(prin, main = "Loadings")




