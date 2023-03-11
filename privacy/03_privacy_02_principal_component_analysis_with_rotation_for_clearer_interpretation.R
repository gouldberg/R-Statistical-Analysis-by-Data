setwd("//media//kswada//MyFiles//R//privacy")

packages <- c("dplyr", "MPsychoR", "corrplot", "BayesFM")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Privacy
# ------------------------------------------------------------------------------

data("Privacy", package = "MPsychoR")


str(Privacy)



# ----------
Privstd <- scale(Privacy)

head(Privstd)



# ------------------------------------------------------------------------------
# Refit the unrotated solution using psych::principal
# ------------------------------------------------------------------------------

library("psych")

pcaPriv1 <- principal(cor(Privacy), 3, rotate = "none")

summary(pcaPriv1)

pcaPriv1$loadings



# -->
# Note that the loadings are not the same as in the prcomp output.
# The reason for this is that principal standardized the loadings differently, their sum-of-squares (SS) equal to the square of the corresponding eigenvalue
# whereas prcomp and princomp standardize them to SS equal to 1.

# But these are just two different ways of standardizing, the loadings structure is the same, and it does not change anything in the results.



# ------------------------------------------------------------------------------
# Orthogonally rotated solution (varimax)
# ------------------------------------------------------------------------------

pcaPriv2 <- principal(cor(Privacy), 3, rotate = "varimax")

summary(pcaPriv2)

pcaPriv2$loadings


# -->
# Note that the components are labelled with RC (rotated components)
# the first 3 apc items load highly on RC1
# the remaining apc items load highly on RC3
# the four dpc items determing RC2


# We see that a rotation makes the interpretation easier, but the price we pay is that important properties of PCA are getting lost.
# We see that the proportions of explained variance got redistributed across the components



# ------------------------------------------------------------------------------
# Non-Orthogonally rotated solution (promax)
# ------------------------------------------------------------------------------

pcaPriv3 <- principal(cor(Privacy), 3, rotate = "promax")

summary(pcaPriv3)

pcaPriv3$loadings


# -->
# gives even clearer loadings structure








