setwd("//media//kswada//MyFiles//R//brainIQ")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  brainIQ
# ------------------------------------------------------------------------------

data("BrainIQ", package = "MPsychoR")

str(BrainIQ)


# ----------
X <- BrainIQ[, c("VIQ", "PIQ", "MRI_Count")]

head(X, 4)


# -->
# In this dataset the variables are measured on different units:
# the brain size magnitude is in the 1M area, whereas the VIQ and PIQ are on the usual IQ scale.



# ------------------------------------------------------------------------------
# PCA via eigenvalue decomposition by princomp
# ------------------------------------------------------------------------------

# Using the correlation matrix instead of the covariance matrix implies standardization
PCAfit <- princomp(X, cor = TRUE)


PCAfit



# ----------
# weights (loadings)
PCAfit$loadings

round(unclass(PCAfit$loadings), 3)



# ----------
# How princomp works
# eigen value decomposition
( evIQ <- eigen(cor(X)) )

# standard deviations
sqrt(evIQ$values)
PCAfit

# weights (loadings)
round(evIQ$vectors, 3)
round(unclass(PCAfit$loadings), 3)



# ----------
# Cumulative proportion
summary(PCAfit)
cumsum(evIQ$values) / sum(evIQ$values)



# ------------------------------------------------------------------------------
# PCA based on an SVD involving the standardized data matrix (and divided by sqrt(n-1) by prcomp
# ------------------------------------------------------------------------------

PCAfit2 <- prcomp(X, scale = TRUE)

print(PCAfit2, digits = 3)


# ----------
# How prcomp works
svdIQ <- svd(scale(X) / sqrt(nrow(X) - 1))

# singular values  --> standard deviation
round(svdIQ$d, 3)

# right singular vectors --> loadings (rotation)
round(svdIQ$v, 3)



# ----------
# component scores
head(PCAfit2$x, 4)

head(svdIQ$u %*% diag(svdIQ$d) * sqrt(nrow(X)-1), 4)



