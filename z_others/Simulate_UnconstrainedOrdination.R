# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
rm(list=ls())
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "vegan", "gclus", "ape", "missMDA", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# test data
#  - "Numerical Ecology" by Legendre & Legendre  (p.430)
# ------------------------------------------------------------------------------
v1 <- c(2,3,5,7,9)
v2 <- c(1,4,0,6,2)
( y <- cbind(v1, v2) )
rownames(y) <- c(1,2,3,4,5)

n <- nrow(y)
p <- 2
d <- 2


# centering on the column means
( yc <- scale(y, center=TRUE, scale=FALSE) )


graphics.off();  par(mfrow=c(1,1));
plot(yc)


# ------------------------------------------------------------------------------
# Principal component analysis (PCA) by rda()
# ------------------------------------------------------------------------------
# PCA based on a correlation matrix
# Argument scale=TRUE calls for a standardization of the variables
y.pca <- vegan::rda(y, scale = FALSE)
y_s.pca <- vegan::rda(y, scale = TRUE)
y.pca
y_s.pca


# Summary:  default scaling 2
summary(y.pca)
summary(y_s.pca)

summary(y.pca, scaling = 1)
summary(y_s.pca, scaling = 1)


# Eigenvalues
( evl <- y.pca$CA$eig )
( evl_s <- y_s.pca$CA$eig )


# the ratio of cumulative eignevalues to total variance = Coefficient of determination (R^2)
# (in PCA, the Euclidean distances among objects have been preserved through the rotation of axes)
cumsum(evl) / sum(evl)


# Eigenvalues matrix (Lambda)
( Lambda <- matrix(nrow = p, ncol = p, 0) )
diag(Lambda) <- evl
Lambda


# Eigenvectors matrix
# The elements of the eigenvectors are also weights, or loadings of the original descriptors, in the linear combination of descriptors from which the principal components are computed.
# The elements of the eignevectors are direction cosines of the angles between the original descriptors and the principal axes
( U <- y.pca$CA$v )


# Eigenvectors for scaling 2
( Usc2 <- U * sqrt(Lambda) )


# diagonal terms of t(U) %*% U is (length)^2 of the eigenvectors = 1, here equal to 1 because the eigenvectors are scaled
# non-diagonal terms of t(U) %*% U is zero, since eigenvectors are orthogonal
t(U) %*% U





# ------------------------------------------------------------------------------
# Principal component analysis (PCA) by calculation
# ------------------------------------------------------------------------------
# Computing the eigenvectors of a dispersion matrix (covariance matrix)
( S <- 1 / (n-1) * t(yc) %*% yc )


# Check
sum(diag(S))
sum(evl)


# Computing the principal components
( Fmat <- yc %*% U )


# The variance of principal components is equal to eigenvalue
apply(Fmat, 2, var)
evl


# 
Gmat <- Fmat * sqrt()



