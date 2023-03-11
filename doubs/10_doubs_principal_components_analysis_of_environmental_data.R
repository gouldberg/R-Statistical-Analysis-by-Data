setwd("//media//kswada//MyFiles//R//doubs")

packages <- c("dplyr", "vegan", "cluster", "dendextend", "pvclust", "labdsv", "RColorBrewer")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Doubs
# ------------------------------------------------------------------------------

load("./data/Doubs.RData")


dim(spe)
car::some(spe)


dim(env)
car::some(env)


dim(spa)
car::some(spa)


dim(fishtraits)
car::some(fishtraits)


dim(latlong)
car::some(latlong)



# ----------
# remove site 8, which have zero species
spe <- spe[-8,]
env <- env[-8,]
spa <- spa[-8,]
latlong <- latlong[-8,]



# ------------------------------------------------------------------------------
# Principal component analysis (PCA)
#  - PCA on the full environmental dataset
#
# PCA works on a dispersion matrix, i.e. an association matrix among variables containing the variances and covariances of the variable
# (when these are dimensionally homogeneou), or the correlations computed from dimensionally heterogeneous variables.
# 
# intertia:
#   - either the sum of the variances of the variables (PCA on a covariance matrix) or
#     as in this case (PCA on a correlation matrix), the sum of the diagonal values of the correlation matrix, i.e. the sum of all correlations of the variables with themselves,
#     which corresponds to the number of variables (11)
# eigenvalues:
#   - measures of the importance (variance) of the PCA axes, proportions of variation accounted for by the axes.
# scaling 1:  distance biplot: the eigenvectors are scaled to unit length.
#   - Distances among objects in the biplot are approximations of their Euclidean distances in multidimensional space.
#   - The angles among descriptor vectors do not reflect their correlations
# scaling 2:  correlation biplot:  each eigenvector is scaled to the square root of its eigenvalue
#   - Distances among objects in the biplot are not approximations of their Euclidean distances in multidimensional space
#   - The angles between descriptors in the biplot reflect their correlations
# If the main interest of the analysis is to interpret the relationships among OBJECTS, choose scaling 1
# If the main interest focuses on th erelationships among DESCRIPTORS, choose scaling 2
# Scaling 3:  "symmetric scaling", supposed to allow a simultaneous representation of sites and scores without emphasizing one or the other point of view.
# ------------------------------------------------------------------------------

# A reminder of the content of the env dataset
summary(env)



# ----------
# PCA based on a correlation matrix
# rda performs redundancy analysis, or optionally principal components analysis
# Argument scale=TRUE calls for a standardization of the variables
# default is scaling 2

env.pca <- vegan::rda(env, scale = TRUE)


env.pca



# ------------------------------------------------------------------------------
# The proportion of variance, site and species scores
# ------------------------------------------------------------------------------

summary(env.pca)



# ----------
# display no site and species scores (axes = 0)
summary(env.pca, axes = 0)



# -->
# The proportion of variance accounted for by the 1st two axes is 0.751 or 75.1%



# ----------
summary(env.pca, scaling = 1)



# ------------------------------------------------------------------------------
# Eigenvectors (Principal axes of dispersion matrix)
# ------------------------------------------------------------------------------

head(env.pca$CA$u)



# -->
# The elements of the eigenvectors are also weights, or loadings of the original descriptors,
# in the linear combination of descriptors from which the principal components are computed.
# The elements of the eignevectors are direction cosines of the angles between the original descriptors and the principal axes



# ------------------------------------------------------------------------------
# Eigenvalues:  the amounts of variance of the data along the succesiive principal axes
# ------------------------------------------------------------------------------
# Examines the eigenvalues, how many axes are worth representing and displaying on the basis of the amount of variance explained ?

( ev <- env.pca$CA$eig )



# ----------
cumsum(env.pca$CA$eig) / sum(env.pca$CA$eig)



# -->
# the ratio of cumulative eignevalues to total variance = Coefficient of determination (R^2)
# (in PCA, the Euclidean distances among objects have been preserved through the rotation of axes)
# The proportion of variance accounted for by the 1st two axes is 0.751 or 75.1%



# ----------
# Scree plot and broken stick model
#  - randomly divides a stick of unit length into the same number of pieces as there are PCA eigenvalues.  The pieces are then put in order of decreasing lengths
#   and compared to the eigenvalues.
#  - One interprets only the axes whose eigenvalues are larger than the length of the corresponding piece of the stick, or, alternately, one may compare
#   the sum of eigenvalues, from 1 to k, to the sum of the values from 1 to k predicted by the broken stick model.

par(mfrow = c(1, 1))

stats::screeplot(env.pca, bstick = TRUE, npcs = length(env.pca$CA$eig))


