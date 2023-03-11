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
# Passive (post hoc) explanation of axes using environmental variables
# A posteriori projection of environmental variables in a PCA
#
# envfit() finds vectors or factor averages of environmental variables.
# The projections of points onto vectors have maximum correlation with corresponding environmental variables,
# and the factors show the averages of factor levels.
# ------------------------------------------------------------------------------

# scaling 2 is default
( spe.h.pca.env <- envfit(spe.h.pca, env, scaling = 2) ) 



# ----------
# Plot significant variables with a user-selected colour
# This has added the significant environmental variables to the last biplot drawn by R.
# BEWARE: envfit must be given the same scaling as the plot to which its result is added!

# envfit() computes a permutation test of the environmental variables and
# the plot() function allows users to draw only the variables with p-values equal to or smaller than a given level.

par(mfrow=c(1,1))

biplot(spe.h.pca, main = "PCA fish abundances - scaling 2")

plot(spe.h.pca.env, p.max = 0.05, col = 3)



