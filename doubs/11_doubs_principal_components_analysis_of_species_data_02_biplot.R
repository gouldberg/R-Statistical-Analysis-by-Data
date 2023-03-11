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
# PCA biplot  (Hellinger pre-transformed data)
# ------------------------------------------------------------------------------

source("./functions/cleanplot.pca.R")

spe.pca.sc1 <- scores(spe.h.pca, display = "species", scaling = 1)

spe.pca.sc2 <- scores(spe.h.pca, display = "species", scaling = 2)



# ----------
par(mfrow = c(1, 2))

cleanplot.pca(spe.h.pca, scaling = 1, mar.percent = 0.06)

cleanplot.pca(spe.h.pca, scaling = 2, mar.percent = 0.06)



# -->
# Scaling 1 PCA biplot reveals the underlying gradients structuring the commnuity:
# the sites are ordered along the axes according to their positions along the gradients.
# The circle of equilibrium contribution allows the identification of the species contributing most to the plotted pair of axes.

# Scaling 2 biplot reveals the relationships among species in a correlation-like-fasion;
# since the data have been transformed, the correlations are not equivalent to Pearson's r computed from the raw data.


# The species do not form clear groups like the environmental variables. However, see how the species replace one another along the site sequence.
# In the scaling 1 biplot, observe that 8 species contribute strongly to axes 1 and 2.
# Are these species partly or completely the same as those identified as indicators of the groups ?



# ------------------------------------------------------------------------------
# PCA biplot  (Chi-square pre-transformed data)
# ------------------------------------------------------------------------------

# Technical note:
# PCA solution is very similar, but not identical to a correspondence analysis (CA) of the species data.
# Although the two methods preserve the chi-square distance among the sites,
# the calculation of the eigen-decomposition is not done in exactly the same way and leads to different sets of eigenvalues and eigenvectors.

spe.pca.sc1 <- scores(spe.c.pca, display = "species", scaling = 1)

spe.pca.sc2 <- scores(spe.c.pca, display = "species", scaling = 2)



# ----------
par(mfrow = c(1, 2))

cleanplot.pca(spe.c.pca, scaling = 1, mar.percent = 0.06)

cleanplot.pca(spe.c.pca, scaling = 2, mar.percent = 0.06)



# ------------------------------------------------------------------------------
# Compare biplots of chi-square-transformed, hellinger-transformed, and original data
# ------------------------------------------------------------------------------
# original data biplots (Scaling 1)
#  - in not-hellinger-transfored biplot, many species are outside the circle of equillibrium contribution
#  - Ordination along PC1 is not so much different but ordination along PC2 is much different

par(mfrow = c(3, 2))

cleanplot.pca(spe.c.pca, scaling = 1, mar.percent = 0.06)

cleanplot.pca(spe.c.pca, scaling = 2, mar.percent = 0.06)

cleanplot.pca(spe.h.pca, scaling = 1, mar.percent = 0.06)

cleanplot.pca(spe.h.pca, scaling = 2, mar.percent = 0.06)

cleanplot.pca(spe.pca, scaling = 1, mar.percent = 0.06)

cleanplot.pca(spe.pca, scaling = 2, mar.percent = 0.06)



# ------------------------------------------------------------------------------
# For quick assessment of the structure of data
# ------------------------------------------------------------------------------

source("./functions/PCA.newr.R")

env.PCA.PL <- PCA.newr(env, stand = TRUE)


# ----------
par(mfrow = c(1,2))

biplot.PCA.newr(env.PCA.PL)

biplot.PCA.newr(env.PCA.PL, scaling = 2)

