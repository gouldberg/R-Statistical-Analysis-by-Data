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
# PCA on transformed species data
#   - PCA on the fish abundance data
# ------------------------------------------------------------------------------

# Hellinger pre-transformation of the species data
# The justification is that the chord, Hellinger and log-chord distances computed on presence-absence data are equal to sqrt(2) * sqrt(1 - Ochiai similarity),
# so PCA after Hellinger or chord transformation preserves the Ochiai distance among objects in scaling type 1 plots.
# squrt(1 - Ochiai similarity) is a metric and Euclidean distance, which is appropriate for ordination analysis of community composition presence-absence data.

spe.h <- decostand(spe, "hellinger")


# ----------
# For comparison, chi-quare transformation
spe.c <- decostand(spe, "chi.square")


head(spe)

head(spe.h)

head(spe.c)



# ----------
# principal component analysis

( spe.h.pca <- rda(spe.h) )

( spe.pca <- rda(spe) )

( spe.c.pca <- rda(spe.c) )



# ------------------------------------------------------------------------------
# Scree plot and broken stick model
# -----------------------------------------------------

graphics.off();  par(mfrow=c(2,2));

screeplot(spe.h.pca, bstick = TRUE, npcs = length(spe.h.pca$CA$eig))

screeplot(spe.c.pca, bstick = TRUE, npcs = length(spe.c.pca$CA$eig))

screeplot(spe.pca, bstick = TRUE, npcs = length(spe.pca$CA$eig))


# -->
# Note that the result of PCA of Hellinger pre-transformed data have comparatively smoothly decreasing scree plot.
# Although PCA itself is not modified and remains a linear ordination model,
# the pre-transformations ensure that the species data are treaated according to their specificity,
# i.e. without undue importance being given to double zeros.


