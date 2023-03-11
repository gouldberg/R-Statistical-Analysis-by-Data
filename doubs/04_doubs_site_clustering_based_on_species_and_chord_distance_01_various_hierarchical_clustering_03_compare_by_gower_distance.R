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
# Comparing Hierarchical Clustering:  Gower distance
#  - Gower distance computed as the sum of squared differences between the original dissimilarities and cophenetic distances
#  - The clustering method that produces the smallest Gower distance may be seen as the one that provides the best clustering model of the dissimilarity matrix.
#  - This measure, also called stress 1, takes values in the interval 0 to infinite.  It is used in standardized form as a measure of goodness-of-fit in nonmetric multidimensional scaling.
# ------------------------------------------------------------------------------

# UPGMA is the best clustering model based on Gower distance

( gow.dist.single <- sum((spe.ch - spe.ch.single.coph)^2) )

( gow.dist.complete <- sum((spe.ch - spe.ch.complete.coph)^2) )

( gow.dist.UPGMA <- sum((spe.ch - spe.ch.UPGMA.coph)^2) )

( gow.dist.ward <- sum((spe.ch - spe.ch.ward.coph)^2) )

( gow.dist.beta2 <- sum((spe.ch - spe.ch.beta2.coph)^2) )



