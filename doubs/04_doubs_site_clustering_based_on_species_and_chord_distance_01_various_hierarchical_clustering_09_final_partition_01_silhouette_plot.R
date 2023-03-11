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
# Silhouette Plot of the final partition
# ------------------------------------------------------------------------------

k <- 4

spech.ward.g <- cutree(spe.ch.ward, k = k)

sil <- cluster::silhouette(spech.ward.g, spe.ch)

rownames(sil) <- row.names(spe)



# ----------
graphics.off(); par(mfrow=c(1,1));

plot(sil, main = "Silhouette plot - Chord - Ward.D2", cex.names = 0.8, col = 2:(k+1), nmax = 100)



# -->
# Cluster 1 and 3 are the most coherent, while cluster 2 contains misclassified objects.


