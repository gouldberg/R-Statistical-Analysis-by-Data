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



# ----------
# chord distance among sites (Q-mode):  Euclidean distance computed on site vectors normalized to length 1
spe.norm <- decostand(spe, "normalize")
spe.ch <- vegdist(spe.norm, "euc")
attr(spe.ch, "labels") <- rownames(spe)


# ----------
# for comparison, clusters by hierarchical clustering
spe.ch.ward <- hclust(spe.ch, method = "ward.D2")
k <- 4
spech.ward.g <- cutree(spe.ch.ward, k = k)




# ------------------------------------------------------------------------------
# Silhouette plot of Final partition
# ------------------------------------------------------------------------------

spech.ward.gk <- spe.kmeans2$cluster

sil_wardgk <- cluster::silhouette(spech.ward.gk, spe.ch)

rownames(sil_wardgk) <- row.names(spe)


graphics.off();  par(mfrow = c(1, 2));

k <- 4

plot(sil_ward, main = "Silhouette plot - Chord - Ward.D2", cex.names = 0.8, col = 2:(k+1), nmax = 100)

plot(sil_wardgk, main = "Silhouette plot - Ward & k-means", cex.names = 0.8, col = 2:(k + 1), nmax = 100)


