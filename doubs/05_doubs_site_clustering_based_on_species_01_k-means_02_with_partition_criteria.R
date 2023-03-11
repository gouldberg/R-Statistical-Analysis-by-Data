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
# k-means clustering with partition criterion
# cascadeKM() is a wrapper for the kmeans() function.  It creates several partitions forming a cascade from small (inf.gr) to large values of k (sup.gr).
# Criterion to determine number of clusters
#  - Milligan and Cooper (1985) recommend maximizing the Calinski-Harabasz index (F-statistic comparing the comng-group to the within-group sum of squares of the partition),
#   although its value tends to be lower for unequal-sized paritions.
#  - The maximum of Simple Structure Index (ssi) is another good indicator of the best partition in the least-squares sense.
# ------------------------------------------------------------------------------

set.seed(201808)

spe.KM.cascade <- vegan::cascadeKM(spe.norm, inf.gr = 2, sup.gr = 10, iter = 100, criterion = "ssi")



# ----------
spe.KM.cascade$results

spe.KM.cascade$size



# -->
# "results" shows TESS statistic and the value of criterion (calinski or ssi) for each value of k
# 3 groups has maximized ssi
# If one has reasons to prefer a larger number of groups, 4 groups is best.




# ------------------------------------------------------------------------------
# clustering result
# ------------------------------------------------------------------------------

# 2 colors for k = 2, 3 colors for k = 3
graphics.off();  par(mfrow=c(1,1));

plot(spe.KM.cascade, sortg = TRUE)




# ------------------------------------------------------------------------------
# Examine clusters contents:  Reorder the sites according to the k-means result
# ------------------------------------------------------------------------------

# Examine clusters contents:  Reorder the sites according to the k-means result
spe.kmeans.g <- spe.kmeans$cluster

spe[order(spe.kmeans.g),]



# ----------
# Reorder sites and species using function vegemite()
( Ord.KM <- vegan::vegemite(spe, spe.kmeans.g) )

spe[Ord.KM$sites, Ord.KM$species]



