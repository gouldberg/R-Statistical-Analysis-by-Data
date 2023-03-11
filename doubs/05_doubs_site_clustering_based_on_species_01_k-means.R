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
# k-means clustering
#  - The k-means method uses the local structure of the data to delineate clusters:  groups are formed by identifying high-density regions in the data.
#  - To achieve this, the method iteratively minimizes an objective function called the total error sum of squares (same criterion as used in Ward's agglomerative clustering)
#  - k-means is a linear method, i.e., it is not appropriate for raw species abundance data with lots of zeros. To remain coherent with previous hierarchical clustering, we can use the chord-transformed or "normalized" speciies data.
# ------------------------------------------------------------------------------

# k-means partitioning of the pre-transformed species data

k <- 4

set.seed(201808)

spe.kmeans <- kmeans(spe.norm, centers = k, nstart = 100)

spe.kmeans



# ----------
# Compare with hierarchical clusting --> fairly similar except one site.
table(spe.kmeans$cluster, spech.ward.g)



