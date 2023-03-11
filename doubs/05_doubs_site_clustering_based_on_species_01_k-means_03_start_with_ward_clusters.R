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
# Ward + k-means
# 
# Use of k-means partitioning to optimize an independently obtained classification
# Hierarchical clustering (ward.D2) --> its mean as starting point --> k-means
# ------------------------------------------------------------------------------

# Compute starting points:  means per group (centroids) based on Ward's hiearchical clustering

groups <- as.factor(spech.ward.g)

spe.means <- matrix(0, ncol(spe), length(levels(groups)))

row.names(spe.means) <- colnames(spe)

for(i in 1:ncol(spe)){ spe.means[i,] <- tapply(spe.norm[,i], spech.ward.g, mean) }

startpoints <- t(spe.means)



# ----------
# k-means with Ward species means per group (centroids) as starting points
spe.kmeans2 <- kmeans(spe.norm, centers = startpoints)



# ------------------------------------------------------------------------------
# Ward + k-means
# A slightly different approach is to go back to the hierarchical clustering, 
# identify the most 'typical' object ("medoids") in each group (cf. silhouette plot), and provide these medoids as starting points to kmeans:
# ------------------------------------------------------------------------------

spech.ward.g <- cutree(spe.ch.ward, k = 4)

sil_ward <- cluster::silhouette(spech.ward.g, spe.ch)

rownames(sil_wafd) <- row.names(spe)

graphics.off(); par(mfrow=c(1,1));

plot(sil_ward, main = "Silhouette plot - Chord - Ward.D2", cex.names = 0.8, col = 2:(k+1), nmax = 100)



# ----------
# medoids: site 2, 17, 21, 23
startobjects <- spe.norm[c(2, 17, 21, 23), ]

spe.kmeans3 <- kmeans(spe.norm, centers = startobjects)



# ----------
# only site 15 is different between Ward hierarchical clustering and k-means clustering with starting points
table(spe.kmeans2$cluster, spech.ward.g)


# Same cluster
table(spe.kmeans2$cluster, spe.kmeans3$cluster)


