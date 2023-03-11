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
# Spatial plot of the clusters
# ------------------------------------------------------------------------------

source("./functions/drawmap.R")


graphics.off();  par(mfrow = c(1,1));


drawmap(xy = spa, clusters = spech.ward.g, main = "4 Ward clusters along the Doubs River")




# ------------------------------------------------------------------------------
# Compare with the maps of four fish species
#  - Brown trout (Satr):  abundant in cluster 1 and 2
#  - Gralyling (Thth):  abundant in cluster 1 only at intermediate part of the river
#  - Barbel (Baba):  abundant in cluster 2 and 3
#  - Common bream (Abbr):  abundant in cluster 3
# ------------------------------------------------------------------------------

par(mfrow = c(2,2))

plot(spa, asp = 1, cex.axis = 0.8, col = "brown", cex = spe$Satr, main = "Brown trout", xlab = "x coordinate (km)", ylab = "y coordinate (km)")
lines(spa, col = "light blue")

plot(spa, asp = 1, cex.axis = 0.8, col = "brown", cex = spe$Thth, main = "Gralyling", xlab = "x coordinate (km)", ylab = "y coordinate (km)")
lines(spa, col = "light blue")

plot(spa, asp = 1, cex.axis = 0.8, col = "brown", cex = spe$Baba, main = "Barbel", xlab = "x coordinate (km)", ylab = "y coordinate (km)")
lines(spa, col = "light blue")

plot(spa, asp = 1, cex.axis = 0.8, col = "brown", cex = spe$Abbr, main = "Common bream", xlab = "x coordinate (km)", ylab = "y coordinate (km)")
lines(spa, col = "light blue")
