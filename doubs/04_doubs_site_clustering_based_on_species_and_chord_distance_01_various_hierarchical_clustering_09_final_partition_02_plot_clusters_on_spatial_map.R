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
# Final Dendrogram
# ------------------------------------------------------------------------------

# Reorder clusters
spe.chwo <- reorder(spe.ch.ward, spe.ch)



# ----------
# plot reordered dendrogram with group labels
graphics.off();  par(mfrow=c(1,1));

plot(spe.chwo, hang = -1, xlab = "4 groups", sub = "", ylab = "Height", main = "Chord - Ward (reordered)", labels = cutree(spe.chwo, k = k))

rect.hclust(spe.chwo, k = k)




# ------------------------------------------------------------------------------
# Spatial plot of the clusters
# ------------------------------------------------------------------------------

source("./functions/drawmap.R")


graphics.off();  par(mfrow = c(1,2));


drawmap(xy = spa, clusters = spech.ward.g, main = "Four optimized Ward clusters along the Doubs River")
drawmap(xy = spa, clusters = spech.ward.gk, main = "Four optimized Ward and k-means clusters along the Doubs River")



# --> Site 15 is differently classified



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
