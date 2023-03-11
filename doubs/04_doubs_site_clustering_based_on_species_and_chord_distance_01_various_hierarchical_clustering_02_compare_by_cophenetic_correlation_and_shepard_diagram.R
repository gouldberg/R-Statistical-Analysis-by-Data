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
# Comparing Hierarchical Clustering:  Cophenetic Correlation
#  - The cophenetic distance between 2 objects in a dendrogram is the distance where the 2 objects become members of the same group.
#  - Cophenetic correlation is computed between the original dissimilarity martix and the cophenetic matrix
#  - The method with the highest cophenetic correlation may be seen as the one that produces the clustering model that retains most of the information contained in the dissimilarity matrix.
#
#  - The cophenetic correlation cannot be tested for significance, since the cophenetic matrix is derived from the original dissimilarity matrix. The 2 sets of distances are not independent.
#  - The cophenetic correlation dependes strongly on the clustering method used, in addition to the data.
# ------------------------------------------------------------------------------

# Compute cophenetic correlation matrix
# Cophenetic correlations can also be computed using Spearman or Kendall correlation.

( spe.ch.single.coph <- stats::cophenetic(spe.ch.single) )

( spe.ch.complete.coph <- stats::cophenetic(spe.ch.complete) )

( spe.ch.UPGMA.coph <- stats::cophenetic(spe.ch.UPGMA) )

( spe.ch.ward.coph <- stats::cophenetic(spe.ch.ward) )

( spe.ch.beta2.coph <- stats::cophenetic(spe.ch.beta2) )



# ----------
# Cophenetic matrix is strongly correlated with original dissimilarity matrix
cor(spe.ch, spe.ch.ward.coph)

cor(spe.ch, spe.ch.ward.coph, method = "spearman")



# ------------------------------------------------------------------------------
# Shepard-like diagrams comparing chord distances (species data) to cophenetic distance
# ------------------------------------------------------------------------------

par(mfrow=c(2,2))

plot(spe.ch, spe.ch.single.coph, xlab = "Chord distance", ylab = "Cophenetic distance", asp = 1, xlim = c(0, sqrt(2)), ylim = c(0, sqrt(2)), 
     main = c("Single linkage", paste("Cophenetic correlation = ", round(cor(spe.ch, spe.ch.single.coph), 3))))
abline(0, 1)
lines(lowess(spe.ch, spe.ch.single.coph), col = "red")


plot(spe.ch, spe.ch.complete.coph, xlab = "Chord distance", ylab = "Cophenetic distance", asp = 1, xlim = c(0, sqrt(2)), ylim = c(0, sqrt(2)), 
     main = c("Complete linkage", paste("Cophenetic correlation = ", round(cor(spe.ch, spe.ch.complete.coph), 3))))
abline(0, 1)
lines(lowess(spe.ch, spe.ch.complete.coph), col = "red")


plot(spe.ch, spe.ch.UPGMA.coph, xlab = "Chord distance", ylab = "Cophenetic distance", asp = 1, xlim = c(0, sqrt(2)), ylim = c(0, sqrt(2)), 
     main = c("UPGMA", paste("Cophenetic correlation = ", round(cor(spe.ch, spe.ch.UPGMA.coph), 3))))
abline(0, 1)
lines(lowess(spe.ch, spe.ch.UPGMA.coph), col = "red")


plot(spe.ch, spe.ch.ward.coph, xlab = "Chord distance", ylab = "Cophenetic distance", asp = 1, xlim = c(0, sqrt(2)), ylim = c(0, max(spe.ch.ward$height)),
     main = c("Ward", paste("Cophenetic correlation = ", round(cor(spe.ch, spe.ch.ward.coph), 3))))
abline(0, 1)
lines(lowess(spe.ch, spe.ch.ward.coph), col = "red")


plot(spe.ch, spe.ch.beta2.coph, xlab = "Chord distance", ylab = "Cophenetic distance", asp = 1, xlim = c(0, sqrt(2)), ylim = c(0, max(spe.ch.beta2$height)),
     main = c("Beta-flexible", paste("Cophenetic correlation = ", round(cor(spe.ch, spe.ch.beta2.coph), 3))))
abline(0, 1)
lines(lowess(spe.ch, spe.ch.beta2.coph), col = "red")



# -->
# Single linkage:  cophenetic distances are always smaller than or equal to the original distances
# Complete linkage:  spalce-dilation effect, in which cophenetic distances can never be smaller than the original distances
# If the relationship between the original and cophenetic distances is curvillinear in the Shepard-like diagram, as it is the case of Single linkage or Complete linkage,
# a nonparametric correlation coefficient should be used.

# UPGMA is well linearly related to the original distances


