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
# Functions for showing
#  - Site map
#  - Species richness vs Upstream-Downstream gradient + Map of Species Richness
# ------------------------------------------------------------------------------

plot_sitemap <- function(spa){
  par(mfrow=c(1,1));
  plot(spa, asp = 1, type = "n", main = "Site Locations", xlab = "x coordinate (km)", ylab = "y coordinate (km)")
  lines(spa, col = "light blue")
  text(spa, row.names(spa), cex = 1, col = "red")
  text(68, 20, "Upstream", cex = 1.2, col = "red")
  text(15, 35, "Downstream", cex = 1.2, col = "red")
}


plot_spe_richness <- function(spe, spa){
  sit.pres <- apply(spe > 0, 1, sum)
  sort(sit.pres)
  par(mfrow = c(1, 2))
  plot(sit.pres, type = "s", las = 1,  col = "gray", main = "Species Richness vs. \n Upstream-Downstream Gradient", xlab = "Site numbers",  ylab = "Species richness")
  text(sit.pres, row.names(spe), cex = .8, col = "red")
  plot(spa, asp = 1, main = "Map of Species Richness", pch = 21, col = "white", bg = "brown", cex = 5 * sit.pres / max(sit.pres), xlab = "x coordinate (km)", ylab = "y coordinate (km)")
  lines(spa, col = "light blue")
}



# ------------------------------------------------------------------------------
# chord distance among sites (Q-mode):  Euclidean distance computed on site vectors normalized to length 1
# ------------------------------------------------------------------------------
# Compute matrix of chord distance among sites
# Chord distance
#   - widely used to analyse community composition data and genetics
#   - has a maximum value of sqrt(2) for sites with no species in common and a minimum of zero when 2 sites share the same species in the same proportions of the site vector lengths,
#     without it being necessary for these species to be represented by the same snumbers of individuals at 2 sites.
#   - This measure is the Euclidean.
#   - After normalization, the Euclidean distance computed between 2 objects (sites) is equivalent to the length of a chord joining 2 points
#     within a segment of a sphere or hypersphere of radius 1.
#   - If there are only 2 species involved, the normalization places the sites on the circumference of a 90 degree sector of a circle with radius 1 ( = sqrt(2))
#   - Chord Distance = sqrt(2 * (1 - cos(theta)))   theta:  angle (theta) between the 2 site vectors
#   - This measure solves the problem caused by sites having different total abundances of species as well as the paradox by simple metric of Euclidean space.



spe.norm <- vegan::decostand(spe, "normalize")

apply(spe.norm^2, 1, sum)



# ----------
spe.ch <- vegan::vegdist(spe.norm, "euc")


# Attach site names to object of class "dist"
attr(spe.ch, "labels") <- rownames(spe)



# ------------------------------------------------------------------------------
# Site clustering based on species chord distance (Hierarchical Clustering)
# ------------------------------------------------------------------------------
# Single Linkage Agglomerative Clustering
#  - Also called nearest neighbour sorting, this method agglomerates objects on the basis of their shortest pairwise dissimilarities (or greatest similarities)
#  - the fusion of an object (or a group) with a group at a given similarity (or dissimilarity) level only requires that one object of each of the two groups about to agglomerate
#    be linked to one another at that level.
#  - Due to chaining of objects, the interpretation is difficult, but gradients are revealed quite clearly
#  - This clustering method contracted the reference space.
#  - may produce elongated clusters with loose chaining.


spe.ch.single <- hclust(spe.ch, method = "single")

graphics.off();  par(mfrow=c(1,1));

plot(spe.ch.single, labels = rownames(spe), main = "Site clustering \n based on species chord distance (single linkage)")



# ------------------------------------------------------------------------------
# Complete Linkage Agglomerative Clustering
#  - Also called furthest neighbour sorting, this method allows an object (or a group) to agglomerate with another group only at the dissimilarity corresponding to that of
#   the most distant pair of objects
#  - This clustering method dilates the reference space in the neighbourhood of that cluster.  In reference space, this method produces maximally linked and rather spherical clusters.
#  - This method is often desirable in ecology, when one wishes to delineate clusters with clear discontinuities.
# ------------------------------------------------------------------------------

spe.ch.complete <- hclust(spe.ch, method = "complete")

par(mfrow=c(1,1))

plot(spe.ch.complete, labels = rownames(spe), main = "Site clustering \n based on species chord distance (complete linkage)")



# ------------------------------------------------------------------------------
# Average Agglomerative Clustering:  UPGMA (Unweightd pair-group method using arithmetic averages)
#  - This family comprise 4 methods that are based on average dissimilarities among objects or on centroids of clusters.
#  - This method allows on object to join a group at the mean of the dissimilarities between this object and all members of the group.
#  - The methods of this family conserve the metric properties of reference space.
#  - The best-known method of this family, UPGMA, allows an object to join a group at the mean of the dissimilarities between this object and all members of the group.
#  - Because it gives equal weights to the original distances, the UPGMA method assumes that the objects in each group from a representative sample of the corresponding larger groups
#   of objects in the reference population under study. For that reason UPGMA clustering should only be used in connection with simple random or systematic sampling designs
#   if the resutls are to be extrapolated to a larger reference population.
#  - When the branching pattern of the dendrogram displays asymmetry (many more objects in one branch than in the other), this can be attributed to the structure of the reference
#   population if the sampling design was random.
# ------------------------------------------------------------------------------

spe.ch.UPGMA <- hclust(spe.ch, method = "average")

par(mfrow=c(1,1))

plot(spe.ch.UPGMA, labels = rownames(spe), main = "Site clustering \n based on species chord distance (UPGMA)")



# ------------------------------------------------------------------------------
# Ward's Minimum Variance Clustering
#  - This method is based on the linear model criterion of least squares. The objective is to define groups in such a way that the within-group sum of squares is minimized.
#  - The within-cluster sum of squared erroes can be computed as the sum of the squared distances among members of a cluster divided by the number of objects.
#  - Note also that although the computation of within-group sum-of-squares is based on a Euclidean model, the Ward method will produce meaningful results
#    from dissimilarities that are Euclidean or not.
#  - Because the Ward method minimizes the sum of within-group sums of squares (squared error criterion), the clusters tend to be hyperspherical, i.e. sphercial in multidimensioanl space,
#   and to contain roughly equal numbers of objects if the observations are evenly distributed through space.
# ------------------------------------------------------------------------------

spe.ch.ward <- hclust(spe.ch, method = "ward.D2")

par(mfrow=c(1,1))

plot(spe.ch.ward, labels = rownames(spe), main = "Site clustering \n based on species chord distance (ward.D2)")



# ------------------------------------------------------------------------------
# Flexible Clustering
#  - Lance and Williams proposed a model encompassin all the clustering methods above, which are obtained by chaing the values of 4 parameters (alpha-h, alpha-i, beta, gamma)
#  - beta-flexible clustering:  flexibility is commanded by the value of parameter beta
#  - agnes() considers the value of alpha_h in a context where alpha_h = alpha_i = (1-beta)/2 and gamma = 0
# ------------------------------------------------------------------------------

beta <- -0.25

alpha_h <- (1-beta)/2

spe.ch.beta2 <- cluster::agnes(spe.ch, method = "flexible", par.method = alpha_h)

spe.ch.beta2 <- as.hclust(spe.ch.beta2)



# -----------
beta <- -0.1

alpha_h <- (1-beta)/2

spe.ch.beta3 <- cluster::agnes(spe.ch, method = "flexible", par.method = alpha_h)

spe.ch.beta3 <- as.hclust(spe.ch.beta3)



# ----------
par(mfrow=c(1,2))

plot(spe.ch.beta2, labels = rownames(spe), main = "Site clustering \n based on species chord distance (Beta-flexible) \n beta = -0.25")

plot(spe.ch.beta3, labels = rownames(spe), main = "Site clustering \n based on species chord distance (Beta-flexible) \n beta = -0.1")



# ------------------------------------------------------------------------------
# Compare Single Linkage method, Complete Linkage method and UPGMA (Average Agglomerative Clustering:  Unweightd pair-group method using arithmetic averages)
# Single Linkage
#  - "closest friend" procedure does not always show clearly separated groups, but can be used to identify gradients in the data.
# Complete Linkage
#  - A group admits a new member only at a dissimilarity corresponding to the furthest object of the group
#  - The admission requires unanimity of the members of the group
#  - The larger a group is, the more difficult it is to agglomerate with it. This method tends to produce many small separate groups, rather spherical in multivariate space.
#  - Used to search for and identify discontinuities in data.
# UPGMA (Average Agglomerative Clustering:  Unweightd pair-group method using arithmetic averages)
#  - The result of UPGMA often looks somewhat intermediate between a single and a complete linkage clustering
# ------------------------------------------------------------------------------


# Both of single linkage and complete linkage methods grouped site 23,24 and 25 in one group: closest among each other and furthest from other groups
# Complete linkage grouped site 23,24 and 25 with other downstream sites (20 to 30) at higher level.
graphics.off();  par(mfrow=c(2,2));

plot(spe.ch.single, labels = rownames(spe), main = "Site clustering \n based on species chord distance (single linkage)")

plot(spe.ch.complete, labels = rownames(spe), main = "Site clustering \n based on species chord distance (complete linkage)")

plot(spe.ch.UPGMA, labels = rownames(spe), main = "Site clustering \n based on species chord distance (UPGMA)")



# ----------
# for check sitemap and species richness
plot_sitemap(spa)

plot_spe_richness(spe = spe, spa = spa)



# ------------------------------------------------------
# Compare Ward's minimum variance clustering and Flexible clustering
# ------------------------------------------------------

par(mfrow=c(1,2))

plot(spe.ch.ward, labels = rownames(spe), main = "Site clustering \n based on species chord distance (ward.D2)")

plot(spe.ch.beta2, labels = rownames(spe), main = "Site clustering \n based on species chord distance (Beta-flexible)")



# -->
# The result of flexible clustering is completely same with Ward's method (except the height value)





