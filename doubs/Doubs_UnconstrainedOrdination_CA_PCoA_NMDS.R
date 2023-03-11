# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
rm(list=ls())
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "vegan", "ape", "ade4")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Doubs and mite
#
# mite
#  - Substrate (7 classes),  Shrubs (3 classes), and Microtopography (2 classes)
# ------------------------------------------------------------------------------
load("./RefData/NumericalEcologyWithR/NEwR2-Data/Doubs.RData")

car::some(spe)
car::some(env)
car::some(spa)
car::some(fishtraits)
car::some(latlong)


# remove site 8, which have zero species
spe <- spe[-8,]
env <- env[-8,]
spa <- spa[-8,]
# latlong <- latlong[-8,]



# ---------------------------------------------
# Load the oribatid mite data
load("./RefData/NumericalEcologyWithR/NEwR2-Data/mite.RData")
car::some(mite)
car::some(mite.env)
car::some(mite.xy)



# ------------------------------------------------------------------------------
# Correspondence Analysis (CA)
#  - CA of the raw species dataset (original species abundances)
#
#  - Chi-square is not influenced by double zeros; it is an asymmetrical D function. Therefore, CA is a method adapted to the analysis of species abundance data
#   without pre-transformation.
#  - Note that the data submitted to CA must be frequencies or frequency-like, dimensionally homogenous and non-negative;
#   that is the case of species counts, biomasses, or presence-absence data.
#  - In CA both the objects and the species are generally represented as points in the same joint plot --> most useful in ecology.
# 
#  - CA scaling 1:  rows (sites) are at the centroids of columns (species), appropriate if primarily interested in the ordination of OBJECTS (SITES)
#      (1) OBJECT (SITES) points that are close to one another are likely to be fairly similar in their species relative frequencies
#      (2) Any object found near the point representing a species is likely to contain a high contribution of that species.
#          For presence-absence data, the object is more likely to possess the state "1" for that species.
#  - CA scaling 2:  columns (species) are at the centroids of rows (sites), appropriate if primarily interested in the ordination of SPECIES
#      (1) SPECIES points that are close to one another are likely to be fairly similar relative frequencies along the objects (sites)
#      (2) Any species found near the point representing an object (site) is more likely to be found in that object (site) or
#          to have a higher frequency there than in objects that are further away in the joint plot.
#
#  - In ecology, almost exclusively used to analyze community composition data.
# ------------------------------------------------------------------------------
# Compute CA:  NOTE that we do not need to pre-transform species abundance data
( spe.ca <- vegan::cca(spe) )
summary(spe.ca)		# default scaling 2
summary(spe.ca, scaling = 1)



# ---------------------------------------------
# Scree plot and broken stick model using vegan's screeplot.cca()
# 1st axis has a large eigenvalue.  In CA, values over 0.6 indicate a very srong gradient in the data.
# The eigenvalues are the same in both scalings.  The scaling affects the eigenvectors to be drawn but not the eigenvalues.
# the 1st axis is extremely dominant.
graphics.off();  par(mfrow=c(1,1));
screeplot(spe.ca, bstick = TRUE, npcs = length(spe.ca$CA$eig))



# ---------------------------------------------
# CA biplots:  the 1st axis is extremely dominant.
# Scaling 1: sites are centroids of species
# Scaling 2 (default): species are centroids of sites
# 
# 1st axis opposes the lower section of teh stream (sites 19-30) to the upper portion.
# Many species appear close to the sites 19-30, indicating that they are more abundant downstream. Many of them are actually absent from the upper part of the river.
# The second axis contrasts the 10 upstream sites with the intermediate ones. Both groups of sites, which display short gradients on their own, are associated with characteristic species.
#
# The scaling 2 biplot shows how small groups of species are distributed among the sites.
# The grayling (Thth), the bullhead (Cogo) and teh varione (Teso) are found in the intermediate group of sites (11-18), while the brown trout (Satr), 
# the Eurasian minnow (Phph) and the stone loach (Babl) are found in a longer portion of the stream (approximately sites 1-18)
#
# In both of biplot 1 nad 2, interpretation of the species found near the origin of the graph should be done with care.
# This proximity could mean either that the species is at its optimum in the mid-range of the ecological gradients represented by the axes,
# or that it is present everywhere along the gradient.
par(mfrow = c(1, 2))
plot(spe.ca, scaling = 1, main = "CA fish abundances - biplot scaling 1")
plot(spe.ca, main = "CA fish abundances - biplot scaling 2")



# ------------------------------------------------------------------------------
# Projection of supplementary sites in a CA
# ------------------------------------------------------------------------------
# Projection of supplementary sites in a CA - scaling 1
# Data set with 3 sites removed
graphics.off();  par(mfrow=c(1,1));
sit.small <- spe[-c(7, 13, 22), ]
sitsmall.ca <- vegan::cca(sit.small)
plot(sitsmall.ca, display = "sites", scaling = 1)


# Passive projection of these removed 3 sites
newsit3 <- spe[c(7, 13, 22), ]
ca.newsit <- predict(sitsmall.ca, newsit3, type = "wa", scaling = 1)
text(ca.newsit[, 1], ca.newsit[, 2], labels = rownames(ca.newsit), cex = 0.8, col = "blue")


# Projection of supplementary species in a CA - scaling 2
# Data set with 3 species removed
spe.small <- spe[, -c(1, 3, 10)]
spesmall.ca <- vegan::cca(spe.small)
plot(spesmall.ca, display = "species", scaling = 2)


# Passive projection of these removed 3 sites
newspe3 <- spe[, c(1, 3, 10)]
ca.newspe <- predict(spesmall.ca, newspe3, type = "sp",  scaling = 2)
text(ca.newspe[, 1], ca.newspe[, 2], labels = rownames(ca.newspe), cex = 0.8, col = "blue")



# ------------------------------------------------------------------------------
# Post-hoc curve fitting of environmental variables
#  - Examine how selected environmental variables are connected to the ordination result, but on a broader, non-linear basis.
#  - vegan::ordisurf() fits smoothed two-dimensional splines by means of GAM (generalized additive models)
# ------------------------------------------------------------------------------
# Curve fitting in a CA biplot
# water discharge (dis) fitted surface is strongly nonlinear, whereas the ammonium concentration (amm) fitted surface is made of straight, parallel lines, indicating a linear fit.
graphics.off();  par(mfrow=c(1,1));
plot(spe.ca, main = "CA fish abundances - scaling 2", sub = "Fitted curves: discharge (red), ammonium (green)")
spe.ca.env <- envfit(spe.ca ~ dis + amm, env)
plot(spe.ca.env)  # Two arrows
vegan::ordisurf(spe.ca, env$dis, add = TRUE)
vegan::ordisurf(spe.ca, env$amm, add = TRUE, col = "green")



# ------------------------------------------------------------------------------
# Reordering the data table on the basis of an ordination axis
# ------------------------------------------------------------------------------
vegan::vegemite(spe, spe.ca)


# CA-ordered species table illustrated as a heat map
# Ordering is not optimal since it is done only on the basis of the 1st CA axis.
# Therefore sites 1-10 and 11-18 (separated along axis 2) and their corresponding characteristic species are interspersed.
vegan::tabasco(spe, spe.ca)



# ------------------------------------------------------------------------------
# Simple CA function
#  - a quick assessment of the structure of data
#  - also, display the cumulative fit of species and sites in terms of R^2, helping to identify the axes to which the species or sites contribute the most.
# ------------------------------------------------------------------------------
graphics.off()
source("./RefData/NumericalEcologyWithR/NEwR2-Functions/CA.newr.R")


# CA using CA.newr() function
spe.CA.PL <- CA.newr(spe)
par(mfrow = c(1, 2))
biplot.CA(spe.CA.PL, scaling = 1, cex = 1)
biplot.CA(spe.CA.PL, scaling = 2, cex = 1)


# Ordering of the data table following the first CA axis
# The table is transposed, as in the vegemite() output
summary(spe.CA.PL)
t(spe[order(as.vector(spe.CA.PL$scaling1$sites[, 1])), 
      order(as.vector(spe.CA.PL$scaling1$species[, 1]))])


# Cumulative fit of species
# for instance, Phph is well fitted by axis 1 only 0.865 where as Cogo is very well fitted by axes 1 and 2 together (0.914).
# On the other hand, Lele has no contribution to axes 1 and 2 but gets half of its fit (0.494) on axis 3.
spe.CA.PL$fit$cumulfit.spe


# Cumulative fit of sites
spe.CA.PL$fit$cumulfit.obj



# ------------------------------------------------------------------------------
# Multiple correspondence analysis (MCA)
#  - MCA on the oribatid mite environmental data set
#  - function MCA() offers the possibility of projecting supplementary variables into the MCA result. Note that these supplementary variables are not involved
#   in the computation of the MCA itself, they are added to the result for heuristic prurposses only.
#
# ------------------------------------------------------------------------------
# Preparation of supplementary data: mite classification into 4 groups
# Hellinger transformation of oribatid mite species data
mite.h <- decostand(mite, "hel")

# Ward clustering of mite data
mite.h.ward <- hclust(dist(mite.h), "ward.D2")

# Cut the dendrogram to 4 groups
mite.h.w.g <- cutree(mite.h.ward, 4)

# Assembly of the data set
mite.envplus <- data.frame(mite.env, mite.h.w.g)



# MCA of qualitative environmental data plus supplementary variables
#   (1) quantitative environmental data, 
#   (2) 4-group mite classification.
#   Default: graph=TRUE.
#
# 2 axes together represent 32.33% of the inertia of the dataset.
# (a) sites
# (b) levels (modalities) of the variables involved in the analysis and of the qualitative supplementary variable (groups 1 to 4 marked by green triangles)
# (c) R2 of the involved and supplementary variables with axes 1 and 2
# (d) projection of the supplementary quantitative variables
#
# Site 11 is isolated from all others at the top ofthe graph, the only one that contains Sphagnum group 3
# Pairs of sites 44 + 57 (left) and 21 + 26 (right) stand also relatively apart.
# The 44 + 57 pair shares the "Barepeat" modality and the pair 21 + 26 shares the "Sphagn4" modality. These 2 modalities have only 2 occurrences in the data set.
# These results show that MCA shares with CA the property of emphasizing rare events, which may be useful to identify rare characteristics or otherwise special features in a qualitative dataset.
# The sites that are closest to the forest (smal numbers) are on the right, and the sites that are clsest tot the free weter (large numbers) are on the left.
#
# (c) represents the squared correlation of the variables with the ordination axes. This graph can be used to identify the variables that are most related to the axes.
# Variable "Topo" (blanket vs hummock) has an R^2 = 0.7433 on axis 1 but only 0.0262 on axis 2 (mite.env.MCA$var$eta2)
graphics.off();  par(mfrow=c(2,2))
mite.env.MCA <- FactoMineR::MCA(mite.envplus, quanti.sup = 1:2, quali.sup = 6)
mite.env.MCA


# (c) the squared correlation of the variables with ordination axes.
mite.env.MCA$var$eta2


# The contributions of the modalities to the axes.
# On axis 1, the highest contributions are those of modalities Hummock, Shurub-None and Blanket
# On axis 2, Shurubs-Many, Sphagn3 and Srubs-Few stand out. This points to the great importance of the 2 variables "Topo", and "Shrub".
mite.env.MCA$var$contrib


# Contingency table crossing variables "Shrub" and "Topo"
# The table shows that sites where shrubs are present (modalities "Few" and "Many") are rather evenly represented on blankets and Hummocks, 
# but all sites devoid of shrubs have blanket-type soil coverage.
table(mite.env$Shrub, mite.env$Topo)



# ------------------------------------------------------------------------------
# Principal coordinate analysis (PCoA)
#  - PCoA provides a Eucliean representation of a set of objects whose relationships are measured by any dissimilarity measure chosen by the user.
#
#  - devoted to the ordnatio of dissimilarity matrices, most often in the Q-mode, instead of site-by-variabels tables.
#  - Great flexibility in the choice of association measures.
#  - The ordination axes of a PCoA can be interpreted like those of a PCA or CA: proximity of objects in the ordination represents their similarity in the sense of the association measure used.
#
#  - In the case of association measures that have the Euclidean property, PCoA behaves in a Euclidean manner. For instance, computing a Euclidean distance among sites
#    and running a PCA on a covariance matrix of the same data and looking at the scaling 1 ordination biplot.
#  - But if the association coefficient used is non-Euclidean, then PCoA may react by producing several negative eigenvalues in addition to the positive ones,
#    plus a null eigenvalue in-between.
#  - Technical solutions to this problem
#     - adding a constant to the squared dissimilarities among objects (Lingoes correction):  preferable since it produces a test with correct type I error.
#     - adding a constant to the dissimilarities themselves (Cailliez correction):  it produces a test with slightly inflated rate of type I error.
#  - cmdscale():  Cailliez correction is obtained with the argument add = TRUE
#  - vegan::wcmdscale():  offers a weighted version of PCoA and Lingoes and Cailliez correction are available through argument add = "lingoes" (default) or "cailliez"
#  
#  - Applications to ecological anlaysis
#     - PCoA produces ordinations of the objects in reduced 2-D or 3-D space.
#     - PCoA can also act as a data transformation after computation of an appropriately chosen dissimilarity measure.
#       The coordinates of the objects in full-dimensional PCoA space represent the transformed data. They can be used as starting point for new analyses,
#       for example db-RDA or k-means partitioning.
# 
# ------------------------------------------------------------------------------
# Use percentage difference dissimilarity (not Euclidean) matrix of fish species
spe.bray <- vegan::vegdist(spe)



# ---------------------------------------------
# PCoA without or with argument add = TRUE (Cailliez correction)
spe.b.pcoa <- cmdscale(spe.bray, k = (nrow(spe) - 1), eig = TRUE)
spe.b.pcoa_cailliez <- cmdscale(spe.bray, k = (nrow(spe) - 1), eig = TRUE, add = TRUE)


# Without Cailliez correction, only first 17 eigenvalues are positive
# With Cailliez correction, all eigenvalues are positive
spe.b.pcoa$eig
spe.b.pcoa_cailliez$eig



# ---------------------------------------------
# weighted average projection of species
# Since species are projected as weighted averages o their contributions to the sites, their interpretation with respect to the sites is akin to that of a CA with scaling 2.
spe.wa <- vegan::wascores(spe.b.pcoa$points[, 1:2], spe)



# ---------------------------------------------
# A posteriori projection of environmental variables
( spe.b.pcoa.env <- envfit(spe.b.pcoa, env) )



# ---------------------------------------------
# Plot of the scores of site --> weighted average projection of species --> a posteriori projection of environmental variables
# Don't worry about the warnings issued by R and concerning the species scores.
graphics.off();  par(mfrow=c(1,2));

ordiplot(scores(spe.b.pcoa, choices = c(1, 2)), type = "t", main = "PCoA with species weighted averages")
abline(h = 0, lty = 3)
abline(v = 0, lty = 3)
text(spe.wa, rownames(spe.wa), cex = 0.7, col = "red")
# Plot significant variables with a user-selected colour
plot(spe.b.pcoa.env, p.max = 0.05, col = 3)



# ------------------------------------------------------------------------------
# Another way to draw a double projection
#  - Based on correlations of the environmental variables with the PCoA ordination axes
#  - A PCoA computed on a matrix of Euclidean distances produces eigenvectors corresponding to what would be obtained in a scaling 1 PCA biplot of the same data
# ------------------------------------------------------------------------------
spe.h.pcoa <- ape::pcoa(dist(spe.h))
spe.std <- scale(spe.h)



# ---------------------------------------------
graphics.off();  par(mfrow = c(1, 2));

# First biplot: Hellinger-transformed species data on PCoA ordination axes
# It is important to use the species data with the same transformation (if any) as the one used to compute the dissimilarity matrix
# dir.axis1 = -1 to revert axis 1 for the projection of points and variables (default value = +1) to make the result directly comparable with the PCA result in scaling 1
ape::biplot.pcoa(x = spe.h.pcoa, Y = spe.h, dir.axis1 = -1)
abline(h = 0, lty = 3)
abline(v = 0, lty = 3)
text(-0.5, 0.45, "a", cex = 2)


# Second biplot: standardized Hellinger-transformed species data on PCoA ordination axes
# The standardization as an alternative may help in better visualizing the variables if they have very different variances.
ape::biplot.pcoa(x = spe.h.pcoa, Y = spe.std, dir.axis1 = -1)
abline(h = 0, lty = 3)
abline(v = 0, lty = 3)
text(-2.7, 2.45, "b", cex = 2)



# ---------------------------------------------
# Third biplot: standardized Hellinger-transformed species data on PCoA ordination axes:  only four species that gave their names to Verneaux's zones.
ape::biplot.pcoa(x = spe.h.pcoa, Y = spe.h[, c(2, 5, 11, 21)], dir.axis1 = -1)
abline(h = 0, lty = 3)
abline(v = 0, lty = 3)



# ---------------------------------------------
# Comparison of PCoA results with Euclidean and non-Euclidean dissimilarity matrices
# pcoa() function provides Lingoes and Cailliez corrections and also provides the eigenvalues along with a broken stick comparison in its output.

# PCoA on a Hellinger distance matrix
ade4::is.euclid(dist(spe.h))
summary(spe.h.pcoa)
head(spe.h.pcoa$values)


# PCoA on a percentage difference dissimilarity matrix
ade4::is.euclid(spe.bray)
spe.bray.pcoa <- pcoa(spe.bray)
head(spe.bray.pcoa$values)  # Observe eigenvalues 18 and following


# The easiest way of obtaining a fully Euclidean ordination solution:  take the square root of the percentage difference dissimilarity
ade4::is.euclid(sqrt(spe.bray))
spe.braysq.pcoa <- pcoa(sqrt(spe.bray))
head(spe.braysq.pcoa$values) 	# Observe the eigenvalues


# PCoA on a percentage difference dissimilarity matrix with Lingoes correction
# the 2nd column contains the corrected eigenvalues
spe.brayl.pcoa <- pcoa(spe.bray, correction = "lingoes")
head(spe.brayl.pcoa$values)	 # Observe the eigenvalues, col. 1 and 2


# PCoA on a percentage difference dissimilarity matrix with Cailliez correction
spe.brayc.pcoa <- pcoa(spe.bray, correction = "cailliez")
head(spe.brayc.pcoa$values)	 # Observe the eigenvalues, col. 1 and 2


# --> If you want to choose the analysis displaying the highest proportion of variation on axes 1+2, which solution will you select among those above ?



# ------------------------------------------------------------------------------
# Nonmetric multidimensional scaling (NMDS)
#  - PoCA find solution using eigen-decomposition of the transformed dissimilarity matrix, whereas NMDS find solution by an iterative approximation algorithm.
#  - Like PoCA, NMDS can produce ordinations of objects from any dissimilarity matrix.
#  - The method can cope with missing values, as long as there are enough measures left to position each object with respect to a few others.
#  - The result of NMDS may arbitrarily be rotated or inverted.
#  - NMDS is a computer-intensive iterative technique exposed to the risk of suboptimal solutions.
#
#  - Applications to ecological anlaysis
#     - When it is necessary to squeeze in two dimensions a PCA or PCoA solution that requires 3 or 4 dimenstions, 
#       NMDs is useful to represent well the main dissimilarity relationships among the sites in 2-D.
#       In such a case, it is preferable to use the PCA or PCoA ordination axes as iput into NMDS to make sure that the NMDS solution will not diverge markedly from the metric ordination.
#
#  - If the reseacher's priority is not to preserve the exact dissimilarities among objects in an ordination plot,
#    but rather to represent as well as possible the ordering relationships among objects in a small and specified number of axes, NMDS may be the solution.
#     
#  - vegan::metaMDS() accepts raw data or dissimilarity matrices
#  - MASS::isoMDS() accepts dissimilarity matrix with missing values
#  - lbdsv::bestnmds() is a wrapper for isoMDS() and reduces the risk of reaching a local minimum
# ------------------------------------------------------------------------------
# NMDS applied to the Doubs fish species - percentage difference dissimilarity matrix
spe.nmds <- vegan::metaMDS(spe, distance = "bray")
spe.nmds
spe.nmds$stress


graphics.off();  par(mfrow=c(1,1));
plot(spe.nmds, type = "t", main = paste("NMDS/Percentage difference - Stress =", round(spe.nmds$stress, 3)))


# Explanatory variables could also be added using envfit()



# ---------------------------------------------
# Compare the result of PCA, CA and PCoA plot
graphics.off();  par(mfrow=c(2,2));



# ---------------------------------------------
# Asess the appropriateness of an NMDS result
# Shapard diagram:  the dissimilarities among objects in the ordination plot with the original dissimilaritis
# The Goodness-of-fit of the ordination can be measured as the R^2 of either a linear or a non-linear regression of the NMDS distances on the original dissimilarities
# Poorly fitted sites have larger bubbles.
par(mfrow = c(1, 2))
vegan::stressplot(spe.nmds, main = "Shepard plot")
gof <- vegan::goodness(spe.nmds)
plot(spe.nmds, type = "t", main = "Goodness of fit")
points(spe.nmds, display = "sites", cex = gof * 300)



# ---------------------------------------------
# Add colours from a clustering result to an NMDS plot
# Ward clustering of percentage difference dissimilarity matrix and extraction of four groups
spe.bray.ward <- hclust(spe.bray, "ward.D") # Here better than ward.D2 for 4 groups
spe.bw.groups <- cutree(spe.bray.ward, k = 4)
grp.lev <- levels(factor(spe.bw.groups))



# Combination with NMDS result
sit.sc <- scores(spe.nmds)
graphics.off();  par(mfrow=c(1,1));
p <- ordiplot(sit.sc, type = "n", main = "NMDS/% difference + clusters Ward/% difference")
for (i in 1:length(grp.lev)) points(sit.sc[spe.bw.groups == i, ], pch = (14 + i), cex = 2, col = i + 1)
text(sit.sc, row.names(spe), pos = 4, cex = 0.7)
ordicluster(p, spe.bray.ward, col = "dark grey")
legend(locator(1), paste("Group", c(1:length(grp.lev))), pch = 14 + c(1:length(grp.lev)), col = 1 + c(1:length(grp.lev)), pt.cex = 2)



# ------------------------------------------------------------------------------
# The Code It Yourself Corner
# -----------------------------------------------------------------
# A simple function to perform PCA --> this function should give the exact same results as the function PCA.newr()
myPCA <- function(Y) {
  Y.mat <- as.matrix(Y)
  object.names <- rownames(Y)
  var.names <- colnames(Y)
  
  # Centre the data (needed to compute matrix F)
  Y.cent <- scale(Y.mat, center = TRUE, scale = FALSE)
  
  # Covariance matrix S
  Y.cov <- cov(Y.cent)
  
  # Eigenvectors and eigenvalues of S (Legendre and Legendre 2012, eq. 9.1 and 9.2)
  Y.eig <- eigen(Y.cov)
  
  # Copy the eigenvectors to matrix U (used to represent variables in scaling 1 biplots)
  U <- Y.eig$vectors
  rownames(U) <- var.names
  
  # Compute matrix F (used to represent objects in scaling 1 plots)
  F <- Y.cent %*% U			# eq. 9.4
  rownames(F) <- object.names
  
  # Compute matrix U2 (to represent variables in scaling 2 plots) eq. 9.8
  U2 <- U %*% diag(Y.eig$values ^ 0.5)
  rownames(U2) <- var.names
  
  # Compute matrix G (to represent objects in scaling 2 plots) eq. 9.14
  G <- F %*% diag(Y.eig$values ^ 0.5)
  rownames(G) <- object.names
  
  # Output of a list containing all the results
  result <- list(Y.eig$values, U, F, U2, G)
  names(result) <- c("eigenvalues", "U", "F", "U2", "G")
  result
}



# ---------------------------------------------
# PCA on fish species using hand-written function
fish.PCA <- myPCA(spe.h)
summary(fish.PCA)
fish.PCA$eigenvalues


# Eigenvalues expressed as percentages
( pv <- round(100 * fish.PCA$eigenvalues / sum(fish.PCA$eigenvalues), 2) )


# Alternate computation of total variation (denominator)
round(100 * fish.PCA$eigenvalues / sum(diag(cov(spe.h))), 2)


# Cumulative eigenvalues expressed as percentages
round(cumsum(100 * fish.PCA$eigenvalues / sum(fish.PCA$eigenvalues)), 2)


# Biplots
par(mfrow = c(1, 2))
# Scaling 1 biplot
biplot(fish.PCA$F, fish.PCA$U)
# Scaling 2 biplot
biplot(fish.PCA$G, fish.PCA$U2)


# Plots using generic R plot() function
par(mfrow = c(1, 2))


# Scaling 1
plot(fish.PCA$F[, 1], fish.PCA$F[, 2], asp = 1, main = "PCA scaling 1", xlab = paste("Axis 1 (", pv[1], "%)", sep = ""), ylab = paste("Axis 2 (", pv[2], "%)", sep = ""))
arrows(x0 = 0, y0 = 0, fish.PCA$U[, 1], fish.PCA$U[, 2], length = 0.1, col = "red")
text(fish.PCA$F[, 1], fish.PCA$F[, 2], labels = row.names(spe), pos = 3, cex = 0.8)
text(fish.PCA$U[, 1], fish.PCA$U[, 2], labels = colnames(spe), adj = c(-0.2, 0.2), col = "red", cex = 0.8)
abline(h = 0, lty = 3)
abline(v = 0, lty = 3)


# Scaling 2
plot(fish.PCA$G[, 1], fish.PCA$G[, 2], asp = 1, main = "PCA scaling 2", xlab = paste("Axis 1 (", pv[1], "%)", sep = ""), ylab = paste("Axis 2 (", pv[2], "%)", sep = ""))
arrows(x0 = 0, y0 = 0, fish.PCA$U2[, 1], fish.PCA$U2[, 2], length = 0.1, col = "red")
text(fish.PCA$G[, 1], fish.PCA$G[, 2], labels = row.names(spe), pos = 3, cex = 0.8)
text(fish.PCA$U2[, 1], fish.PCA$U2[, 2], labels = colnames(spe), col = "red", adj = c(-0.2, 0.2), cex = 0.8)
abline(h = 0, lty = 3)
abline(v = 0, lty = 3)

