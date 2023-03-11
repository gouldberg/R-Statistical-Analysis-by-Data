# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
rm(list=ls())
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "vegan", "ade4", "adespatial", "gclus", "cluster", "FD")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  Doubs
# ------------------------------------------------------------------------------
load("./RefData/NumericalEcologyWithR/NEwR2-Data/Doubs.RData")


# Remove empty site 8
spe <- spe[-8, ]
env <- env[-8, ]
spa <- spa[-8, ]



# ------------------------------------------------------------------------------
# Source additional function
# ------------------------------------------------------------------------------
source("./RefData/NumericalEcologyWithR/NEwR2-Functions/coldiss.R")

# Usage:
# coldiss(D = dissimilarity.matrix,
#         nc = 4,
#         byrank = TRUE,
#         diag = FALSE)
# If D is not a dissimilarity matrix (max(D) > 1), then D is divided by max(D)
# nc number of colours (classes)
# byrank =  TRUE	equal-sized classes
# byrank =  FALSE	equal-length intervals
# diag = TRUE	print object labels also on the diagonal

# A reordering feature is included in coldiss(), which uses the function order.single() of the gclus packages to reorder each dissimilarity matrix,
# so that similar sites are displayed close together along the main diagonal.


source("./RefData/NumericalEcologyWithR/NEwR2-Functions/panelutils.R")

# pairs() is a function to plot a matrix of bivariate scatter plots.
# panelutils.R is a set of functions that add useful features to pairs():
# upper.panel = panel.cor: to print correlation coefficients in the
# upper panel, with significance levels;
# diag.panel = panel.hist: to plot histograms of the variables in the diagonal.
# Specify method for the choice of the correlation coefficient:
# by default, method = "pearson", other choices are "spearman" and "kendall".
# To get a plot in grey tones instead of colors, use no.col = TRUE and lower.panel = panel.smoothb.


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
# Q-mode:  Computing dissimilarity matrices among objects
# (semi-)quantitative species data
# ------------------------------------------------------------------------------
# Percentage difference (aka Bray-Curtis) dissimilarity matrix on raw species data
#  - gives same importance to absolute differences in abundance irrespective of the other of magnitude of the abundances
# method = "bray" (default)
# ".db" means "distance Bray".
( spe.db_v <- vegan::vegdist(spe) )


# True abundances are often log-transformed
# Percentage difference (aka Bray-Curtis) dissimilarity matrix on log-transformed abundances
( spe.dbln_v <- vegan::vegdist(log1p(spe)) )


# Compare dissimilarities
#  - in stats, vegan, the conversion from similarities S to dissimilaritis D is done by computing D = 1 - S
round(head(spe.db_v, 20), 3)
round(head(spe.dbln_v, 20), 3)


# plot dissimilarity matrix
coldiss(spe.db_v, byrank = FALSE, diag = TRUE)
coldiss(spe.dbln_v, byrank = FALSE, diag = TRUE)



# ------------------------------------------------------
# Chord distance matrix
#  - Euclidean distance computed on site vectors normalized to length 1
spe.dc_ades <- adespatial::dist.ldc(spe, "chord")


# Alternate, two-step computation in vegan:
spe.norm <- vegan::decostand(spe, "nor")
spe.dc_vd <- dist(spe.norm)


# Compare dissimilarities calculated by different packages
#  - in stats, vegan, the conversion from similarities S to dissimilaritis D is done by computing D = 1 - S
round(head(spe.dc_vd, 20), 3)
#  - in adespatial for most similarity coeffs are converted as D = 1 - S (except Jaccard, Sorencsen and Ochiai)
round(head(spe.dc_ades, 20), 3)


# plot dissimilarity matrix
coldiss(spe.dc_vd, byrank = FALSE, diag = TRUE)



# ------------------------------------------------------
# Hellinger distance matrix
#  - Euclidean distance between site vectors where the abundance values are first divided by the site total abundance, 
#   and the result is square-root transformed. (Hellinger transformation)
#  - Hellinger distance is the chord transformation of square-root transformed abundance data.
# By dist.ldc(), Hellinger is default distance
spe.dh_ades <- adespatial::dist.ldc(spe)


# Alternate, two-step computation in vegan:
spe.hel <- vegan:: decostand(spe, "hel")
spe.dh_vd <- dist(spe.hel)


# Compare dissimilarities calculated by different packages
#  - in stats, vegan, the conversion from similarities S to dissimilaritis D is done by computing D = 1 - S
round(head(spe.dh_vd, 20), 3)
#  - in adespatial for most similarity coeffs are converted as D = 1 - S (except Jaccard, Sorencsen and Ochiai)
round(head(spe.dh_ades, 20), 3)


# plot dissimilarity matrix
coldiss(spe.dh_vd, byrank = FALSE, diag = TRUE)



# ------------------------------------------------------
# Log-chord distance matrix
#  - Chord distance applied to log-transformed abundance data.
#  - Obtained by first transforming the raw abundance data by ln(y+1), followed by the computation of the chord transformation or the chord distance
spe.logchord_ades <- adespatial::dist.ldc(spe, "log.chord")


# Alternate, three-step computation in vegan:
spe.ln <- log1p(spe)
spe.ln.norm <- vegan::decostand(spe.ln, "nor")
spe.logchord_vd <- dist(spe.ln.norm)


# Compare dissimilarities calculated by different packages
#  - in stats, vegan, the conversion from similarities S to dissimilaritis D is done by computing D = 1 - S
round(head(spe.logchord_vd, 20), 3)
#  - in adespatial for most similarity coeffs are converted as D = 1 - S (except Jaccard, Sorencsen and Ochiai)
round(head(spe.logchord_ades, 20), 3)


# plot dissimilarity matrix
coldiss(spe.logchord_vd, byrank = FALSE, diag = TRUE)


# ------------------------------------------------------
# Compare dissimilarities:
# - Percentage Difference (Bray-Curtis), Percentage Difference based on log-transformed abundance data
# - Chord Distance
# - Hellinger Distance
# - Log-Chord Distance
round(head(spe.db_v, 30), 3)
round(head(spe.dbln_v, 30), 3)
round(head(spe.dc_vd, 30), 3)
round(head(spe.dh_vd, 30), 3)
round(head(spe.logchord_vd, 30), 3)


# Compare the two percentage difference plots. The differences are due to the log transformation.
# In the untransformed dissimilarity matrix, small differences in abundant species have the same importance as small differences in species with few individuals
graphics.off();  par(mfrow=c(1,1));
coldiss(spe.db_v, byrank = FALSE, diag = TRUE)
coldiss(spe.dbln_v, byrank = FALSE, diag = TRUE)



#  
coldiss(spe.dc_vd, byrank = FALSE, diag = TRUE)
coldiss(spe.dh_vd, byrank = FALSE, diag = TRUE)
coldiss(spe.logchord_vd, byrank = FALSE, diag = TRUE)



# ------------------------------------------------------------------------------
# Q-mode:  Computing dissimilarity measures for binary data
# Binary (Presence-Absence) species data
#  - When the only data available are binary, or when the abundances are irrelevant, or sometimes, when the data table contains quantitative values of uncertain or unequal quality,
#   the analyses are done on presence-absence (1-0) data.
# ------------------------------------------------------------------------------
# Jaccard dissimilarity matrix
#  - Jaccard "similarity" is the ratio between the number of double 1's and the number of species, excluding the species represented by double zeros in the pair of objects considered.
#  - Jaccard similarity of 0.25 means that 25% of the total number of species observed at 2 sites were present in both sites and 75% in one site only.
#  - Jaccard dissimilarity computed in R is either (1-0.25) or sqrt(1-0.25) depending on the package used.

spe.dj_d <- dist(spe, "binary")
spe.dj_v <- vegan::vegdist(spe, "jac", binary = TRUE)
spe.dj_ade4 <- ade4::dist.binary(spe, method = 1)
spe.dj_ades <- adespatial::dist.ldc(spe, "jaccard")


# Compare dissimilarity
#  - in stats, vegan, the conversion from similarities S to dissimilaritis D is done by computing D = 1 - S
round(head(spe.dj_d, 20), 3)
round(head(spe.dj_v, 20), 3)
#  - in adespatial for Jaccard, Sorensen and Ochiai, conversion from similarities S to dissimilaritis D is done by computing D = sqrt(1 - S)
round(head(spe.dj_ades, 20), 3)
round(head(spe.dj_ades^2, 20), 3)
#  - in ade4, conversion from similarities S to dissimilaritis D is done by computing D = sqrt(1 - S)
round(head(spe.dj_ade4, 20), 3)
round(head(spe.dj_ade4^2, 20), 3)


# ------------------------------------------------------
# Sorensen dissimilarity matrix
#  - Sorensen "similarity" gives double weight to the number of double 1's
#  - Its reciprocal (complement to 1) is equivalent to the percentage difference (aka Bray-Curtis) dissimilarity computed on species presence-absence data

spe.ds_v <- vegan::vegdist(spe, method = "bray", binary = TRUE)
spe.ds_ades <- adespatial::dist.ldc(spe, "sorensen")
spe.ds_ade4 <- ade4::dist.binary(spe, method = 5)


# Compare dissimilarity
#  - in stats, vegan, the conversion from similarities S to dissimilaritis D is done by computing D = 1 - S
round(head(spe.ds_v, 20), 3)
#  - in adespatial for Jaccard, Sorensen and Ochiai, conversion from similarities S to dissimilaritis D is done by computing D = sqrt(1 - S)
round(head(spe.ds_ades, 20), 3)
round(head(spe.ds_ades^2, 20), 3)
#  - in ade4, conversion from similarities S to dissimilaritis D is done by computing D = sqrt(1 - S)
round(head(spe.ds_ade4, 20), 3)
round(head(spe.ds_ade4^2, 20), 3)



# ------------------------------------------------------
# Ochiai dissimilarity matrix
#  - Computing either one of the chord, Hellingeer and log-chord distance on presence-absence data, followed by division by sqrt(2), produces sqrt(1 - Ochiai Similarity)

spe.och_ades <- adespatial::dist.ldc(spe, "ochiai")
spe.och_ade4 <- ade4::dist.binary(spe, method = 7)


# Compare dissimilarity
#  - in adespatial for Jaccard, Sorensen and Ochiai, conversion from similarities S to dissimilaritis D is done by computing D = sqrt(1 - S)
round(head(spe.och_ades, 20), 3)
round(head(spe.och_ades^2, 20), 3)
#  - in ade4, conversion from similarities S to dissimilaritis D is done by computing D = sqrt(1 - S)
round(head(spe.och_ade4, 20), 3)
round(head(spe.och_ade4^2, 20), 3)



# ------------------------------------------------------
# Compare dissimilarities:
# - Jaccard, Sorencen and Ochiai
round(head(spe.dj_ades, 30), 3)
round(head(spe.ds_ades, 30), 3)
round(head(spe.och_ades, 30), 3)

round(head(spe.dj_ades^2, 30), 3)
round(head(spe.ds_ades^2, 30), 3)
round(head(spe.och_ades^2, 30), 3)


# Compare Jaccard plot with abundance dissimilarity
# site 23,24,25 is not clustered by Jaccard Dissimilarity
# site 11-15 is more clearly clustered by Jaccard dissimilarity
graphics.off();  par(mfrow=c(1,1));
plot_sitemap(spa = spa)
plot_spe_richness(spe = spe, spa = spa)
coldiss(spe.dc_vd, byrank = FALSE, diag = TRUE)
coldiss(spe.dj_ades^2, byrank = FALSE, diag = TRUE)
# coldiss(spe.dj_ades, byrank = FALSE, diag = TRUE)
coldiss(spe.ds_ades^2, byrank = FALSE, diag = TRUE)
coldiss(spe.och_ades^2, byrank = FALSE, diag = TRUE)



# ------------------------------------------------------
# For comparison:  symmetrical simple matching dissimilarity:  called the Sokal and Michener index in ade4 (S1)
spe.s1_ade4 <- ade4::dist.binary(spe, method = 2)


# Compare Jaccard (assymmetrical) and simple matching dissimilarity (S1)
# Symmetrical S1 is affected by double zeros
graphics.off();  par(mfrow=c(1,1));
plot_sitemap(spa = spa)
plot_spe_richness(spe = spe, spa = spa)
coldiss(spe.dj_ades^2, byrank = FALSE, diag = TRUE)
coldiss(spe.s1_ade4^2, byrank = FALSE, diag = TRUE)



# ------------------------------------------------------------------------------
# Q-mode:  Quantitative data (excluding species abundances)
# Quick comparison of distance matrices from environmental, species and spatial data. 16 colours with equal-size classes
# ------------------------------------------------------------------------------
# Remove the 'dfs' variable from the env dataset
env2 <- env[, -1]


# scale
env.de <- dist(scale(env2))


# Compare
# - Euclidean distance matrix of the standardized environmental data and
# - Hellinger distance matrix of the species data
# baed on non-ordered matrix
graphics.off();  par(mfrow=c(1,1));
coldiss(env.de, nc = 16, diag = TRUE)
coldiss(spe.dh, nc = 16, diag = TRUE)



# ------------------------------------------------------
# Euclidean distance matrix on spatial coordinates (2D) and distance from the source (1D)
spa.de <- dist(spa)
dfs.df <- as.data.frame(env$dfs, row.names = rownames(env))
riv.de <- dist(dfs.df)


# Compare 2 distance matrices, based on 2D and 1D
coldiss(spa.de, nc = 16, diag = TRUE)
coldiss(riv.de, nc = 16, diag = TRUE)



# ------------------------------------------------------------------------------
# Q-mode:  Binary data (exvluding species presence-absence data)
# Examples with artificial data
# ------------------------------------------------------------------------------
# Sokal and Mechener (S1) index

# Compute five binary variables with 30 objects each.
var1 <- sample(c(rep(1, 10), rep(0, 20)))
var2 <- c(rep(0, 15), rep(1, 15))
var3 <- rep(c(1, 1, 1, 0, 0, 0), 5)
var4 <- rep(c(rep(1, 5), rep(0, 10)), 2)
var5.1 <- sample(c(rep(1, 7), rep(0, 9)))
var5.2 <- c(rep(0, 4), rep(1, 10))
var5 <- c(var5.1, var5.2)


# Variables 1 to 5 are put into a data frame
(dat <- data.frame(var1, var2, var3, var4, var5))
dim(dat)


# Computation of a matrix of simple matching coefficients:  called the Sokal and Michener index in ade4 (S1)
dat.s1 <- ade4::dist.binary(dat, method = 2)
coldiss(dat.s1, diag = TRUE)



# ------------------------------------------------------------------------------
# Q-mode:  Mixed Types including categorical (qualitative muticlass) variables
# Examples with artificial data
# ------------------------------------------------------------------------------
# Gower's similarity (S15)
#  - devised to handle data containing variables of various mathematical types, each variable receiving a treatment corresponding to its category
#  - THe fianl (dis)similarity between 2 objects is obtained by averaging the partial (dis)similarities computed for all variables separately.
#  - We shall use Gower's similarity as a symmetrical index; when a variable is declared as a factor in a data frame, the simple matching rule is applied, 
#   i.e., for each pair of objects the similarity is 1 for that variable if the factor has the same level in the 2 objects and 0 if the level is different.

# Artificial data
var.g1 <- rnorm(30, 0, 1)
var.g2 <- runif(30, 0, 5)
var.g3 <- gl(3, 10, labels = c("A", "B", "C"))
var.g4 <- gl(2, 5, 30, labels = c("D", "E"))

( dat2 <- data.frame(var.g1, var.g2, var.g3, var.g4) )
summary(dat2)


# ------------------------------------------------------
# Gower dissimilarity using function cluster::daisy()
# in cluster package, all available measuress are dissimilarities, so no conversion has to be made.
# When there missing values (coded NA) in the data, the function excludes from the comparison of 2 sites a variable where one or the other object (or both) has a missing value.
dat2.S15 <- cluster::daisy(dat2, "gower")
range(dat2.S15)

graphics.off();  par(mfrow=c(1,1));
coldiss(dat2.S15, diag = TRUE)



# Data matrix with the two orthogonal factors only
dat2partial.S15 <- cluster::daisy(dat2[, 3:4], "gower")
coldiss(dat2partial.S15, diag = TRUE)
head(as.matrix(dat2partial.S15))


# What are the dissimilarity values in the dat2partial.S15 matrix?
levels(factor(dat2partial.S15))


# Computation of a matrix of Gower dissimilarity using function gowdis() of package FD
# FD::gowdis() is the most complete function to compute Gower's coeffs.
#  - It computes the distance for mixed variables including asymmetrical binary variables.
#  - It provides 3 wyas of handling ordinal variables, including the method of Podani (1999).
dat2.S15.2 <- FD::gowdis(dat2)
range(dat2.S15.2)
coldiss(dat2.S15.2, diag = TRUE)


# Data matrix with the two orthogonal factors only
dat2partial.S15.2 <- FD::gowdis(dat2[, 3:4])
coldiss(dat2partial.S15.2, diag = TRUE)
head(as.matrix(dat2partial.S15.2))



# What are the dissimilarity values in the dat2partial.S15.2 matrix? 
levels(factor(dat2partial.S15.2))



# ------------------------------------------------------------------------------
# R-mode:  Computing dependence matrices among variables
# Species abundance data
#
#  - Covariances as well as parametric and non-parametric correlation coeffs are often used to compare species distributions through space or time.
#   Note that double-zeros as well as the joint variations in abundance contribute to increase the correlations.
#  - In the search for species associations, one can applies data transformations in order to remove the effect of the total abundance per site
#   prior to the calculation of parametric and non-parametric correlations.
#  - Some concepts of species association use only the positive covariances or correlations to recognize associations of co-varying species.
# ------------------------------------------------------------------------------
# Transpose matrix of species abundances
spe.t <- t(spe)


# Besides correlations, the chi-square distance can also be computed
# Chi-square pre-transformation followed by Euclidean distance
spe.t.chi <- vegan::decostand(spe.t, "chi.square")
spe.t.D16 <- dist(spe.t.chi)
coldiss(spe.t.D16, diag = TRUE)



# ------------------------------------------------------------------------------
# R-mode:  Species presence-absence data
#
#  - For binary data, the Jaccard, Sorensen and Ochiai coefficients can also be used in the R mode.
# ------------------------------------------------------------------------------
# Jaccard, Sorensen and Ochiai index on fish presence-absence
spe.t.j_v <- vegan::vegdist(spe.t, "jaccard", binary = TRUE)
spe.t.s_v <- vegan::vegdist(spe.t, method = "bray", binary = TRUE)  # Sorensen
spe.t.s_ades <- adespatial::dist.ldc(spe.t, "sorensen")
spe.t.och_ades <- adespatial::dist.ldc(spe.t, "ochiai")
spe.t.och_ade4 <- ade4::dist.binary(spe.t, method = 7)


round(head(spe.t.j_v, 30), 3)
round(head(spe.t.s_v, 30), 3)
round(head(spe.t.s_ades^2, 30), 3)
round(head(spe.t.och_ades^2, 30), 3)
round(head(spe.t.och_ade4^2, 30), 3)


graphics.off();  par(mfrow=c(1,1));
coldiss(spe.t.j_v, diag = TRUE)
coldiss(spe.t.s_v, diag = TRUE)
coldiss(spe.t.och_ades, diag = TRUE)



# ------------------------------------------------------------------------------
# R-mode:  Quantitative and ordinal data (other than species abundances)
# ------------------------------------------------------------------------------
# Pearson r
#  - linear correlation among environmental variables
#  - Pearson r may performa poorly to detect monotonic but nonlinear relationships among variables.
#  - If the variables are not dimensionally homogenesou, Pearson's r must be preferred to the covariance, since the correlation r is actually the covariance computed on starndardized variables.

# Reorder the variables prior to plotting
env.pearson <- cor(env)	# default method = "pearson"
round(env.pearson, 2)

env.o <- glus::order.single(env.pearson)


# plot
pairs(env[, env.o], lower.panel = panel.smooth, upper.panel = panel.cor, diag.panel = panel.hist, main = "Pearson Correlation Matrix")


# ------------------------------------------------------
# Kendall tau rank correlation or Spearman's rho
#  - Comparison among ordinal variables or among quantitative variables that may be monotonically but not linearly related
env.ken <- cor(env, method = "kendall")
env.o <- gclus::order.single(env.ken)
pairs(env[, env.o], lower.panel = panel.smoothb, upper.panel = panel.cor, no.col = TRUE, method = "kendall", diag.panel = panel.hist, main = "Kendall Correlation Matrix")


