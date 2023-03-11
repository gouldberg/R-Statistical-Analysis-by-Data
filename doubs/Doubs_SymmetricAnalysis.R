# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
rm(list=ls())
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "vegan", "adespatial", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  Doubs
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


# ------------------------------------------------------------------------------
# Preparation
# ------------------------------------------------------------------------------
# Set aside the variable 'dfs' (distance from the source) for later use
dfs <- env[, 1]


# Remove the 'dfs' variable from the env data frame
env2 <- env[, -1]


# Recode the slope variable (slo) into a factor (qualitative) variable to show how these are handled in the ordinations
slo2 <- rep(".very_steep", nrow(env))
slo2[env$slo <= quantile(env$slo)[4]] <- ".steep"
slo2[env$slo <= quantile(env$slo)[3]] <- ".moderate"
slo2[env$slo <= quantile(env$slo)[2]] <- ".low"
slo2 <- factor(slo2, levels = c(".low", ".moderate", ".steep", ".very_steep"))
table(slo2)


# Create an env3 data frame with slope as a qualitative variable
env3 <- env2
env3$slo <- slo2


# Create two subsets of explanatory variables
# Physiography (upstream-downstream gradient)
envtopo <- env2[, c(1 : 3)]
names(envtopo)
# Water quality
envchem <- env2[, c(4 : 10)]
names(envchem)



# ------------------------------------------------------------------------------
# Canonical correlation analysis (CCorA)
#  - The aim of the method is to represent the observations along canonical axes that maximize the correlations between the 2 tables.
#  - CCorA is appropriate for exploratory purposess and in cases where the 2 groups of variables are likely to influence each other, which may often occur in real ecological systems.
#     - Ex.: the study of 2 groups of competing taxa, a vegetation-herbivore system, and long-term studies of soil-vegetation relationships during a colonization process
#  
#  - The solution is found by maximizing the between-set dispersion, expressed by the covariance matrix between the 2 sets of variables, with respect to the within-set dispersion.
#  - The 2 sets of variables must be quantitative and are assumed to be multinormally distributed.
#
#  - One can also test the hypothesis of linear independence of the 2 multivariate data tables. (Pillai's trace is the most robust statistic to departures from normality)
#
#  - With Doubs data, we could study the structure of correlation of the two complete subsets of explanatory variables, how does chemistry relate to physiography ?
#
#  - CCorA is also available in packages stats (function concor()) and in a package (unfortunately called) CCA (funciton cc(), wrapper for cancor())
# ------------------------------------------------------------------------------
# Preparation of data (transformations to make variable distributions approximately symmetrical)
envchem2 <- envchem
envchem2$pho <- log(envchem$pho)
envchem2$nit <- sqrt(envchem$nit)
envchem2$amm <- log1p(envchem$amm)
envchem2$bod <- log(envchem$bod)
envtopo2 <- envtopo
envtopo2$ele <- log(envtopo$ele)
envtopo2$slo <- log(envtopo$slo)
envtopo2$dis <- sqrt(envtopo$dis)


# CCorA (on standardized variables)
# CCorA equation includes automatic standardization of the variables.
chem.topo.ccora <- vegan::CCorA(envchem2, envtopo2, stand.Y = TRUE, stand.X = TRUE, permutations = how(nperm = 999))
chem.topo.ccora


# --> there is a significant relationship between the 2 matrices (permutational probability = 0.001).
# Pillai's trace is the sum of the squared canonical correlations.
# The canonical correlations are high on the 1st two axes.
# The RDA R^2 and adjusted R^2 are not part of the CCorA computations strictly speaking;
#  - the 2 RDAs are computed separately for information.
#  - This information is useful to asess whether the canonical axes are likely to express a substantial amount of variation, since canonical correlations may be large
#   even when the common variation is small with respect to the total variation of the two data sets.



# The pair of biplots expresses the fact that oxygenated waters (oxy) are related to high elevation (ele) and stepp slope (slo), i.e.,
# upstream conditions, whereas discharge (dis) is highly positively correlated with high hardness (har), phosphates (pho) and nitrates (nit);
# elevation (ele) and slope (slo) are highly negatively correlated with these same variables.
biplot(chem.topo.ccora)
biplot(chem.topo.ccora, plot.type = "biplot")



# ------------------------------------------------------------------------------
# Co-inertia analysis (CoIA)
#  - altertive to CCorA, very general and flexible way to couple 2 or more data tables.
#  - Basically, CoIA requires site-by-variables input matrices.
#
#  - An attractive feature of CoIA is the possibility to adapt it to the mathematical type of the variables of the 2 matrices, using the various transformations.
#    Hellinger, chord or other appropriate transformations for species presence-absence or abundance data.
#
#  - A function coinertia() retrieves the centered or transformed data matrices from the dudi.xxx() output objects and computes coinertia analysis from them.
# ------------------------------------------------------------------------------
# PCA on both matrices using ade4 functions
dudi.chem <- dudi.pca(envchem2, scale = TRUE, scannf = FALSE)
dudi.topo <- dudi.pca(envtopo2, scale = TRUE, scannf = FALSE)


# Cumulated relative variation of eigenvalues
# 3 axes of the chemistry PCA account for 89.8% variation.
cumsum(dudi.chem$eig / sum(dudi.chem$eig))


# Cumulated relative variation of eigenvalues
# 2 axes of the physiography PCA account fro 98.9% variation.
cumsum(dudi.topo$eig / sum(dudi.topo$eig))


# Are the row weights equal in the 2 analyses?
all.equal(dudi.chem$lw, dudi.topo$lw)



# Co-inertia analysis
# After having verified that the row weights are equal in the 2 PCAs, these 2 results are then submitted to CoIA, which is asked to retain 2 canonical axes.
coia.chem.topo <- coinertia(dudi.chem, dudi.topo, scannf = FALSE, nf = 2)



# Eigenvalues decomposition of the matrix of co-inertia on 2 axes (rows)
#  - eigenvalues (eig), covariance (covar), standard deviation (sdX and sdY) of the two sets of sites scores on the co-inertia axes
#   and correlations between the 2 sets of site scores, computed using the Pearson correlation coefficient.
# Inertia & coinertia X and Y
#  - Compare the inertia of the (cumulated) projections of the data tables to the maximum cumulated inertia of the axes of the separate ordinations ("max")
#  - It also gives the ratio between these values as a measure of concordance between the 2 projections.
# RV coefficient:  the ratio of the total co-inertia to the square root of the product of the total inertias of the separate analyses.
#  - Ranged between 0 (independent) and 1 (homothetic), it measures the closeness between the 2 sets of points derived from the separate ordinations of X and Y
#  - for 2 simple variables x1 and x2, RV is the square of their Pearson correlation coeff.
summary(coia.chem.topo)



# Relative variation on first eigenvalue
# 1st eigenvalues, which represents 98.9% of the total variation, is overwhelmingly larger than the second one.
coia.chem.topo$eig[1] / sum(coia.chem.topo$eig)



# Permutation test is run to assess the significance of the co-inertia structure of the data tables.
randtest(coia.chem.topo, nrepet = 999)



# The circular plots:  axes 1 of the 2 PCAs are almost perfectly aligned on the 1st CoIA axis.
# Upper righ-hand plot (normed site scores):
#  - the positions of the sites on the co-inertia axes using the chemistry (origins of the arrows) and physiography (arrowheads) co-inertia weights.
#  - shorter an arrow is, the better the concordance between the 2 projections of the point.
# Lower right-hand pair of plots
#  - the contributions of the 2 groups of variables to the canonical space.
#  - vectors pointing in the same direction are correlated and longer vectors contribute more to the structure.
#  - Oxygen (oxy) correlates positively with slope (slo), phosphates (pho) negatively with slope (slo)
#  - nitrates (nit), hardness (har, lable masked by nitrates) and biological oxygen demand (bod) are all negatively correlated with elevation (ele),
#    since these variables have higher values downstream, and positively with discharge (dis), which increases downstream.

plot(coia.chem.topo)



# ------------------------------------------------------------------------------
# Multiple factor analysis (MFA)
#  - Useful to explore the complex relationships among several ecologically meaningful groups of descriptors, whatever their number and type.
#  - This analysis is correlative:  it does not involve any hypothesis of causal influence of a data set on another.
#  - The variables must belong to the same mathematical type (quantitative or qualitative) within each subset.
#  - If all variables are quantitative, the nMFA is basically a PCA applied to the whole set of variables in which each subset is weighted.
#
#  - DO NOT CONFUSE it with multiple correspondence analysis (MCA) where a single matrix of qualitative variables is submitted to ordination and other matrices
#    may be added as supplementary (passive) information.
# ------------------------------------------------------------------------------
# Hellinger-transform the species dataset
spe.hel <- decostand(spe, "hellinger")


# MFA on 3 groups of variables: Regroup the 3 tables (Hellinger-transformed species, physiographic variables, chemical variables) 
tab3 <- data.frame(spe.hel, envtopo, envchem)
dim(tab3)


# Number of variables in each group
( grn <- c(ncol(spe), ncol(envtopo), ncol(envchem)) )


# Compute the MFA without multiple plots
graphics.off()
t3.mfa <- FactoMineR::MFA(tab3, group = grn, type = c("c", "s", "s"), ncp = 2, name.group = c("Fish community", "Physiography", "Water quality"), graph = FALSE)
t3.mfa

summary(t3.mfa)
t3.mfa$ind


# Partial axes:  represents the projection of the principal components of each separate PCA on the global PCA
# the 1st 2 axes represent more than 63% of the total variance
plot(t3.mfa, choix = "axes", habillage = "group", shadowtext = TRUE)


# Individual Factor Map:  the positions of the sites according to 4 viepoints
#  - the labelled black points represent the MFA site scores (centroids of the site scores of the 3 separate PCAs)
#  - they are connected by coloured lines to the points representing their scores in the 3 separate PCAs
plot(t3.mfa, choix = "ind", partial = "all", habillage = "group")


# Correlation circle:  the normalized vectors of all quantitative variables
plot(t3.mfa, choix = "var", habillage = "group", shadowtext = TRUE)


# --> If we examine Individual Factor Map and Correlation Circle together, we can recognize the main upstream-downstream gradient along the 1st axis
# and the gradient of water quality along a combination of the 1st and 2nd axes (from upper left to lower right)
# For example, the scores of sites 1, 2 and 3 correspond to high elevation and strong slope, as well as high oxygen concentration.
# Here, close to the source, the ecological conditions are dominated by physiography.
# The relatively poor fish community is characterized by Satr, Phph and Babl.
# On the opposite side, sites 23, 24 and 25 show the highest concentrations in phosphates, ammonium and nitrates, and a high biological oxygen demand.
# These three sites are heavily polluted and their community is characterized by another set of 3 species: Alal, Ruru and Sqce.


plot(t3.mfa, choix = "group")


# RV coefficients with tests
#  - RV coeffs:  lower-left triangles
#  - p-values:  upper-rightt triangles
# fish communities are mostly linked to the physiographic conditions (RV = 0.58),
# which are themselves partly linked to water chemistry (RV = 0.36)
rvp <- t3.mfa$group$RV
rvp[1, 2] <- coeffRV(spe.hel, scale(envtopo))$p.value
rvp[1, 3] <- coeffRV(spe.hel, scale(envchem))$p.value
rvp[2, 3] <- coeffRV(scale(envtopo), scale(envchem))$p.value
round(rvp[-4, -4], 6)



# ----------------------------------------------------
# One can draw a scree plot and a broken stick model of the MFA eigenvalues, but function screeplot.cca() of vegan does not work on the output of the MFA() function.
source("./RefData/NumericalEcologyWithR/NEwR2-Functions/screestick.R")
ev <- t3.mfa$eig[, 1]
names(ev) <- paste("MFA", 1 : length(ev))
screestick(ev, las = 2)



# ----------------------------------------------------
# Alternative to the standard, automatic MFA plots :  Plot only the significant variables (correlations)
# Select the most characteristic variables
aa <- dimdesc(t3.mfa, axes = 1:2, proba = 0.0001)

# Plot
varsig <- t3.mfa$quanti.var$cor[unique(c(rownames(aa$Dim.1$quanti), rownames(aa$Dim.2$quanti))), ]
plot(varsig[, 1:2], asp = 1, type = "n", xlim = c(-1, 1), ylim = c(-1, 1))
abline(h = 0, lty = 3)
abline(v = 0, lty = 3)
symbols(0, 0, circles = 1, inches = FALSE, add = TRUE)
arrows(0, 0, varsig[, 1], varsig[, 2], length = 0.08, angle = 20)
for (v in 1 : nrow(varsig))
{
  if (abs(varsig[v, 1]) > abs(varsig[v, 2]))
  {
    if (varsig[v, 1] >=  0) pos <- 4
    else pos <- 2
  }
  else
  {
    if (varsig[v, 2] >=  0) pos <- 3
    else pos <- 1
  }
  text(varsig[v, 1], varsig[v, 2], 
       labels = rownames(varsig)[v], 
       pos = pos)
}


