# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
rm(list=ls())
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "vegan", "cluster", "dendextend", "pvclust", "labdsv", "picante", "indicspecies", "vegclust", "rioja", "RColorBrewer", "agricolae")
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


# remove empty site 8
spe <- spe[-8,]
env <- env[-8,]
spa <- spa[-8,]
latlong <- latlong[-8,]



# ------------------------------------------------------------------------------
# chord distance among sites (Q-mode):  Euclidean distance computed on site vectors normalized to length 1
# ------------------------------------------------------------------------------
spe.norm <- decostand(spe, "normalize")
spe.ch <- vegdist(spe.norm, "euc")
attr(spe.ch, "labels") <- rownames(spe)



# ------------------------------------------------------------------------------
# Clutering result
#  - previously analyzed clusters
# ------------------------------------------------------------------------------
k <- 4

# Ward's hierarchical clustering
spe.ch.ward <- hclust(spe.ch, method = "ward.D2")
spech.ward.g <- cutree(spe.ch.ward, k = k)


# k-means partitioning of the pre-transformed species data
set.seed(201808)
spe.kmeans <- kmeans(spe.norm, centers = k, nstart = 100)
spe.kmeans.g <- spe.kmeans$cluster


# Ward + kmeans
groups <- as.factor(spech.ward.g)
spe.means <- matrix(0, ncol(spe), length(levels(groups)))
row.names(spe.means) <- colnames(spe)
for(i in 1:ncol(spe)){ spe.means[i,] <- tapply(spe.norm[,i], spech.ward.g, mean) }
startpoints <- t(spe.means)
spe.kmeans2 <- kmeans(spe.norm, centers = startpoints)
spech.ward.gk <- spe.kmeans2$cluster


# PAM
spe.ch.pam <- pam(spe.ch, k = k, diss = TRUE)
spe.ch.pam.g <- spe.ch.pam$clustering



# ------------------------------------------------------------------------------
# Assess ecological interpretability of clustered groups:  Comparing a typology with external data (ANOVA approach)
#
# 4 environmental variables are significantly different among species clusters ?
#  - Elevation, Slope, Oxygen, and Ammonium (after some transformations)
# ------------------------------------------------------------------------------
# Test of ANOVA assumptions:  Normality test of residuals
# Shapiro-Wilk test of normality do not reject H0: data are sampled from normally distributed population
shapiro.test(resid(aov(sqrt(env$ele) ~ as.factor(spech.ward.gk))))
shapiro.test(resid(aov(log(env$slo) ~ as.factor(spech.ward.gk))))
shapiro.test(resid(aov(env$oxy ~ as.factor(spech.ward.gk))))
shapiro.test(resid(aov(sqrt(env$amm) ~ as.factor(spech.ward.gk))))



# --------------------------------------------
# Test of ANOVA assumptions:  Bartlette test of Homogeneity of variances
# variable sqrt(ele) has heterogeneous variances, not appropriate for parametric ANOVA
bartlett.test(sqrt(env$ele) ~ as.factor(spech.ward.gk))
bartlett.test(log(env$slo) ~ as.factor(spech.ward.gk))
bartlett.test(env$oxy ~ as.factor(spech.ward.gk))
bartlett.test(sqrt(env$amm) ~ as.factor(spech.ward.gk))


# Parametric Bartlett test is sensitive to departures from normality.
# For non-normal data, batrlett.perm.R computes parametric, permutation and bootstrap Bartlett test.
source("./RefData/NumericalEcologyWithR/NEwR2-Functions/bartlett.perm.R")
bartlett.perm(sqrt(env$ele), as.factor(spech.ward.gk), centr="MEDIAN", nperm=999, alpha=0.05)
bartlett.perm(log(env$slo), as.factor(spech.ward.gk), centr="MEDIAN", nperm=999, alpha=0.05)
bartlett.perm(env$oxy, as.factor(spech.ward.gk), centr="MEDIAN", nperm=999, alpha=0.05)
bartlett.perm(sqrt(env$amm), as.factor(spech.ward.gk), centr="MEDIAN", nperm=999, alpha=0.05)



# --------------------------------------------
# ANOVA test:
# apply Kruskal-Wallis non-parametric test for elevation
# Environmental variables are significantly different among species clusters
summary(aov(log(env$slo) ~ as.factor(spech.ward.gk)))
summary(aov(env$oxy ~ as.factor(spech.ward.gk)))
summary(aov(sqrt(env$amm) ~ as.factor(spech.ward.gk)))

kruskal.test(env$ele ~ as.factor(spech.ward.gk))



# --------------------------------------------
# post-hoc tests and show the distributions of 4 environmental variables by each clustered group
graphics.off();  par(mfrow = c(2, 2));
boxplot(sqrt(env$ele) ~ spech.ward.gk, main = "Elevation", las = 1, ylab = "sqrt(alt)", col = (1:k) + 1, varwidth = TRUE)
boxplot(log(env$slo) ~ spech.ward.gk, main = "Slope", las = 1, ylab = "log(slo)", col = (1:k) + 1, varwidth = TRUE)
boxplot(env$oxy ~ spech.ward.gk, main = "Oxygen", las = 1, ylab = "oxy", col = (1:k) + 1, varwidth = TRUE)
boxplot(sqrt(env$amm) ~ spech.ward.gk, main = "Ammonium", las = 1, ylab = "sqrt(amm)", col = (1:k) + 1, varwidth = TRUE)


# THIS IS BETTER:  considering multiple comparisons and Kruskal-Wallis non-parametric test with Holm correction
# package "agricolae" is required.

# Stars indicate the significance of the differences among groups for each environmental variables
# Different letters denote significant differences among groups

# boxplert(): perform ANOVA and LSD tests for multiple comparisons
# boxplerk(): perform Kruskal-Wallis test and its corresponding post-hoc comparisons (both with Holm correction)
source("./RefData/NumericalEcologyWithR/NEwR2-Functions/boxplerk.R")
source("./RefData/NumericalEcologyWithR/NEwR2-Functions/boxplert.R")
par(mfrow = c(2, 2))
boxplerk(env$ele, spech.ward.gk, xlab = "", ylab = "ele", main = "Elevation", bcol = (1:k) + 1, p.adj = "holm")
boxplert(log(env$slo), spech.ward.gk, xlab = "", ylab = "log(slo)", main = "Slope", bcol = (1:k) + 1, p.adj = "holm")
boxplert(env$oxy, spech.ward.gk, xlab = "", ylab = "oxy", main = "Oxygen", bcol = (1:k) + 1, p.adj = "holm")
boxplert(sqrt(env$amm), spech.ward.gk, xlab = "", ylab = "sqrt(amm)", main = "Ammonium", bcol = (1:k) + 1, p.adj = "holm")


# --> Sites in cluster 1 is different in Elevation, Slope, Oxygen and Ammonium from other sites


# --------------------------------------------
# Examine other environmental variables
# scatter plot by raw data
var <- c("pH", "har", "pho", "nit", "bod")
source("./RefData/NumericalEcologyWithR/NEwR2-Functions/panelutils.R")
pairs(env[var], panel = panel.smooth, diag.panel = panel.hist, main = "Bivariate Plots with Histograms and Smooth Curves")
pairs(sqrt(env[var]), panel = panel.smooth, diag.panel = panel.hist, main = "Bivariate Plots with Histograms and Smooth Curves")

shapiro.test(resid(aov(sqrt(env$pH) ~ as.factor(spech.ward.gk))))
shapiro.test(resid(aov(sqrt(env$har) ~ as.factor(spech.ward.gk))))
shapiro.test(resid(aov(sqrt(env$pho) ~ as.factor(spech.ward.gk))))
shapiro.test(resid(aov(sqrt(env$nit) ~ as.factor(spech.ward.gk))))
shapiro.test(resid(aov(sqrt(env$bod) ~ as.factor(spech.ward.gk))))

source("./RefData/NumericalEcologyWithR/NEwR2-Functions/bartlett.perm.R")
bartlett.perm(sqrt(env$pH), as.factor(spech.ward.gk), centr="MEDIAN", nperm=999, alpha=0.05)
bartlett.perm(sqrt(env$har), as.factor(spech.ward.gk), centr="MEDIAN", nperm=999, alpha=0.05)
bartlett.perm(sqrt(env$pho), as.factor(spech.ward.gk), centr="MEDIAN", nperm=999, alpha=0.05)
bartlett.perm(sqrt(env$nit), as.factor(spech.ward.gk), centr="MEDIAN", nperm=999, alpha=0.05)
bartlett.perm(sqrt(env$bod), as.factor(spech.ward.gk), centr="MEDIAN", nperm=999, alpha=0.05)


par(mfrow = c(2, 2))
boxplerk(sqrt(env$har), spech.ward.gk, xlab = "", ylab = "sqrt(har)", main = "Hardness", bcol = (1:k) + 1, p.adj = "holm")
boxplert(sqrt(env$pH), spech.ward.gk, xlab = "", ylab = "sqrt(pH)", main = "pH", bcol = (1:k) + 1, p.adj = "holm")
boxplert(sqrt(env$pho), spech.ward.gk, xlab = "", ylab = "sqrt(pho)", main = "Pho", bcol = (1:k) + 1, p.adj = "holm")
boxplert(sqrt(env$nit), spech.ward.gk, xlab = "", ylab = "sqrt(nit)", main = "Nitrate", bcol = (1:k) + 1, p.adj = "holm")
boxplert(sqrt(env$bod), spech.ward.gk, xlab = "", ylab = "sqrt(bod)", main = "BOD", bcol = (1:k) + 1, p.adj = "holm")



# ------------------------------------------------------------------------------
# Comparing site and environment typologies (contingency table approach)
# ------------------------------------------------------------------------------
env2 <- env[, -1]


# Dissimilarity indices for community ecologists
env.de <- vegan::vegdist(scale(env2), "euc")


# k-means clustering of environmental data
env.kmeans <- kmeans(env.de, centers = 4, nstart = 100)
env.kmeans.g <- env.kmeans$cluster


# Table crossing the species and environment 4-group typologies
table(spe.kmeans.g, env.kmeans.g)


# Test the relationship using a chi-square test chisq.test(table(spe.kmeans.g, env.kmeans.g))
# Change the testing procedure to a permutation test chisq.test(table(spe.kmeans.g, env.kmeans.g), simulate.p.value = TRUE)

# Test the relationship using a Fisher's exact test and permutation test
fisher.test(table(spe.kmeans.g, env.kmeans.g))
chisq.test(table(spe.kmeans.g, env.kmeans.g), simulate.p.value = TRUE)



# ------------------------------------------------------------------------------
# Species Assemblages:  Simple statistics on group contents
# ------------------------------------------------------------------------------
# compute mean species abundances in the 4 groups from the optimized Ward clustering
groups <- as.factor(spech.ward.gk)
spe.means <- matrix(0, ncol(spe), length(levels(groups)))
row.names(spe.means) <- colnames(spe)
for (i in 1:ncol(spe)) spe.means[i, ] <- tapply(spe[, i], spech.ward.gk, mean)
head(spe.means)


# Mean species abundances of the four groups
group1 <- round(sort(spe.means[, 1], decreasing = TRUE), 2)
group2 <- round(sort(spe.means[, 2], decreasing = TRUE), 2)
group3 <- round(sort(spe.means[, 3], decreasing = TRUE), 2)
group4 <- round(sort(spe.means[, 4], decreasing = TRUE), 2)



# --------------------------------------------
# Group1:  Site classified as group 1 and Species with abundances greater than group 1 mean species abundance
names(spech.ward.gk[spech.ward.gk == 1])
group1[which(group1 > mean(group1))]



# --------------------------------------------
# Group2:  Site classified as group 2 and Species with abundances greater than group 2 mean species abundance
names(spech.ward.gk[spech.ward.gk == 2])
group2[which(group2 > mean(group2))]



# --------------------------------------------
# Group3:  Site classified as group 3 and Species with abundances greater than group 3 mean species abundance
names(spech.ward.gk[spech.ward.gk == 3])
group3[which(group3 > mean(group3))]



# --------------------------------------------
# Group4:  Site classified as group 4 and Species with abundances greater than group 4 mean species abundance
names(spech.ward.gk[spech.ward.gk == 4])
group4[which(group4 > mean(group4))]



# ------------------------------------------------------------------------------
# Species Assemblages:  Kendall's W coefficient of concordance
#  - An overall test of independence of all species is first carried out. If the null hypothesis is rejected, one looks for groups of correlated species and,
#   within each group, tests the contribution of each species to the overall statistic, using a permutation test.
#  - In this method, the search for species associations is done without any reference to a typology of the sites know a priori or computed from other data.
#  - The method aims at finding the most encompassing assemblages, i.e., the smallest number of groups containing the largest number of positively and significantly associated species.
#  - This method cannot be applied to presence-absence data.
# ------------------------------------------------------------------------------
# Transformation of species data and transposition, necessary because Kendall's W is based on correlation coefficients.
spe.hel <- decostand(spe, "hellinger")
apply(spe.hel^2, 1, sum)

spe.std <- decostand(spe.hel, "standardize")
apply(spe.std, 2, sd)

spe.t <- t(spe.std)



# --------------------------------------------
# First test of Kendall concordance involving all species
# kendall.global() function includes a parametric F-test which does not suffer from the problems of the chi-square test (overly conservative) and has correct Type I error.
# The H0 of absence of concordance is rejected.
( spe.kendall.global1 <- vegan::kendall.global(spe.hel) )



# --------------------------------------------
# Then we can look for groups of species, then proceed with the Kendall analysis of these groups
# k-means partitioning of species
# The result indicates that 2 groups may be a good choice.
# Avoid solutions with groups containing a single species except when it is clear that this species belongs to no other group.
# 3 or 4 groups would also be fine at this point of the analysis: all groups would have 3 species or more.
spe.t.kmeans.casc <- cascadeKM(spe.t, inf.gr = 2, sup.gr = 8, iter = 100, criterion = "calinski")
plot(spe.t.kmeans.casc, sortg = TRUE)

head(spe.t.kmeans.casc$partition)



# --------------------------------------------
# The partition into 2 groups is found in column 1 of the object $partition
( clusters2 <- spe.t.kmeans.casc$partition[, 1] )


# Partitions into three or four groups
( clusters3 <- spe.t.kmeans.casc$partition[, 2] ) 
( clusters4 <- spe.t.kmeans.casc$partition[, 3] )



# --------------------------------------------
# Kendall W test on each group: Concordance analysis
# Look at the corrected permutational p-values.

# If all values are equal to or smaller than 0.05, you can consider that all groups are globally significant,
# i.e. that on the whole they contain species that are concordant. (only at least some species are)

# If the corrected p-values for some groups were not significant (it is not the case with this example), 
# it would indicate that these groups include non-concordant species and should be subdivided into smaller groups.
# In other words, a partition into more than 2 groups would be in order.

( spe.kendall.global2 <- vegan::kendall.global(spe.hel, clusters2) )



# --------------------------------------------
# A posteriori tests to identify the significantly concordant species within each group

# Look at the mean Spearman correlation coefficients of the individual species.
# A group contains concordant species if each of its species has a positive mean correlation with all the other species of its group.
# If a species has negative mean correlation with all other members of its group, this indicates that this species should be left out of the group

# With 2 groups, we have in the largest group one species (Sqce) that has a negative mean correlation with all members of its group.
# --> This indicates that we should look for a finer partition of the species.
( spe.kendall.post2 <- kendall.post(spe.hel, clusters2, nperm = 9999) )



# --------------------------------------------
# Now all species in the 3 groups have positive mean Spearman correlations with the other members of their group.
# Sqce finds itself in a new group of 9 species with which it has a mean positive correlation, although its contribution tothe concordance of that group is not significant.
# All the other species in all 3 groups contribute significantly to the concordance of their group. So we can stop the analysis.
( spe.kendall.post3 <- kendall.post(spe.hel, clusters3, nperm = 9999) )

( spe.kendall.post4 <- kendall.post(spe.hel, clusters4, nperm = 9999) )



# ------------------------------------------------------------------------------
# Species assemblages on presence-absence data
#  - This method consists in computing the "a" component of Jaccard's S7 coefficient (as a measure of co-occurrence among species) in R-mode 
#    and assessing its probability by means of a permutation test in the spirit of the Raup and Crick coefficient.
#  - The p-values have very small for highly co-occurring species.
# ------------------------------------------------------------------------------
# Transform the data to presence-absence
spe.pa <- decostand(spe, "pa")



# --------------------------------------------
# Test the co-occurrence of species

# Many permutations to have enough decimal places for Holm correction (see below).
# There are 27 species, and thus 27 * 26/2 = 351 tests will be run.  Bonferroni correction requires a p-value of 0.05/31 = 0.0001425 to remain significatn at the 0.05 level.
# This requires at least 9999 permutations, since the smallest p-value would be 1/(9999+1) = 0.0001.
# 99,999 permutations provide a finer estimation of the p-value, but IT TAKES TIME (2 min.)
source("./RefData/NumericalEcologyWithR/NEwR2-Functions/test.a.R")
res <- test.a(spe.pa, nperm = 99999)
summary(res)



# Compute a Holm correction on the matrix of p-values
( res.p.vec <- as.vector(res$p.a.dist) )
( adjust.res <- p.adjust(res.p.vec, method = "holm") )



# Check that the number of permutations allows significant p-values after Holm's correction. Look for values <=0.05:
range(adjust.res)


# Significance threshold: among the corrected Holm p-values, find 0.05 or the closest value smaller than 0.05.
( adj.sigth <- max(adjust.res[adjust.res <= 0.05]) )


# Now find the uncorrected p-value corresponding to adj.sigth:
( sigth <- max(res.p.vec[adjust.res <= 0.05]) )


# Assign 1 to nonsignificant p-values after Holm's correction
res.pa.dist <- res$p.a.dist
res.pa.dist[res.pa.dist > sigth] <- 1


# How many (unadjusted) values are equal to or smaller than sigth?
length(which(res.p.vec <= sigth))



# --------------------------------------------
# Heat map of significant p-values
source("./RefData/NumericalEcologyWithR/NEwR2-Functions/coldiss.R")
coldiss(res.pa.dist, nc = 16, byrank = TRUE, diag = TRUE)



# ------------------------------------------------------------------------------
# Species Co-occurrence Network
#  - Co-occurrence network analysis becomes more and more popular in community ecology, especially to investigate ecological interactions among species or among communities.
#  - Network structure is characterized by 2 main properties,
#     1. modularity:  the extent to which species co-occurrences are organized into modules, i.e. densely connected, non-overlapping subsets of species
#     2. nestedness:  the tendency of the network to show a nested pattern wherein the species composition of small assemblages is a nested subset of larger assemblages
#  - The role of a species is defined by its position compared with other species in itw own module (its "standardized within-module degree", 
#    i.e., the number of links that a node has with other nodes in the same module, standardized by the mean and standard deviation of the number of links
#    per node in the module) and how well it connects to species in other modules (its among-module "connectivity")
# ------------------------------------------------------------------------------
library(igraph)
library(rgexf)



# --------------------------------------------
# The network is built from an adjacency matric, which may be binary (species significantly co-occur or not) or numeric (links among species are weighted)
# Build adjacency matrix.

# Adjacency matrix from the binary matrix of significant co-occurrences
adjm1 <- 1 - as.matrix(res.pa.dist)
diag(adjm1) <- 0


# Adjacency matrix from the "a" distance matrix
adjm2 <- 1 - as.matrix(res$p.a.dist)
adjm2[adjm2 < 0.5] <- 0
diag(adjm2) <- 0


# Adjacency matrix from the Spearman rank correlation matrix
adjm3 <- cor(spe, method = "spearman")
# Only positive associations (rho > 0.2)
# adjm[adjm3 < 0.2] <- 0
# adjm[adjm3 >= 0.2] <- 1  # binary co-occurrences
adjm2[adjm3 < 0.25] <- 0
adjm2[adjm3 >= 0.25] <- 1  # binary co-occurrences
diag(adjm3) <- 0


# Compute species co-occurrence dissimilarities (picante, Hardy 2008)
# Here choose Jaccard dissimilarity
adjm4 <- picante::species.dist(spe.pa, metric = "jaccard")
adjm4 <- as.matrix(adjm4)
adjm4[adjm4 < 0.4] <- 0



# --------------------------------------------
# Select an adjacency matrix
adjm <- adjm4



# --------------------------------------------
# summary
summary(as.vector(adjm))


# Plot histogram of adjacency values
graphics.off();  par(mfrow=c(1,1));
hist(adjm)


# Build graph
go <- graph_from_adjacency_matrix(adjm, weighted = TRUE, mode = "undirected")
plot(go)



# --------------------------------------------
# Network structure detection: find densely connected subgraphs (modules or "communities") in a graph
wc <- cluster_walktrap(go)
# wc <- cluster_edge_betweenness(go)
# wc <- cluster_fast_greedy(go)
# wc <- cluster_leading_eigen(go)
# wc <- cluster_spinglass(go)
# wc <- igraph::cluster_optimal(go)
modularity(wc)
membership(wc)
plot(wc, go)



# --------------------------------------------
# Export to gephi
gexfo <- igraph.to.gexf(go)
print(gexfo, file = "doubs0.gexf", replace = TRUE)



# --------------------------------------------
# Detach package to avoid conflicts
detach("package:rgexf", unload = TRUE)
detach("package:igraph", unload = TRUE)

# If not sufficient:
unloadNamespace("rgexf")
unloadNamespace("igraph")



# ------------------------------------------------------------------------------
# Indicator species:  Indicator value indices
#  - Indicator value indices are usefule for asessin the species predictive values as bioinditors, e.g. for field determination of community types or for ecological monitoring,
#    whereas correlation indices should be used to determine the ecological preference of a given species among a set of alternative site groups.
#  - IndVal index is computed as the product of the SPECIFICITY of a species to the targeted group by its FIDELITY to the targeted group.
#     - SPECIFICITY:  the mean abundance of the species within the targeted group compared to its mean abundance across the groups
#     - FIDELITY:  the proportion of sites of the targeted group where the species is present
#  - The indval approach looks for species that are both necessary and sufficient,
#    i.e., if you find that species you should be in that type, and if you are in that type you should find that species.
#  - The groups of sites can be defined by clustering the sites on the basis of independent data (environmental variables, for instance).
#    The indicator species can then be considered "indicator" in the true sense of the word, i.e., species closely related to the ecological conditions of their group.
# ------------------------------------------------------------------------------
# Divide the sites into 4 groups depending on the distance from the source of the river
dfs.D1 <- dist(data.frame(dfs = env[, 1], row.names = rownames(env)))
dfsD1.kmeans <- kmeans(dfs.D1, centers = 4, nstart = 100)


# Cluster delimitation and numbering
dfsD1.kmeans$cluster


# The numbering is machine-dependent. To avoid confusions let us construct a vector of groups with consecutive numbers on the basis of dfsD1.kmeans$cluster:
grps <- rep(1:4, c(8, 10, 6, 5))



# --------------------------------------------
# Indicator species for this typology of the sites
( iva <- labdsv::indval(spe, grps, numitr = 10000) )



# --------------------------------------------
# relative frequency of the species in each group = number of sites where species is present / number of sites in the group
iva$relfrq

# relative abundance of the species across groups = total abundance in group / grand total abundance
iva$relabu


# indicator value (IndVal) of each species
iva$indval


# cluster where the species has highest IndVal
iva$maxcls


# highest IndVal of the species
iva$indcls


# permutational p-value of IndVal
iva$pval


# Correction of the p-values for multiple testing
iva$pval
( pval.adj <- p.adjust(iva$pval) )



# --------------------------------------------
# Table of the significant indicator species
# Note that the indicator species identified here may differ from the members of the species assemblages identified before.
# The indicator species are linked to predefined groups, whereas the species assemblages are identified without any prior classification or reference to environmental conditions.
gr <- iva$maxcls[pval.adj <= 0.05]
iv <- iva$indcls[pval.adj <= 0.05]
pv <- iva$pval[pval.adj <= 0.05]
fr <- apply(spe > 0, 2, sum)[pval.adj <= 0.05]
fidg <- data.frame(group = gr, indval = iv, pvalue = pv, freq = fr)
fidg <- fidg[order(fidg$group, -fidg$indval), ]
fidg


# Export the result to a CSV file (to be opened in a spreadsheet)
# write.csv(fidg, "IndVal-dfs.csv")



# ------------------------------------------------------------------------------
# Indicator species:  Indicator value indices by pooled groups
#  - Indval with multipatt{indicspecies} with search for indicator species of pooled groups
#  - Pool 2 or more groups of a typology to look for species that may be indicators of pooled groups
# ------------------------------------------------------------------------------
# In multipatt(), default indicator measure is func = IndVal.g (g. indicats that the measure is corrected for unequal group sizes)
( iva2 <- indicspecies::multipatt(spe, grps, max.order = 2, control = how(nperm = 999)) )



# --------------------------------------------
# matrix of the belonging of sites to all possible combinations up to the order requested in the analysis
iva2$comb


# If the function is the IndVal index, value of its A component (specificity), otherwise NULL
iva2$A


# If the function is the IndVal index, value of its B component (fidelity), otherwise NULL
iva2$B


# results of the best patterns, i.e., the group or combination of groups where the statistic is the highest,
# and permutation test results for that result.
# No correction for multiple testing is provided
iva2$sign



# --------------------------------------------
# Here again, correction of the p-values for multiple testing
( pval.adj2 <- p.adjust(iva2$sign$p.value) )



# --------------------------------------------
# The summary displays the groups and combinations of groups with significant indicator species.
# indvalcomp = TRUE, the A (specificity) and B (fidelity) components of the IndVal index are displayed as well

# --> for the brown trout (Satr) the highest value is found for the combination of groups 1 + 2 whereas, if the analysis is considered on separate groups only,
# the highest IndVal is in group 1
summary(iva2, indvalcomp = TRUE)



# --------------------------------------------
# Indval with bootstrap confidence intervals of indicator values
# the indicator value and the lower, upper limits of the confidence intervals.
( iva2.boot <- indicspecies::strassoc(spe, grps, func = "IndVal.g", nboot = 1000) )


# --> In groups 3 and 4, the IndVal values of the Eurasian minnow (Phph) are 0.09 and 0.054 respectively, but the lower limit of the confidence intervals is 0.
# in group 1, the CI limits for the brown trout (Satr) and the Eurasian minnow (Phph) are 0.391 - 0.828 and 0.306 - 0.758.
# Therefore, there is a large probability that the true IndVal values for these 2 species reside somewhere within the common part of these ranges (0.391 - 0.758)



# ------------------------------------------------------------------------------
# Indicator species:  Correlation-type Index for presence-absence data
#   - Pearson's phi coefficient of association
# ------------------------------------------------------------------------------
# "r.g" option corrects the measure for unequal group sizes
( iva.phi <- multipatt(spe, grps, func = "r.g", max.order = 2, control = how(nperm = 999)) )


# The results look quite similar to that of IndVal analysis.
# Note that the statistical tests highlight positive phi values, therefore, it is natural that both approaches (species indicator values and correlation)
# highlight the same strongest associations between species and selected groups.
summary(iva.phi)



# --------------------------------------------
# It may be interesting to display all the phi values to identify possible sets of environmental conditions that are avoided by some species
# The bleak (Alal) shows a very strong negative association (phi = -0.92) with the group of sites 1 + 2, 
# which mirrors its strongest positive assocation (phi = 0.92) with the group of sites 3 + 4.
# Indeed, the bleak is absent from all but 3 sites of groups 1 + 2, and is present in all sites of groups 3 + 4.
# Obviously, the bleak prefers the lower regions of the river and avoids the higher ones.
round(iva.phi$str, 3)



# --------------------------------------------
# Since the phi values can be negative or positive, you can use this opportunity to obtain a bootstrap test of significance of the phi values.
# This complements the permutational tests for the negative values.
# For instance, the aboidance of the bullhead (Cogo) of the conditions prevailing in group 1 is significant in that sense, the CI interval being [-0.313, -0.209]
( iva.phi.boot <- strassoc(spe, grps, func = "r.g", nboot = 1000) )



# ------------------------------------------------------------------------------
# Multivariate regression trees
# As of this writing, {mvpart} and {MVPARTwrap} must be installed from github.
# If is is not already done, follow these lines:
# On Windows machines, Rtools (3.4 and above) must be installed first. Go to:
# https://cran.r-project.org/bin/windows/Rtools/
# Rtools must be reinstalled with every new installation of R. After that (for Windows and MacOS), type:
#   install.packages("devtools")
#   library(devtools)
#   install_github("cran/mvpart", force = TRUE)
#   install_github("cran/MVPARTwrap", force = TRUE)
#
# mvpart()
#  - only packages implementing a complete and handy version of MRT
#  - No longer supported by the R Core Team
#  - the response data belong to class "matrix" and the explanatory variables to class "data frame"
#
# ------------------------------------------------------------------------------
library(mvpart)


# xv = "pick":  one left-clicks on the point representing the desired nmber of groups and a tree is the drawn.
par(mfrow = c(1, 2))
spe.ch.mvpart <- mvpart(data.matrix(spe.norm) ~ ., env, margin = 0.08, cp = 0, xv = "pick", xval = nrow(spe), xvmult = 100)
# Here, click on the desired number of groups (for example 4)


summary(spe.ch.mvpart)
printcp(spe.ch.mvpart)


# Residuals of MRT
par(mfrow = c(1, 2))
hist(residuals(spe.ch.mvpart), col = "bisque")
plot(predict(spe.ch.mvpart, type = "matrix"), residuals(spe.ch.mvpart), main = "Residuals vs Predicted")
abline(h = 0, lty = 3, col = "grey")


# Group composition
spe.ch.mvpart$where
# Group identity
(groups.mrt <- levels(as.factor(spe.ch.mvpart$where)))
# Fish composition of first leaf
spe.norm[which(spe.ch.mvpart$where == groups.mrt[1]), ]
# Environmental variables of first leaf
env[which(spe.ch.mvpart$where == groups.mrt[1]), ]


# Table and pie charts of fish composition of leaves
leaf.sum <- matrix(0, length(groups.mrt), ncol(spe))
colnames(leaf.sum) <- colnames(spe)
for (i in 1:length(groups.mrt)) leaf.sum[i, ] <- apply(spe.norm[which(spe.ch.mvpart$where == groups.mrt[i]), ], 2, sum)
leaf.sum

par(mfrow = c(2, 2))
for (i in 1:length(groups.mrt)) pie(which(leaf.sum[i, ] > 0), radius = 1, main = paste("leaf #", groups.mrt[i]))


# Extracting MRT results from an mvpart object
# Packages MVPARTwrap and rdaTest must have been loaded
spe.ch.mvpart.wrap <- MRT(spe.ch.mvpart, percent = 10, species = colnames(spe))
summary(spe.ch.mvpart.wrap)



# ------------------------------------------------------------------------------
# Combining MRT and IndVal
# ------------------------------------------------------------------------------
# Indicator species search on the MRT result
# spe.ch.MRT.indval$pval		# Probability
spe.ch.MRT.indval <- indval(spe.norm, spe.ch.mvpart$where)
pval.adj3 <- p.adjust(spe.ch.MRT.indval$pval)    # Corrected prob.


# For each significant species, find the leaf with the highest IndVal
# spe.ch.MRT.indval$maxcls[which(spe.ch.MRT.indval$pval <= 0.05)]
spe.ch.MRT.indval$maxcls[which(pval.adj3 <= 0.05)]


# IndVal value in the best leaf for each significant species
# spe.ch.MRT.indval$indcls[which(spe.ch.MRT.indval$pval <= 0.05)]
spe.ch.MRT.indval$indcls[which(pval.adj3 <= 0.05)]


# Partition of objects based on MRT
spech.mvpart.g <- factor(spe.ch.mvpart$where)
levels(spech.mvpart.g) <- 1:length(levels(spech.mvpart.g))
# Compare with partition from unconstrained clustering
table(spech.mvpart.g, spech.ward.g)


# Plot of the MRT clusters on a map of the Doubs River
graphics.off();  par(mfrow=c(1,1));
drawmap(xy = spa, clusters = spech.mvpart.g, main = "Four MRT clusters along the Doubs River")



# ------------------------------------------------------------------------------
# MRT as a monothetic clustering method
#  - Build the tree using the species data as both the response and explanatory variables
# ------------------------------------------------------------------------------
# Method related to Williams & Lambert (1959) association analysis:
# spe.pa (presence-absence) is the response and the explanatory matrix

spe.pa <- decostand(spe, "pa")

graphics.off()
par(mfrow = c(1, 2))

# spe.pa is the response and the explanatory matrix
res.part1 <- mvpart(data.matrix(spe.pa) ~ ., data = spe.pa, margin = 0.08, xv = "p", xvmult = 100)
# Here, click on the desired tree size (suggested: 6)
res.part1$where


# spe.norm is the response, spe.pa is the explanatory matrix
res.part2 <- mvpart(data.matrix(spe.norm) ~ ., data = spe.pa, margin = 0.08, xv = "p", xvmult = 100)
# Here, click on the desired tree size (suggested: 5)
res.part2$where


# spe.norm is the response and spe (untransformed) is the explanatory matrix
res.part3 <- mvpart(data.matrix(spe.norm) ~ ., data = spe, margin = 0.08, xv = "p", cp = 0, xvmult = 100)
# Here, click on the desired tree size (suggested: 6)


# Membership of objects to groups – presence-absence on both sides
res.part1$where
res.part1.g <- factor(res.part1$where)
levels(res.part1.g) <- 1:length(levels(res.part1.g))
# Compare with groups from unconstrained clustering
table(res.part1.g, spech.ward.g)
table(res.part1.g, spech.ward.gk)


# Plot of the MRT clusters on a map of the Doubs River
drawmap3(xy = spa, clusters = res.part1.g, main = "Six monothetic clusters along the Doubs River")



# ------------------------------------------------------------------------------
# Clustering with sequential constraint
#  - In cases where the data present themselves in a spatial (transect) or temporal sequence, the contiguity information can be taken into account
#   when looking for groups, and for discontinuities, along the series.
#  - Here we will apply a method of clustering with contiguity constraint developed for stratigraphic research (called CONISS)
# ------------------------------------------------------------------------------
graphics.off();  par(mfrow = c(1, 2));
spe.ch.seq <- mvpart(as.matrix(spe) ~ dfs,  env,  cp = 0,  xv = "pick",  margin = 0.08, xval = nrow(spe),  xvmult = 100,  which = 4)
# Here, click on the desired number of groups

summary(spe.ch.seq)


# Group composition (labels of terminal nodes)
(gr <- spe.ch.seq$where)


# Renumber clusters sequentially
aa <- 1
gr2 <- rep(1, length(gr))
for (i in 2 : length(gr))
{
  if (gr[i] !=  gr[i-1]) aa <- aa + 1
  gr2[i] <- aa
}


# Plot the clusters on a map of the Doubs river
source("./RefData/NumericalEcologyWithR/NEwR2-Functions/drawmap3.R")
par(mfrow=c(1,1))
drawmap3(xy = spa, clusters = gr2, main = "MRT sequential clustering along the Doubs River")


# Default method CONISS on the percentage difference dissimilarity matrix (aka Bray-Curtis dissimilarities)
spe.chcl <- rioja::chclust(vegdist(spe))


# Group-by-SS (sum of squares) graph comapring the dispersion of the classification at different fusion levels to a null model.
# Red line: broken stick model
# The graph suggest cutting the dendrogram to retain 2 or 4 clusters (points above the red line obtained from the broken stick model)
bstick(spe.chcl, 10)


# Cut the dendrogram in 4 clusters
k <- 4
(gr4 <- cutree(spe.chcl, k = k))
plot(spe.chcl, hang = -1, main = "CONISS clustering")
rect.hclust(spe.chcl, k = k)


# Dendrogram with observations plotted according to dfs
plot(spe.chcl, xvar = env$dfs, hang = -1, main = "CONISS clustering", cex = 0.8)


# Plot the clusters on a map of the Doubs River
# Now clustered in a consistent sequence.  The constraint of spatial contiguity has forced the separation of sites 15-22 and 26-30 into separate groups
# because the sequence is interrupted by th group 24-25 of polluted sites.
drawmap(xy = spa, clusters = gr4, main = "Sequential clusters along the river")



# ------------------------------------------------------------------------------
# Fuzzy clustering:  c-means clustering
#  - Fuzzy c-means clutstering is implemented in several packages, cluster (function fanny()) and e1071 (function cmeans())
# ------------------------------------------------------------------------------
# directly input the chord distance matrix "spe.ch"
# the default metric is euclidean
# An identical result would be obtained by using the chord-transformed species data "spe.norm" with the metric = "euclidean"
# memb.exp is a kind of "fuzzyiness exponenet" with values ranging from 1 (close to non-fuzzy clustering) to any large value.
k <- 4
spe.fuz <- cluster::fanny(spe.ch, k = k, memb.exp = 1.5)
summary(spe.fuz)


# Site fuzzy membership
# sites 10, 15, 19 is not well defined.
spe.fuz$membership


# Nearest crisp clustering
spe.fuz$clustering
spefuz.g <- spe.fuz$clustering


# Silhouette plot
# In particular, the cluster 2 is not well defined.
graphics.off();  par(mfrow=c(1,1));
plot(silhouette(spe.fuz), main = "Silhouette plot - Fuzzy clustering", cex.names = 0.8, col = spe.fuz$silinfo$widths + 1)


# Ordination of fuzzy clusters (PCoA):  ordination (PCoA) of the fish chord distance matrix
dc.pcoa <- cmdscale(spe.ch)
dc.scores <- scores(dc.pcoa, choices = c(1, 2))


# c-means fuzzy clustering of the fish data preserving the chord distance.
# Principal coordinate ordination (PCoA) associated star plots showing the membership of the sites
# the star plots of the ill-classified objects (10,15,19) also illustrate that their membership is unclear.
par(mfrow=c(1,1))
plot(dc.scores, asp = 1, type = "n", main = "Ordination of fuzzy clusters (PCoA)")
abline(h = 0, lty = "dotted")
abline(v = 0, lty = "dotted")
for (i in 1:k)
{
  gg <- dc.scores[spefuz.g == i, ]
  hpts <- chull(gg)
  hpts <- c(hpts, hpts[1])
  lines(gg[hpts, ], col = i + 1)
}

stars(spe.fuz$membership, location = dc.scores, key.loc = c(0.6, 0.4), key.labels = 1:k, draw.segments = TRUE, add = TRUE,
  # scale = FALSE,
  len = 0.075, col.segments = 2:(k + 1))


# Plot the fuzzy clusters on a map of the Doubs River
# Add sectors to represent fuzzy membership
par(mfrow=c(1,1))
plot(spa, asp = 1, type = "n", main = "Fuzzy clusters along the river", xlab = "x coordinate (km)", ylab = "y coordinate (km)")
lines(spa, col = "light blue")
text(65, 20, "Upstream", cex = 1.2)
text(15, 32, "Downstream", cex = 1.2)

for (i in 1:k) { stars(spe.fuz$membership, location = spa, key.loc = c(150, 20), key.labels = 1:k, draw.segments = TRUE, add = TRUE,
    # scale = FALSE,
    len = 5, col.segments = 2:(k + 1))
}


# ------------------------------------------------------------------------------
# Noise clustering (vegclust)
#  - The principle of the method consists in defining a cluster called "Noise" n addition to the regular clusters.
#    The "Noise" cluster is represented by an imaginary point located at a constant distance "delta" from all observations. 
#    The effect of this cluster is to capture the "objects that lie farther than delta from all the c "good centroids"
#    A small delta results in a large membership in the Noise cluster.
# ------------------------------------------------------------------------------
# Create noise clustering with 4 clusters. Perform 30 starts from random seeds and keep the best solution
# Normalized species matrix with the argument method = "NC"
k <- 4
spe.nc <- vegclust(spe.norm, mobileCenters = k, m = 1.5, dnoise = 0.75, method = "NC", nstart = 30)
spe.nc


# Medoids of species
(medoids <- spe.nc$mobileCenters)


# Fuzzy membership matrix
spe.nc$memb


# Cardinality of fuzzy clusters (i.e., the number of objects belonging to each cluster)
spe.nc$size


# Obtain hard membership vector, with 'N' for objects that are unclassified
spefuz.g <- defuzzify(spe.nc$memb)$cluster
clNum <- as.numeric(as.factor(spefuz.g))


# Ordination of fuzzy clusters (PCoA)
par(mfrow=c(1,1))
plot(dc.scores,
     # main = "Ordination of fuzzy clusters (PCoA)",
     asp = 1, type = "n")
abline(h = 0, lty = "dotted")
abline(v = 0, lty = "dotted")

for (i in 1:k)
{
  gg <- dc.scores[clNum == i, ]
  hpts <- chull(gg)
  hpts <- c(hpts, hpts[1])
  lines(gg[hpts, ], col = i + 1)
}

stars(spe.nc$memb[, 1:4], location = dc.scores, key.loc = c(0.6, 0.4), key.labels = 1:k, draw.segments = TRUE, add = TRUE,
  # scale = FALSE,
  len = 0.075, col.segments = 2:(k + 1)
)


# Defuzzified site plot shows the 2 unclassified objects
plot(dc.pcoa, xlab = "MDS1", ylab = "MDS2", pch = clNum, col = clNum)
legend("topleft", col = 1:(k + 1), pch = 1:(k + 1), legend = levels(as.factor(spefuz.g)), bty = "n")


# Plot the fuzzy clusters on a map of the Doubs River
plot(spa, asp = 1, type = "n", main = "Noise clusters along the river", xlab = "x coordinate (km)", ylab = "y coordinate (km)")
lines(spa, col = "light blue")
text(65, 20, "Upstream", cex = 1.2)
text(15, 32, "Downstream", cex = 1.2)

