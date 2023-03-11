# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
rm(list=ls())
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "vegan", "missMDA")
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
# Principal component analysis (PCA)
#  - PCA on the full environmental dataset
#
# PCA works on a dispersion matrix, i.e. an association matrix among variables containing the variances and covariances of the variable
# (when these are dimensionally homogeneou), or the correlations computed from dimensionally heterogeneous variables.
# 
# intertia:
#   - either the sum of the variances of the variables (PCA on a covariance matrix) or
#     as in this case (PCA on a correlation matrix), the sum of the diagonal values of the correlation matrix, i.e. the sum of all correlations of the variables with themselves,
#     which corresponds to the number of variables (11)
# eigenvalues:
#   - measures of the importance (variance) of the PCA axes, proportions of variation accounted for by the axes.
# scaling 1:  distance biplot: the eigenvectors are scaled to unit length.
#   - Distances among objects in the biplot are approximations of their Euclidean distances in multidimensional space.
#   - The angles among descriptor vectors do not reflect their correlations
# scaling 2:  correlation biplot:  each eigenvector is scaled to the square root of its eigenvalue
#   - Distances among objects in the biplot are not approximations of their Euclidean distances in multidimensional space
#   - The angles between descriptors in the biplot reflect their correlations
# If the main interest of the analysis is to interpret the relationships among OBJECTS, choose scaling 1
# If the main interest focuses on th erelationships among DESCRIPTORS, choose scaling 2
# Scaling 3:  "symmetric scaling", supposed to allow a simultaneous representation of sites and scores without emphasizing one or the other point of view.
# ------------------------------------------------------------------------------

# A reminder of the content of the env dataset
summary(env)



# ---------------------------------------------
# PCA based on a correlation matrix
# Argument scale=TRUE calls for a standardization of the variables
env.pca <- vegan::rda(env, scale = TRUE)
env.pca

summary(env.pca) # Default scaling 2


# display no site and species scores (axes = 0)
# The proportion of variance accounted for by the 1st two axes is 0.751 or 75.1%
summary(env.pca, axes = 0)
summary(env.pca, scaling = 1)


# Examine and plot partial results from PCA output
?cca.object


# Eigenvectors (Principal Axes of dispersion matrix)
# The elements of the eigenvectors are also weights, or loadings of the original descriptors, in the linear combination of descriptors from which the principal components are computed.
# The elements of the eignevectors are direction cosines of the angles between the original descriptors and the principal axes
head(env.pca$CA$u)


# Principal Components: the positions of the objects with respect ot the new system of principal axes



# -----------------------------------------------------
# Examines the eigenvalues, how many axes are worth representing and displaying on the basis of the amount of variance explained ?

# Eigenvalues:  the amounts of variance of the data along the succesiive principal axes
( ev <- env.pca$CA$eig )


# the ratio of cumulative eignevalues to total variance = Coefficient of determination (R^2)
# (in PCA, the Euclidean distances among objects have been preserved through the rotation of axes)
# The proportion of variance accounted for by the 1st two axes is 0.751 or 75.1%
cumsum(env.pca$CA$eig) / sum(env.pca$CA$eig)



# -----------------------------------------------------
# Examines the eigenvalues, how many axes are worth representing and displaying on the basis of the amount of variance explained ?
# Scree plot and broken stick model
#  - randomly divides a stick of unit length into the same number of pieces as there are PCA eigenvalues.  The pieces are then put in order of decreasing lengths
#   and compared to the eigenvalues.
#  - One interprets only the axes whose eigenvalues are larger than the length of the corresponding piece of the stick, or, alternately, one may compare
#   the sum of eigenvalues, from 1 to k, to the sum of the values from 1 to k predicted by the broken stick model.

par(mfrow = c(1, 1))
stats::screeplot(env.pca, bstick = TRUE, npcs = length(env.pca$CA$eig))



# -----------------------------------------------------
# Two PCA biplots: scaling 1 and scaling 2
# Plots using biplot.rda
# Following the tradition, OBJECTS are represented as points, and variables are displayed as arrows
# Scaling 1: optimal display of distance relationships among objects
# Scaling 2: optimal display of covariances among variables
par(mfrow = c(1, 2))
biplot(env.pca, scaling = 1, main = "PCA - scaling 1")
biplot(env.pca, main = "PCA - scaling 2")  # Default scaling 2


# Plots using cleanplot.pca
# A rectangular graphic window is needed to draw the plots together
# scaling 1 biplot
#  - the circle is called a circle of equilibrium contribution.
#    Its radius is equal to square root of d/p (= 2/11, d: numbers of axes in the biplot, p: dimensions of the PCA space)
#    The radius of this circle represents the length of the vector representing a variable that would contribute equally to all dimensions of the PCA space.
#    The variables that have vectors longer than this radius make a higher contribution than average and can be interpreted with confidence.
# scaling 2 biplot
#  - projection into a Mahalanobis space. The variables are organized in groups.

graphics.off()
source("./RefData/NumericalEcologyWithR/NEwR2-Functions/cleanplot.pca.R")
par(mfrow = c(1, 2))
cleanplot.pca(env.pca, scaling = 1, mar.percent = 0.08)
cleanplot.pca(env.pca, scaling = 2, mar.percent = 0.04)


# just compare
graphics.off();  par(mfrow=c(2,2));
biplot(env.pca, scaling = 1, main = "PCA - scaling 1")
biplot(env.pca, main = "PCA - scaling 2")
cleanplot.pca(env.pca, scaling = 1, mar.percent = 0.08)
cleanplot.pca(env.pca, scaling = 2, mar.percent = 0.04)


# -->
# Scaling 1 biplot: Note that ARCH type shape is shown.
#  - a gradient from left to right, starting with a group formed by sites 1-10 which display the highest values of elevation (ele) and slope (slo),
#    and the lowest values in river discharge (dis), distance from the source (dfs) and hardness (har).
#  - The second group of sites (11-16) has the highest values in oxygen content (oxy) and the lowest in nitrate concentration (nit).
#  - A third group of very similar sites (17-22) show intermediate values in almost all the measured variables; they are not spread out by the variables contributing to axes 1 and 2.
#  - Phosphate (pho) and ammonium (amm) concentrations, as well as biological oxygen demand (bod) show their maximum values around sites 23-25; the values decrease afterwards.
#  - Overall, the progression from oligotrophic, oxygen-rich to eutrophic, oxygen-deprived water is clear.

# Scaling 2 biplot shows the variables are organized in groups. (FAR MORE INFORMATIVE THAN THE VISUAL EXAMINATION OF A CORRELATION MATRIX)
#  - The lower left part of the biplot shows that evelevation and slope are very highly, positively correlated,
#    and that these 2 variables are very highly, negatively correlated with another group comprising distance from the source, river discharge and hardness.
#  - Oxygen content is positively correlatted with slope and elevation, but very negatively with phosphate and ammonium concentration and,
#    of course, with biological oxygen demand.
#  - The right part of the diagram shows the variables associated with the lower section of the river, i.e., the group discharge and hardness,
#    which are highly correlated with the distance from the source, and the group of variables linked to eutrophication, i.e. phosphate, ammonium concentration
#    and biological oxygen demand.
#  - Positively correlated with these 2 groups is nitrate concentration.
#  - Nitrate and pH have nearly orthogonal arrows, indicating a correlation close to 0.
#    pH displays a shorter arrow, showing its lesser importance for the ordination of the sites in the ordination plane.
#    A plot of axes 1 and 3 would emphasize its contribution to axis 3.



# -----------------------------------------------------
# biplot by axes 1,2 and 1,3, and 2,3
# pH displays a shorter arrow in space of axes 1 and 2, but longer in space by axes 1 and 3
par(mfrow = c(2, 2))
biplot(env.pca, main = "PCA - scaling 2", choices = c(1,2))
biplot(env.pca, main = "PCA - scaling 2", choices = c(1,3))
biplot(env.pca, main = "PCA - scaling 2", choices = c(2,3))



# -----------------------------------------------------
# Plots with a subset of variables: ele, oxy, har, bod, 
# using cleanplot.pca
par(mfrow = c(1, 2))
var.subset <- c(2, 6, 10, 11)
cleanplot.pca(env.pca, scaling = 1, select.spe = var.subset, mar.percent = 0.10)
cleanplot.pca(env.pca, scaling = 2, select.spe = var.subset, mar.percent = 0.04)


# Plots with a subset of variables: ele, oxy, har, bod, 
# using biplot {vegan}
par(mfrow = c(1, 2))
# Scaling 1
var.sc1.sub <- scores(env.pca,  scaling = 1, display = "species")[c(2, 6, 10, 11), ]
biplot(env.pca, scaling = 1, main = "PCA scaling 1", type = "n")
text(env.pca, scaling = 1, display = "sites", cex = 0.7)
arrows(0, 0, var.sc1.sub[, 1], var.sc1.sub[, 2], length = 0.10, angle = 10, col = "red")
text(var.sc1.sub[, 1], var.sc1.sub[, 2], labels = rownames(var.sc1.sub), col = "red", pos = 4)


# Scaling 2
var.sc2.sub <- scores(env.pca, display = "species")[c(2, 6, 10, 11), ]
biplot(env.pca, type = "n", main = "PCA scaling 2")
text(env.pca, scaling = 2, display = "sites", cex = 0.7)
arrows(0, 0, var.sc2.sub[, 1], var.sc2.sub[, 2], length = 0.10, angle = 10, col = "red")
text(var.sc2.sub[, 1], var.sc2.sub[, 2], labels = rownames(var.sc2.sub), col = "red", pos = 4)



# -----------------------------------------------------
# Projecting position of new variables in a PCA by predict()

# PCA of the environmental variables minus oxy and bod
# Create data frame with oxy and bod (our "new" variables)
env.pca2 <- rda(env[, -c(10, 11)], scale = TRUE)
new.var <- env[, c(10, 11)]


# Compute position of new variables (oxy and bod) (arrow tips)
new.vscores <- predict(env.pca2, type = "sp", newdata = new.var, scaling = 2)


# Plot of the result - scaling 2
biplot(env.pca2, scaling = 2)
arrows(0, 0, new.vscores[, 1], new.vscores[, 2], length = 0.05, angle = 30, col = "blue")
text(new.vscores[, 1], new.vscores[, 2], labels = rownames(new.vscores), cex = 0.8, col = "blue", pos = 2)



# Projecting supplementary objects into a PCA biplot

# PCA and projection of supplementary sites 2, 9 and 23 using prcomp() {stats} and predict()
# Line numbers 8 and 22 are offset because of the deletion of empty site #8

# PCA using prcomp()
# Argument 'scale.= TRUE' calls for a PCA on a correlation matrix; it is equivalent to 'scale = TRUE' in function rda()
env.prcomp <- prcomp(env[-c(2, 8, 22), ], scale. = TRUE)


# Plot of PCA results using biplot.prcomp().
# Functions text() and points() do not seem to work with this biplot(env.prcomp, scale = 0)


# Plot of PCA site scores using generic function plot()
plot(env.prcomp$x[ ,1], env.prcomp$x[ ,2], type = "n", main = "PCA scaling 1 - sites with supplementary objects", xlab = "PCA 1", ylab = "PCA 2")
abline(h = 0, col = "gray")
abline(v = 0, col = "gray")
text(env.prcomp$x[ ,1], env.prcomp$x[ ,2], labels = rownames(env[-c(2, 8, 22), ]))


# Prediction of new site scores
new.sit <- env[c(2, 8, 22), ]
pca.newsit <- predict(env.prcomp, new.sit) 
# Projection of new site scores into the PCA plot
text(pca.newsit[, 1], pca.newsit[, 2], labels = rownames(pca.newsit), cex = 0.8, col = "blue")



# NOT IN THE BOOK--------------------------------------------------
# PCA of the environmental variables minus sites 2, 9 and 23
# Based on a PCA computed using function rda() of vegan
env.small <- env[-c(2, 8, 22), ]
env.pca3 <- rda(env.small, scale = TRUE)
# Create data frame with sites 2, 8 and 22 (our "new" sites)
new.sit <- env[c(2, 8, 22), ]

# Compute scores of new sites

# Extract Matrix U (called v in vegan)
U.mat <- env.pca3$CA$v
# Standardize new data with parameters of data used in PCA
env.mean <- apply(env.small, 2, mean)
env.sd <- apply(env.small, 2, sd)
newsit.stand <- scale(new.sit, center = env.mean, scale = env.sd)
# Raw scaling 1 scores of new sites
newsit.scores <- newsit.stand %*% U.mat
# Extraction of vegan site scores and retrieval of the vegan 
# scaling constant
env.scores1 <- scores(env.pca, display = "sites", choices = c(1, 2), scaling = 1)
const <- attributes(env.scores1)$const


# Compute scores for new sites - in the vegan universe:
newsit.scores.cons <- newsit.scores / const
# Plot of the result - scaling 1
biplot(env.pca3, scaling = 1)
text(newsit.scores.cons[, 1], newsit.scores.cons[, 2], labels = rownames(newsit.scores.cons), cex = 0.8, col = "blue")


# Manual computation of the vegan constant
n <- nrow(env[-c(2, 8, 22), ])
eigenv <- env.pca3$CA$eig
tot <- sum(eigenv)
const <- ((n - 1) * tot) ^ 0.25



# ------------------------------------------------------------------------------
# Combining clustering and ordination result
#  - Plot together cluster dendrogram and PCA biplot (scaling 1)
# ------------------------------------------------------------------------------
# Clustering the objects using the environmental data: Euclidean distance after standardizing the variables, followed by Ward clustering
env.w <- hclust(dist(scale(env)), "ward.D")


# Cut the dendrogram to yield 4 groups
gr <- cutree(env.w, k = 4)
grl <- levels(factor(gr))



# -----------------------------------------------------
# Extract the site scores, scaling 1
sit.sc1 <- scores(env.pca, display = "wa", scaling = 1)



# -----------------------------------------------------
# Cluster dendrogram and PCA biplot (scaling1) of the Doubs environmental data with overlaid clustering results
# Plot the sites with cluster symbols and colours (scaling 1)
graphics.off();  par(mfrow=c(1,2));
plot(env.w)
p <- plot(env.pca, display = "wa", scaling = 1, type = "n", main = "PCA correlation + clusters")
abline(v = 0, lty = "dotted");  abline(h = 0, lty = "dotted");
for (i in 1:length(grl)) points(sit.sc1[gr == i, ], pch = (14 + i), cex = 2, col = i + 1)
text(sit.sc1, row.names(env), cex = 0.7, pos = 3)
vegan::ordicluster(p, env.w, col = "dark grey")
legend(locator(1), paste("Cluster", c(1:length(grl))), pch = 14 + c(1:length(grl)), col = 1 + c(1:length(grl)), pt.cex = 2)



# ------------------------------------------------------------------------------
# PCA on transformed species data
#   - PCA on the fish abundance data
# ------------------------------------------------------------------------------
# Hellinger pre-transformation of the species data
spe.h <- decostand(spe, "hellinger")
( spe.h.pca <- rda(spe.h) )
( spe.pca <- rda(spe) )


# For comparison, chi-quare transformation
spe.c <- decostand(spe, "chi.square")
( spe.c.pca <- rda(spe.c) )



# -----------------------------------------------------
# Scree plot and broken stick model
graphics.off();  par(mfrow=c(2,1));
screeplot(spe.h.pca, bstick = TRUE, npcs = length(spe.h.pca$CA$eig))
screeplot(spe.pca, bstick = TRUE, npcs = length(spe.pca$CA$eig))



# -----------------------------------------------------
# PCA biplots 
# The species do not form clear groups like the environmental variables. However, see how the species replace one another along the site sequence.
# In the scaling 1 biplot, observe that 8 species contribute strongly to axes 1 and 2.
# Are these species partly or completely the same as those identified as indicators of the groups ?
spe.pca.sc1 <- scores(spe.h.pca, display = "species", scaling = 1)
spe.pca.sc2 <- scores(spe.h.pca, display = "species", scaling = 2)

par(mfrow = c(1, 2))
cleanplot.pca(spe.h.pca, scaling = 1, mar.percent = 0.06)
cleanplot.pca(spe.h.pca, scaling = 2, mar.percent = 0.06)



# -----------------------------------------------------
# Compare biplots of chi-square-transformed, hellinger-transformed, and original data
# original data biplots (Scaling 1)
#  - in not-hellinger-transfored biplot, many species are outside the circle of equillibrium contribution
#  - Ordination along PC1 is not so much different but ordination along PC2 is much different
par(mfrow = c(3, 2))
cleanplot.pca(spe.c.pca, scaling = 1, mar.percent = 0.06)
cleanplot.pca(spe.c.pca, scaling = 2, mar.percent = 0.06)
cleanplot.pca(spe.h.pca, scaling = 1, mar.percent = 0.06)
cleanplot.pca(spe.h.pca, scaling = 2, mar.percent = 0.06)
cleanplot.pca(spe.pca, scaling = 1, mar.percent = 0.06)
cleanplot.pca(spe.pca, scaling = 2, mar.percent = 0.06)



# ------------------------------------------------------------------------------
# Passive (post hoc) explanation of axes using environmental variables
# A posteriori projection of environmental variables in a PCA
#
# envfit() finds vectors or factor averages of environmental variables.
# The projections of points onto vectors have maximum correlation with corresponding environmental variables, and the factors show the averages of factor levels.
# ------------------------------------------------------------------------------
( spe.h.pca.env <- envfit(spe.h.pca, env, scaling = 2) ) # Scaling 2 is default


# Plot significant variables with a user-selected colour
# This has added the significant environmental variables to the last biplot drawn by R.
# BEWARE: envfit must be given the same scaling as the plot to which its result is added!

# envfit() computes a permutation test of the environmental variables and
# the plot() function allows users to draw only the variables with p-values equal to or smaller than a given level.
par(mfrow=c(1,1))
biplot(spe.h.pca, main = "PCA fish abundances - scaling 2")
plot(spe.h.pca.env, p.max = 0.05, col = 3)



# ------------------------------------------------------------------------------
# PCA using PCA.newr() and biplot.PCA.newr() for quick assessment of the structure of data
# ------------------------------------------------------------------------------
graphics.off()
source("./RefData/NumericalEcologyWithR/NEwR2-Functions/PCA.newr.R")



# PCA on enviromental data
par(mfrow=c(2,2))
env.PCA.PL <- PCA.newr(env, stand = TRUE)
biplot.PCA.newr(env.PCA.PL)
biplot.PCA.newr(env.PCA.PL, scaling = 2)
biplot(env.pca, main = "PCA - scaling 1", scaling = 1, choices = c(1,2))
biplot(env.pca, main = "PCA - scaling 2", scaling = 2, choices = c(1,2))
cleanplot.pca(env.pca, scaling = 1, mar.percent = 0.06)
cleanplot.pca(env.pca, scaling = 2, mar.percent = 0.06)



# PCA on species data:  WHY NOT SAME ??
par(mfrow=c(2,2))
speh.PCA.PL <- PCA.newr(spe.h, stand = TRUE)
biplot.PCA.newr(speh.PCA.PL)
biplot.PCA.newr(speh.PCA.PL, scaling = 2)
cleanplot.pca(spe.h.pca, scaling = 1, mar.percent = 0.06)
cleanplot.pca(spe.h.pca, scaling = 2, mar.percent = 0.06)



# ------------------------------------------------------------------------------
# Imputation of missing values in PCA:  3 missing values
#  - Josse and Husson (2012) proposed a regularized iterative PCA algorithm to provide point estimates of the principal azes and components.
#  - The PCA is computed iteratively, axis by axis, in a completely different way as the one used in vegan. When missing values are present,
#    they are first replaced by the mean of the corresponding variable. Then a PCA is computed, and the missing vaues receive new estimates baed on the result.
#  - The procedure is repeated until convergence, which corresponds to the point where the imputed values become stable.
#  - Complex procedures are implemented (1) to avoid overfitting of the imputation model, and (2) to overcome the problem of the underestimation of the variance of the imputed data.
#
#  - The PCA solutions obtained with imputed values estimated using different numbers of PCA axes are not nested, i.e., a solution based on s axes is not identical
#    to the first s axes of a solution with (s+1) or (s+2) axes. Consequently, it is important to make an appropriate a priori decision about the desired number of axes.
#
#  - Josse and Husson have grouped all necessary functions to perform iterative imputation of missing values in PCA in a packages called missMDA.
# ------------------------------------------------------------------------------
env.miss3 <- env


# Replacement of 3 selected values by NA
# Delete the pH value in site 2, where pH is 8.0 which is close to mean (=8.04).  pH has reasonably symmetrical distribution.
# Delete the pho value of site 19, where pho is 0.60, which is close to mean (=0.57).  But pho has highly asymmetrical variable.
# Delete the bod value of site 23, where bod is 16.4, which is the second highest value, far from its meaan.
env.miss3[2, 5] <- NA    # pH
env.miss3[18, 7] <- NA   # pho
env.miss3[22, 11] <- NA  # dbo


# New means of the involved variables (without missing values)
mean(env.miss3[, 5], na.rm = TRUE)
mean(env.miss3[, 7], na.rm = TRUE)
mean(env.miss3[, 11], na.rm = TRUE)



# -----------------------------------------------------
# Imputation
env.imp <- missMDA::imputePCA(env.miss3)


# Imputed values
# The only good imputation is the one for pH, which falls right on the original value.
env.imp$completeObs[2, 5]    # Original value: 8.0
env.imp$completeObs[18, 7]   # Original value: 0.60
env.imp$completeObs[22, 11]  # Original value: 16.4



# -----------------------------------------------------
# PCA on the imputed data
env.imp3 <- env.imp$completeObs
env.imp3.pca <- vegan::rda(env.imp3, scale = TRUE)


# Procrustes analysis finds the best superposition of two configurations of the same objects by rotation of one of the two sets.
pca.proc <- vegan::procrustes(env.pca, env.imp3.pca, scaling = 1)


# Procrustes comparison of original PCA and PCA on imputed data
# THe differences between the real and reconstructed values had a very small impact on the ordination
# The only visible differene is at site #23, where the difference between the real and imputed value is the largest.
graphics.off();  par(mfrow=c(1,1))
plot(pca.proc, main = "Procrustes rotation of original and imputed PCA\n3 missing values")
points(pca.proc, display = "target", col = "red")
text(pca.proc, display = "target", col = "red", pos = 4, cex = 0.6)



# ------------------------------------------------------------------------------
# Imputation of missing values in PCA - 32 missing values
# ------------------------------------------------------------------------------
# Random replacement of 32 values (out of the 319) by NA
rnd <- matrix(sample(c(rep(1, 32), rep(0, 287))), 29, 11)
env.miss32 <- env
env.miss32[rnd == 1] <- NA


# How many NA in each site?
summary(t(env.miss32))


# Alternative way to display the number of NA:
# sapply(as.data.frame(t(env.miss32)), function(x) sum(is.na(x)))


# Imputation
env.imp2 <- imputePCA(env.miss32)


# PCA on the imputed data
env.imp32 <- env.imp2$completeObs
env.imp32.pca <- rda(env.imp32, scale = TRUE)


# Procrustes comparison of original PCA and PCA on imputed data
# Here the difference is larger (with the largest distance for site 1), but the overall shape of the ordination diagram, 
# including the orientation of the variables (not shown here), is still reasonably well preserved.
pca.proc32 <- procrustes(env.pca, env.imp32.pca, scaling = 1)

graphics.off();  par(mfrow=c(1,1))
plot(pca.proc32, main = "Procrustes rotation of original and imputed PCA\n32 missing values")
points(pca.proc32, display = "target", col = "red")
text(pca.proc32, display = "target", col = "red", pos = 4, cex = 0.6)


# Comparison of the two PCAs in scaling 2
par(mfrow = c(1, 2))
biplot(env.pca, main = "Original PCA, scaling 2")
biplot(env.imp32.pca, main = "Imputed PCA, scaling 2")


