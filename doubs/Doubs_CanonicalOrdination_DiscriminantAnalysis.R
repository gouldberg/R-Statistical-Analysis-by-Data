# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
rm(list=ls())
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "vegan", "MASS", "rrcov")
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
# Remove the 'dfs' variable from the env data frame
env2 <- env[, -1]



# ------------------------------------------------------------------------------
# Linear discriminant analysis (LDA)
#
#  - We use the 4-group classification of sites based on the fish species, and try to determine to what extent the 3 environmental variables (ele, oxy, and bod) can actually explain this grouping.
#  - In this type of ananlysis, the grouping is known at the start of the analysis, the problem is in interpretation.
#  - Testing for differences among group means, in discriminant analysis, is identical to ANOVA for a single explanatory variable and to MANOVA for multiple variables (X).
#  - Discriminant analysis can handle several groups.
#
#  - DISCRIMINATION:  determine the relative contributions of various explanatory descriptors to the distinction among these states
#     - Coeffs of discriminant functions are used to assess the relatvie contributions of the descriptors to the final discrimination
#
#  - IDENTIFICATION:  obtain a linear equation to acclocate new objects to one of the stats of the classification criterion
#     - identification functions are computed from the original descriptors, used to compute the group to which a new object is most likely to belong
#
#  - Spherical within-group dispersions are obtained only if the condition of homogeneity of the within-group dispersion matrices is fulfilled.
#    Even if discriminant analysis is moderately robust to departures from this condition, it remains advisable to examine whether this condition is met prior to LDA.
#    Anderson's test of homogeneity of multivariate dispersions (which is robust to departures from normality) can be computed for any dissimilarity measure of choice, 
#    and is available in VEGAN's function betadisper()
#
#  - Several important tests in discriminant analysis are based on Wilk's lambda statistic.  This statistic can be used in an overall test to assess if the groups significantly
#    differ in the positions of their centroids, given the within-group dispersions.
# ------------------------------------------------------------------------------
# Hellinger-transform the species dataset
spe.hel <- decostand(spe, "hellinger")


# Ward clustering result of Hellinger-transformed species data, cut into 4 groups
gr <- cutree(hclust(vegdist(spe.hel, "euc"), "ward.D2"), k = 4)


# Environmental matrix with only 3 variables (ele, oxy and bod)
env.pars2 <- as.matrix(env2[, c(1, 9, 10)])


# Verify multivariate homogeneity of within-group covariance matrices using the betadisper() function {vegan}
env.pars2.d1 <- dist(env.pars2)
( env.MHV <- betadisper(env.pars2.d1, gr) )


# The within-group covariance matrices ARE NOT homogeneous.
anova(env.MHV)
permutest(env.MHV)	# Permutational test


# Log transform ele and bod
# This time the within-group covariance matrices are homogeneous.
env.pars3 <- cbind(log(env2$ele), env2$oxy, log(env2$bod))
colnames(env.pars3) <- c("ele.ln", "oxy", "bod.ln") 
rownames(env.pars3) <- rownames(env2)
env.pars3.d1 <- dist(env.pars3)
( env.MHV2 <- betadisper(env.pars3.d1, gr) )
permutest(env.MHV2)



# -------------------------------------------
# Test with Wilks' lambda that the explanatory vasriables have distinct means.
# Preliminary test :  do the means of the explanatory variable differ among groups?
# First way: with function Wilks.test() of package rrcov, χ2 test
rrcov::Wilks.test(env.pars3, gr)
# Second way: with function manova() of stats, which uses an F-test approximation
lw <-  manova(env.pars3 ~ as.factor(gr))
summary(lw, test = "Wilks")



# -------------------------------------------
# Computation of LDA - identification functions (on unstandardized variables)
# Alternate coding without formula interface:  spe.lda <- lda(env.pars3.df, gr)
# TECHNICAL NOTE:  Running the function with unstandardized explanatory variables will allow the classsification of new objects
# while producing the exact same discrimination as a run with standardized variables.
env.pars3.df <- as.data.frame(env.pars3)
( spe.lda <- lda(gr ~ ele.ln + oxy + bod.ln, data = env.pars3.df) )
summary(spe.lda)


# Display the group means for the 3 variables
spe.lda$means


# Extract the unstandardized identification functions (matrix C, eq. 11.33 in Legendre and Legendre 2012)
( C <- spe.lda$scaling )


# Classification of two new objects (identification)
# A new object is created with two sites: 
#     (1) ln(ele) = 6.8, oxygen = 9 and ln(bod) = 0.8 
# and (2) ln(ele) = 5.5, oxygen = 10 and ln(bod) = 1.0
newo <- data.frame(c(6.8, 5.5), c(9, 10), c(0.8, 1))
colnames(newo) <- colnames(env.pars3)
newo
( predict.new <- predict(spe.lda, newdata = newo) )


# Posterior probabilities of new objects (rows) to belong to groups 1-4 (columns)
# The 1st object (row 1) has the highest probability (0.88) to belong to group 2 and the second object to group 3
predict.new$posterior



# -------------------------------------------
# Computation of LDA - discrimination functions (on standardized variables)
env.pars3.sc <- as.data.frame(scale(env.pars3.df))
spe.lda2 <- lda(gr ~ ., data = env.pars3.sc)
spe.lda2$means


# Extract the classification functions
( C2 <- spe.lda2$scaling )


# Compute the canonical eigenvalues
spe.lda2$svd^2


# Position the objects in the space of the canonical variates
# alternative way :  Fp2 <- as.matrix(env.pars3.sc) %*% C2
( Fp2 <- predict(spe.lda2)$x )


# Classification of the objects
( spe.class2 <- predict(spe.lda2)$class )


# Posterior probabilities of the objects to belong to the groups
# (rounded for easier interpretation)
( spe.post2 <- round(predict(spe.lda2)$posterior, 2) )


# Contingency table of prior versus predicted classifications
( spe.table2 <- table(gr, spe.class2) )


# Proportion of correct classification (classification success)
diag(prop.table(spe.table2, 1))


# Plot the LDA results using the homemade function plot.lda()
# Some trial and error is needed to adjust the lengths of the arrows with argument mul.coef.
source("./RefData/NumericalEcologyWithR/NEwR2-Functions/plot.lda.R")
plot.lda(lda.out = spe.lda2, groups = gr, plot.sites = 2, plot.centroids = 1, mul.coef = 2.35)


# LDA with jackknife-based classification (i.e., leave-one-out cross-validation)
( spe.lda.jac <- lda(gr ~ ele.ln + oxy + bod.ln, data = env.pars3.sc, CV = TRUE) )
summary(spe.lda.jac)


# Numbers and proportions of correct classification
spe.jac.class <- spe.lda.jac$class
spe.jac.table <- table(gr, spe.jac.class)


# Classification success
# This is no as good as the result in spe.table2.
# However, spe.table2 shows an a posteriori classification of the objects that have been used in the computations, it is too optimistic.
diag(prop.table(spe.jac.table, 1))
diag(prop.table(spe.table2, 1))



# ------------------------------------------------------------------------------
# Example of Legendre and Legendre (2012, p. 683)
#  - 7 objects, allocated to 3 groups, are described by 2 descriptors.
#  - Normally, these data should not be submitted to discriminant analysis since the variances of the group matrices are not homogeneous.
# ------------------------------------------------------------------------------
grY <- c(1, 1, 1, 2, 2, 3, 3)
x1 <- c(1, 2, 2, 8, 8, 8, 9)
x2 <- c(2, 2, 1, 7, 6, 3, 3)
X <- as.data.frame(cbind(x1, x2))



# -------------------------------------------
# Test with Wilks' lambda that the explanatory vasriables have distinct means.
# Preliminary test :  do the means of the explanatory variable differ among groups?
# First way: with function Wilks.test() of package rrcov, χ2 test
rrcov::Wilks.test(X, grY)
# Second way: with function manova() of stats, which uses an F-test approximation
lw <-  manova(env.pars3 ~ as.factor(gr))
summary(lw, test = "Wilks")


# --> There are significant differences among the groups in the X variables.


# -------------------------------------------
# Computation of unstandardized identification functions
unstand.lda <- lda(grY ~ ., data = X)
unstand.lda


# Display the group means for the 3 variables
unstand.lda$means


# Compute the canonical eigenvalues:  proportion of trace
# Canonical axes 1 and 2 explain 93.13% and 6.87% of the among-group variation, respectively.
unstand.lda$svd^2
prop.table(unstand.lda$svd^2)


# disctiminant function coefficients:  Extract the unstandardized identification functions (matrix C, eq. 11.33 in Legendre and Legendre 2012)
# Matrix C contains the rescaled eigenvectors defining the canonical space
( C <- unstand.lda$scaling )


# Canonical variates:  Position the objects in the space of the canonical variates = X centered * C
( Fp <- predict(unstand.lda)$x )


# Classification of the objects
( class_unstand <- predict(unstand.lda)$class )


# Posterior probabilities of the objects to belong to the groups
# (rounded for easier interpretation)
round(predict(unstand.lda)$posterior, 2)



# -------------------------------------------
# Computation of standardized discriminant functions
X.sc <- as.data.frame(scale(X))
stand.lda <- lda(grY ~ ., data = X.sc)
stand.lda
stand.lda$means


# Display the group means for the 3 variables
stand.lda$means
unstand.lda$means


# Compute the canonical eigenvalues:  proportion of trace
# Canonical axes 1 and 2 explain 93.13% and 6.87% of the among-group variation, respectively.
stand.lda$svd^2
prop.table(stand.lda$svd^2)


# disctiminant function coefficients:  Extract the standardized identification functions (matrix C, eq. 11.33 in Legendre and Legendre 2012)
# Matrix C contains the rescaled eigenvectors defining the canonical space
( C2 <- stand.lda$scaling )
C
C2


# Canonical variates:  Position the objects in the space of the canonical variates = X centered * C2
# The raw and standardized data produce exactly the same ordination of the objects and group centroids
( Fp2 <- predict(stand.lda)$x )
Fp2
Fp


# Classification of the objects
( class_stand <- predict(stand.lda)$class )


# Posterior probabilities of the objects to belong to the groups
# (rounded for easier interpretation)
round(predict(stand.lda)$posterior, 2)
