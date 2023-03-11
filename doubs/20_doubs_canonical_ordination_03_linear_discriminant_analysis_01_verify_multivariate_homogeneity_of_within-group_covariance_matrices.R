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
# latlong <- latlong[-8,]


# ----------
# data preparation
dfs <- env[, 1]
env2 <- env[, -1]
slo2 <- rep(".very_steep", nrow(env))
slo2[env$slo <= quantile(env$slo)[4]] <- ".steep"
slo2[env$slo <= quantile(env$slo)[3]] <- ".moderate"
slo2[env$slo <= quantile(env$slo)[2]] <- ".low"
slo2 <- factor(slo2, levels = c(".low", ".moderate", ".steep", ".very_steep"))
env3 <- env2
env3$slo <- slo2
envtopo <- env2[, c(1 : 3)]
envchem <- env2[, c(4 : 10)]



# ------------------------------------------------------------------------------
# Linear discriminant analysis (LDA)
#  - LDA tries to determine to what extent an independent set of quantitative variables can explain this grouping.
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

# ------------------------------------------------------------------------------
# Verify multivariate homogeneity of within-group covariance matrices
# ------------------------------------------------------------------------------

# Hellinger-transform the species dataset
spe.hel <- decostand(spe, "hellinger")



# Ward clustering result of Hellinger-transformed species data, cut into 4 groups
gr <- cutree(hclust(vegdist(spe.hel, "euc"), "ward.D2"), k = 4)



# Environmental matrix with only 3 variables (ele, oxy and bod)
env.pars2 <- as.matrix(env2[, c(1, 9, 10)])



env.pars2.d1 <- dist(env.pars2)



# ----------
# Verify multivariate homogeneity of within-group covariance matrices using the betadisper() function {vegan}
# null: the multivariate group dispersion matrices are homogeneous
( env.MHV <- betadisper(env.pars2.d1, gr) )



# ----------
anova(env.MHV)

permutest(env.MHV)	# Permutational test



# -->
# The within-group covariance matrices ARE NOT homogeneous.

