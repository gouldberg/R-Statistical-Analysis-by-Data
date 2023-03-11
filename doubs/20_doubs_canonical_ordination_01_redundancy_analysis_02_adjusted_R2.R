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
# Redundancy analysis (RDA)
#  - RDA is a method combining regression and principal component analysis.
#    It is a direct extension of multiple regression analysis to model multivariate response data.
#    Conceptually, RDA is a multivariate (multiresponse) multiple linear regression followed by a PCA of the matrix of fitted values.
#
#  - NOTE that rda(Y, X, W) syntax does not allow qualitative variables of class "factor" to be included in the explanatory and covariable matrices.
#    Therefore, in all but the simpelst applications, it is better to use the formula syntax such as rda(Y ~ var1 + factorA)
# ------------------------------------------------------------------------------

# Hellinger-transform the species dataset
spe.hel <- decostand(spe, "hellinger")



# ----------
head(spe.hel)

head(env3)


# RDA of the Hellinger-transformed fish species data, constrained by all the environmental variables contained in env3
( spe.rda <- rda(spe.hel ~ ., env3) )



# ------------------------------------------------------------------------------
# summary and adjusted R^2
# ------------------------------------------------------------------------------

# default: scale = FALSE (RDA on a covariance matrix) and scaling = 2
summary(spe.rda)


# scaling 1
# summary(spe.rda, axes = 0)



# -->
# Note that 1st residual eigenvalue (PC1) is larger than the last canonical eigenvalue,
# meaning that the 1st residual structure (axis) of the data has more variance than
# some of the structures that can be explained by the explanatory variables in X



# ------------------------------------------------------------------------------
# Adjusted R^2:
# = 1 - (n-1)/(n-m-1) * (1 - R^2)
# n:  the number of objects = nrow(spe) = 29,  m: the number of degrees of freedom of the model = the rank of the explanatory matrix = ncol(env3) + 3 - 1 = 12
# (the variable slo is qualitative with 4 factors)
# As a rule of thumb, this adjustment may be overly conservative when m > n/2
# ------------------------------------------------------------------------------

( R2adj <- RsquareAdj(spe.rda)$adj.r.squared )



# Unadjusted R^2 retrieved from the rda object
( R2 <- RsquareAdj(spe.rda)$r.squared )


# Adjusted R^2 retrieved from the rda object
( R2adj <- RsquareAdj(spe.rda)$adj.r.squared )



# -->
# The first two canonical axes explain together 56.1% of the total variance of the response data, 
# the 1st axis alone explaning 45.4%, but these values are unadjusted.
# Since the adjusted R^2 of the RDA^2 is 0.5224,
# the proportions of acumulated constrained eigenvalues show that the 1st axis alone explaines = 0.5224 * 0.6243 = 32.6% variance.
# and the 1st two axes together = 0.5224 * 0.7712 = 40.3% variance. 
# --> We can be CONFIDENT that the major trends have been well modelled in this analysis.



# ------------------------------------------------------------------------------
# Canonical coefficients
# ------------------------------------------------------------------------------

# Canonical coefficients from the rda object
coef(spe.rda)

