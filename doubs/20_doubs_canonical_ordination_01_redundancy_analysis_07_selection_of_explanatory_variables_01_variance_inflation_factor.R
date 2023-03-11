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
# Redundancy analysis (RDA):  Selection of Explanatory Variables
#  - Seach for parsimony
#  - Seach for possible strong linear dependencies (correlations) among the explanatory variables in the RDA model, which could render the regression coeffs
#   of the explanatory variables in the model unstable.
#
#
# In RDA, forward selection is the method most often applied because it works even in cases where the number of explanatory variables is larger than (n-1).
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Variance inflation factors (VIF) in two RDAs
#   -  Ideally, VIFs above 10 should be at least examined, and avoied if possible.
#      But variables with high VIFs should generally not be manually removed before the application of a procedure of selection of variables,
#      since both may contribute significantly.
# ------------------------------------------------------------------------------

# For a matrix X containing quantitative variables only:
# vif <- diag(solve(cor(X)))


# ----------
# all environmental variables except dfs
spe.hel <- decostand(spe, "hellinger")

spe.rda <- rda(spe.hel ~ ., env3)

sort(vif.cca(spe.rda), decreasing = TRUE)



# ----------
# Partial RDA – physiographic variables only
spechem.physio <- rda(spe.hel, envchem, envtopo)

sort(vif.cca(spechem.physio), decreasing = TRUE)


