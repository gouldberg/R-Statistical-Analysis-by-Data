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
# RV coefficients between the pairs of groups and tests for significance
#  - RV coefficients:  the ratio of the total co-inertia to the square root of the product of the total inertias of the separate analysis
#    Ranged between 0 (independent) and 1 (homothetic), it measures the closeness between the two sets of points
#    derived from the separate ordinations of X and Y
#    For two simple variables x1 and x2, RV is the square of their Pearson correlation coefficient
# ------------------------------------------------------------------------------

library(FactoMineR)


# RV coefficients
rvp <- t3.mfa$group$RV

rvp




# ----------
# Test its significance in insert p-value to upper-left triangles
rvp[1, 2] <- coeffRV(spe.hel, scale(envtopo))$p.value

rvp[1, 3] <- coeffRV(spe.hel, scale(envchem))$p.value

rvp[2, 3] <- coeffRV(scale(envtopo), scale(envchem))$p.value


round(rvp[-4, -4], 6)



# -->
# lower-left triangles:  RV coefficients
# upper-left triangles:  p-values

# fish communities are mostly linked to the physiographic conditions (RV = 0.58),
# which are themselves partly linked to water chemistry (RV = 0.36)


