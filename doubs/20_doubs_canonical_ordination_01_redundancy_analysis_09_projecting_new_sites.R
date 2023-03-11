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
# Environmental reconstruction (calibration, bioindication) with RDA
#  - Projecting new sites in an RDA to estimate the values of explanatory variables
# ------------------------------------------------------------------------------

# New (fictitious) objects with fish abundances
# Variables(species) must match those in the original data set in name, number and order


# New site 1 is made from rounded means of species in sites 1 to 15
site1.new <- round(apply(spe[1:15, ], 2, mean))


# New site 2 is made from rounded means of species in sites 16 - 29
site2.new <- round(apply(spe[16:29, ], 2, mean))

( obj.new <- t(cbind(site1.new, site2.new)) )



# Hellinger transformation of the new sites
obj.new.hel <- decostand(obj.new, "hel")



# ----------
# Calibration
calibrate(spe.rda.pars, obj.new.hel)



# ----------
# Compare with real values at sites 7 to 9 and 22 to 24: 
env2[7:9, c(1, 9, 10)]

env2[22:24, c(1, 9, 10)]



