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
# Explanation of fraction labels (two, three and four explanatory matrices) with optional colours
# ------------------------------------------------------------------------------

par(mfrow = c(1, 3), mar = c(1, 1, 1, 1))

showvarparts(2, bg = c("red", "blue"))

showvarparts(3, bg = c("red", "blue", "yellow"))

showvarparts(4, bg = c("red", "blue", "yellow", "green"))




# ------------------------------------------------------------------------------
# 1. Variation partitioning with all explanatory variables (except dfs)
# ------------------------------------------------------------------------------

( spe.part.all <- varpart(spe.hel, envchem, envtopo) )



# ----------
graphics.off();  par(mfrow=c(1,1))

plot(spe.part.all, digits = 2, bg = c("red", "blue"))



# -->
# Plot of the partitioning results (correct values of the adjusted R squares)  (but the sizes of the circles in the Venn diagram are not to scale)
# The unique contribution of the chemical variables (0.241) is more than twice as large as that of physiography (0.112).
# The variation explained jointly by the 2 sets is also large (0.233)
# This indicates that the chemical and physiographic variables are intercorrelated.
# This is a good reason to make an effort towards parsimony, and to combine variation partitioning with forward selection.


