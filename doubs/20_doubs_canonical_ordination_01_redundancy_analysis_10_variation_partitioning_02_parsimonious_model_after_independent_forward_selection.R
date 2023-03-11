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
# 2. Variation partitioning after forward selection of explanatory variables
# ------------------------------------------------------------------------------

# Separate forward selection in each subset of environmental variables
# If one wants to estimate how much of the variation of Y is explained jointly by two or more explanatory data sets
# (usually because they represent different categories of constrints),
# it is important to carry out forward selection separately on the sets of explanatory variables.


spe.chem <- rda(spe.hel, envchem)

R2a.all.chem <- RsquareAdj(spe.chem)$adj.r.squared

forward.sel(spe.hel, envchem, adjR2thresh = R2a.all.chem, nperm = 9999)



# ----------
spe.topo <- rda(spe.hel, envtopo)

R2a.all.topo <- RsquareAdj(spe.topo)$adj.r.squared

forward.sel(spe.hel, envtopo, adjR2thresh = R2a.all.topo, nperm = 9999)



# ----------
# Parsimonious subsets of explanatory variables, based on forward selections
names(envchem)
envchem.pars <- envchem[, c(4, 6, 7)]


names(envtopo)
envtopo.pars <- envtopo[, c(1, 2)]



# ----------
# Variation partitioning
( spe.part <- varpart(spe.hel, envchem.pars, envtopo.pars) )

plot(spe.part, digits = 2, bg = c("red", "blue"), Xnames = c("Chemistry", "Physiography"), id.size = 0.7)

plot(spe.part.all, digits = 2, bg = c("red", "blue"))


# -->
# Forward selection provides a parsimonious solution without sacrificing real explanatory power:
# the adjusted R^2 of the 3 partitionings are approximately equal


# Common fraction should NEVER be mistaken for an interaction term in ANOVA sense.
# Common fractions arise because explanatory variables in different sets are correlated.


