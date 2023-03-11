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



# ------------------------------------------------------------------------------
# Data Preparation
# ------------------------------------------------------------------------------

# Set aside the variable 'dfs' (distance from the source) for later use
dfs <- env[, 1]


# Remove the 'dfs' variable from the env data frame
env2 <- env[, -1]



# ----------
# Recode the slope variable (slo) into a factor (qualitative) variable to show how these are handled in the ordinations
slo2 <- rep(".very_steep", nrow(env))

slo2[env$slo <= quantile(env$slo)[4]] <- ".steep"

slo2[env$slo <= quantile(env$slo)[3]] <- ".moderate"

slo2[env$slo <= quantile(env$slo)[2]] <- ".low"

slo2 <- factor(slo2, levels = c(".low", ".moderate", ".steep", ".very_steep"))

table(slo2)



# ----------
# Create an env3 data frame with slope as a qualitative variable
env3 <- env2

env3$slo <- slo2



# ----------
# Create two subsets of explanatory variables
# Physiography (upstream-downstream gradient)

envtopo <- env2[, c(1 : 3)]
names(envtopo)


# Water quality
envchem <- env2[, c(4 : 10)]
names(envchem)

