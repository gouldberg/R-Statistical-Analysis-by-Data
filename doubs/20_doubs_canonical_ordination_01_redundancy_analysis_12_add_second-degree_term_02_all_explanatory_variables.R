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
# Polynomial RDA, second degree, with forward selection among all environmental variables
# ------------------------------------------------------------------------------

# polyvars() takes only the variables and their respective (orthogonal) quadratic terms.
source("./functions/polyvars.R")

env.square <- polyvars(env2, degr = 2)

names(env.square)



# ----------
# full second-degree terms 
spe.envsq.rda <- rda(spe.hel ~ ., env.square)

R2ad <- RsquareAdj(spe.envsq.rda)$adj.r.squared



# ------------------------------------------------------------------------------
# forward selection of second-degree terms
# ------------------------------------------------------------------------------

spe.envsq.fwd <- adespatial::forward.sel(spe.hel, env.square, adjR2thresh = R2ad)


spe.envsq.fwd



# ----------
# only selected terms
envsquare.red <- env.square[, sort(spe.envsq.fwd$order)]


( spe.envsq.fwd.rda <- rda(spe.hel ~., envsquare.red) )


RsquareAdj(spe.envsq.fwd.rda)


summary(spe.envsq.fwd.rda)



# -->
# The polynomial RDA is far less parsimoniouus, having retained nine explanatory variables.
# As a bonus, the adjusted R2 is now 0.7530, an important increase.

# Indeed, the linear and quadratic terms of four variables have been retained, those of ele, oxy, slo and amm.
# This means that among the species that respond to these variables, some of them do it in a linear way and others by showing a unimodal response
# (modelled by the second-degree term)



# ------------------------------------------------------------------------------
# Triplot using "lc" (model) site scores and scaling 2
# (interested primarily in the relationships among the species)
# ------------------------------------------------------------------------------

# Arrows of the tench (Titi) and squared oxygen content (oxy.2) point in opposite directions.
# The tench tends to be more abundant in places where oxygen concentration is intermediate.
graphics.off();  par(mfrow = c(1,1));

triplot.rda(spe.envsq.fwd.rda, site.sc = "lc", scaling = 2, plot.sites = FALSE, pos.env = 1, mult.arrow = 0.9, mult.spe = 0.9, mar.percent = 0)



