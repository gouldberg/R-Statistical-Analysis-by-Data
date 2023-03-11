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
# Goodness-of-fit
# ------------------------------------------------------------------------------

spe.good <- goodness(spe.rda)

head(spe.good)



# ------------------------------------------------------------------------------
# Select species with goodness-of-fit at least 0.6 in the ordination plane formed by axes 1 and 2
# ------------------------------------------------------------------------------

# Triplots with homemade function triplot.rda(), scalings 1 and 2

source("./functions/triplot.rda.R")

sel.sp <- which(spe.good[, 2] >= 0.6)


# ----------
# scaling 1 and 2
par(mfrow = c(2, 1))

triplot.rda(spe.rda, site.sc = "lc", scaling = 1, cex.char2 = 0.7, pos.env = 3, pos.centr = 1, mult.arrow = 1.1, mar.percent = 0.05, select.spe = sel.sp)

text(-0.92, 0.72, "a", cex = 2)

triplot.rda(spe.rda, site.sc = "lc", scaling = 2, cex.char2 = 0.7, pos.env = 3, pos.centr = 1, mult.arrow = 1.1, mar.percent = 0.05, select.spe = sel.sp)

text(-2.82, 2, "b", cex = 2)


