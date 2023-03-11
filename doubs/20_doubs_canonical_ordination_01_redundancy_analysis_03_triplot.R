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
# Triplots of the rda results (lc scores)
# 1.    sp:  species
# 2-1.  lc:  fitted site scores (linear combinations of explanatory variables)
# 2-2.  wa:  site scores in the species space (weighted averages in CCA or weighted sums in RDA)  --> default
# 3.    cn:  constriants (the explanatory variables)
# ------------------------------------------------------------------------------

graphics.off();  par(mfrow = c(1, 1));

plot(spe.rda, scaling = 1, display = c("sp", "lc", "cn"), main = "Triplot RDA spe.hel ~ env3 - scaling 1 - lc scores")

spe.sc1 <- scores(spe.rda, choices = 1:2, scaling = 1, display = "sp")

arrows(0, 0, spe.sc1[, 1] * 0.92, spe.sc1[, 2] * 0.92, length = 0, lty = 1, col = "red")

text(-0.75, 0.7, "a", cex = 1.5)



# -->
# The fitted site scores (lc) are strictly orthogonal linear combinations of the explanatory variables, representing clearly and exclusively what can be modelled
# using the situations because the "true" ordination diagram of RDA is the ordination of the Y-hat matrix of fitted values.
# On the other hand, wa (the site scores that are weighted sums of species) appear more robust to noise in the environmental variables.

# Site scores as linear combinations of the environmental variables
# the scores are multipled by 0.92 so that the arrows do not cover the names of the variables
# The bottom and lef-hand scales are for the objects and the response variables,
# the top and right-hand scales are for the explanatory varibles

# Oxygen (oxy), elevation (ele), nitrates (nit) and discharge (dis), as well as slope (mainly the level slo.very_steep) play an important role
# in the dispersion of the sites along the 1st axis.



# ------------------------------------------------------------------------------
# Triplots of the rda results: scaling 2
# ------------------------------------------------------------------------------

plot(spe.rda, display = c("sp", "lc", "cn"), main = "Triplot RDA spe.hel ~ env3 - scaling 2 - lc scores")

spe.sc2 <- scores(spe.rda, choices = 1:2, display = "sp")

arrows(0, 0, spe.sc2[, 1] * 0.92, spe.sc2[, 2] * 0.92, length = 0, lty = 1, col = "red")

text(-0.82, 0.55, "b", cex = 1.5)



# -->
# Three groups of fish species correlated with different sets of explanatory variables:
# the brown trout (Satr), Eurasian minnow (Phph) and stone loach (Babl) are found in the 1st half of the sites, and are correlated with high oxygen content and slope as well as high elevation.
# The bleak (Alal), roach (Ruru) and European chub (Sgce), on the opposite, are related to sites 23, 24, 25 characterized by high phosphates (pho), ammonium (amm) and biological oxygen demand (bod) levels.
# Most other species are bunched together away from these extremes. They show mostly shorter projections, indicating that they are either present over most portions of the river or related to
# intermediate ecologicasl conditions.



# ------------------------------------------------------------------------------
# Triplots of the rda results (wa scores):  Site scores as weighted averages (vegan's default)
# ------------------------------------------------------------------------------

# Scaling 1 :  distance triplot
plot(spe.rda,  scaling = 1,  main = "Triplot RDA spe.hel ~ env3 - scaling 1 - wa scores")

arrows(0, 0, spe.sc1[, 1] * 0.92,  spe.sc1[, 2] * 0.92,  length = 0,  lty = 1,  col = "red")



# Scaling 2 (default) :  correlation triplot
plot(spe.rda, main = "Triplot RDA spe.hel ~ env3 - scaling 2 - wa scores")

arrows(0, 0, spe.sc2[, 1] * 0.92, spe.sc2[, 2] * 0.92, length = 0, lty = 1, col = "red")


