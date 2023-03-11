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
# Triplot
#   Scaling 1
#     - projecting an object at righ angle on a quantitative explanatory variable approximates the position of the object along that variable.
#     - an object found near the point representing the centroid of a class of a qualitative explanatory variable is more likely to possess that
#       class of the variable
#     - distances among centroids of qualitative explanatory variables, and between centroids and individual objects, approxiamate X^2 distances.
#
#   Scaling 2
#     - optimum of a species along a quantitative environmental variable can be obtained by projecting the species at right angle on the variable
#     - species found near the centroid of a class of a qualitative environmental variable is likely to be found frequently (or in larger abundances)
#       in the sites possessing that class of the variable
#     - distances among centroids, and between centroids and individual objects, do not approximate X^2 distances.
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow = c(1,2))

# Note that the response variables (species) are represented by points and thus arrows are not available for them.



# ----------
# CCA scalng 1 biplot without species (using lc site scores)
# scaling 1 focuses on the distance relationships among sites, but the presence of species with extreme scores renders the plot
# difficult to interpret beyond trivialities.

plot(spe.cca, scaling = 1, display = c("lc", "cn"), main = "Biplot CCA spe ~ env3 - scaling 1")



# -->
# scaling 1:
# One can see two well-defined groups of sites, one linked to high elevation and very steep slope (sites 1-7 and 10)
# and another with the highest oxygen contents (sites 11 - 15).

# The remaining sites are distributed among vaarious conditions towards more eutrophic waters.

# Remember that this is a constrained ordination of the fish community data,
# not a PCA of the site environmental variables.



# ----------
plot(spe.cca, display = c("sp", "cn"), main = "Triplot CCA spe ~ env3 - scaling 2")



# -->
# scaling 2:
# show two groups of species
#  - Thth, Cogo and Teso linked to high oxygen concentrations
#  - Satr, Phph and Babl also linked to high oxygen concentration and to high elevation and very steep slope.

# Scer, Cyca, Titi, Eslu, Gogo, and Pefl are linked to high ammonium and phosphate concentrations,
# as well as high biological oxygen demand.

# most other species are linked to high nitrate concentrations, moderate to low slopes and high discharge.




