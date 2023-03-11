setwd("//media//kswada//MyFiles//R//doubs")

packages <- c("dplyr", "vegan", "RgoogleMaps", "googleVis", "gclus")
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
? vegan::decostand



# ------------------------------------------------------------------------------
# Ecological Data Transformation
#  - Wisconsin standardization
# ------------------------------------------------------------------------------

# Abundances are first ranged by species maxima and the by site totals
spe.wis <- wisconsin(spe)

apply(spe.wis, 1, vec.norm)

apply(spe.wis, 1, sum)


round(spe.rel[1:5, 2:4], 3)

round(spe.wis[1:5, 2:4], 3)

round(spe.norm[1:5, 2:4], 3)

round(spe.hel[1:5, 2:4], 3)


