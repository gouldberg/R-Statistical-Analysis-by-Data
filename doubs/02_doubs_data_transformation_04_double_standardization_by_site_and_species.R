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
#  - Double standardization by columns (species) and rows (sites)  (MARGIN = 1 and 2)
# ------------------------------------------------------------------------------

# Chi-square transformation
spe.chi <- decostand(spe, "chi.square")

apply(spe.chi, 1, vec.norm)

apply(spe.chi, 1, sum)

apply(spe.chi, 2, vec.norm)

apply(spe.chi, 2, sum)



# ----------
round(spe.rel[1:5, 2:4], 3)

round(spe.norm[1:5, 2:4], 3)

round(spe.hel[1:5, 2:4], 3)

round(spe.chi[1:5, 2:4], 3)



# ----------
# Note that site 8 where no species was found has values of 0 for 0/0 instead of NaN
# decostand produced values of 0 for 0/0 instead of NaN

round(spe.rel[7:9,], 3)

round(spe.norm[7:9,], 3)

round(spe.hel[7:9,], 3)

round(spe.chi[7:9,], 3)


