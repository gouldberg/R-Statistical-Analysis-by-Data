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
#  - Transform abundances to presence-absence (1-0)
# ------------------------------------------------------------------------------

# Simple transformation
# Transfom abundances to presence-absence (1-0)

spe[1:5, 2:4]

spe.pa <- decostand(spe, method = "pa")

spe.pa[1:5, 2:4]



