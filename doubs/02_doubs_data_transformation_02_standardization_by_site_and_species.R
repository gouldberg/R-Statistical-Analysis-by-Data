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
#  - Standardization by columns (species)  (MARGIN = 2)
# ------------------------------------------------------------------------------

# scale abundances by dividing them by the MAXIMUM value of each species
spe.scal <- decostand(spe, "max")

round(spe.scal[1:5, 2:4], 3)

apply(spe.scal, 2, max)

apply(spe.scal, 2, sum)



# ----------
# Scale abundances by dividing them by the species TOTALS (relative abundance by species)
# Note: here, override the default MARGIN = 1 argument of "total"

spe.relsp <- decostand(spe, "total", MARGIN = 2)

round(spe.relsp[1:5, 2:4], 3)

round(spe.scal[1:5, 2:4], 3)

apply(spe.relsp, 2, sum)

apply(spe.relsp, 2, max)



# ------------------------------------------------------------------------------
# Ecological Data Transformation
#  - Standardization by rows (site) (MARGIN = 1)
# ------------------------------------------------------------------------------

# scale abundances by dividing them by the site TOTALS (profiles of relative abundance by site)

spe.rel <- decostand(spe, "total")  # when "total", default MARGIN = 1

round(spe.rel[1:5, 2:4], 3)

round(spe / apply(spe, 1, sum), 3)[1:5, 2:4]

rowSums(spe.rel)

