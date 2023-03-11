setwd("//media//kswada//MyFiles//R//doubs")

packages <- c("dplyr", "vegan", "RgoogleMaps", "googleVis", "gclus")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Doubs
#  - consists of 5 data frames, 3 of them containing a portion of the data used by Verneaux for his studies.
#  - collected at 30 sites along the Doubs River, which runs near the France-Switzerland border in the Jura Mountains.
#
#  - spe:  species (community) data frame (fish abundances), contains coded abundances of 27 fish species
#  - env:  environmental data frame, contains 11 variabels related to the fydrology, geomorphology and chemistry of the river
#  - spa:  spatial data frame (catrtesian coordinates), locations were coded in GPS angular coordinates (WGS84) and transformed into Cartesian coordinates by using geoXY() of package SoDA
#  - fishtraits:  functional traits of fish species, contains 4 quantitative variables and 6 binary variables describing the diet
#     values are taken from variaous sources, mainly fishbase.org
#  - latlong:  spatial data frame - Latitude and Longitude
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




# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

