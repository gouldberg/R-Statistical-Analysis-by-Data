# setwd("//media//kswada//MyFiles//R//rikz")
setwd("//media//kswada//MyFiles//R//Mixed_effects_and_multilevel_model//rikz")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  RIKZ
#   - Marine benthic data from nine inter-tidal areas along the Dutch coast.
#   - The data were collected by the Dutch institute RIKZ in the summer of 2002. In each inter-tidal area (denoted by 'beach'),
#     five samples were taken, and the macro-fauna and abiotic variables were measured.
#   - The underlying question for these data is whether there is a relationship between species richness, exposure, and
#     NAP (the height of a sampling station compared to mean tidal level).
#     Exposure is an index composed of the following elements: wave actio, length of the surf zone, slope, grain size, and the depth of the anaerobic layer.
# ------------------------------------------------------------------------------

RIKZ <- read.table(file = "RIKZ.txt", header = TRUE)


str(RIKZ)


car::some(RIKZ)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

