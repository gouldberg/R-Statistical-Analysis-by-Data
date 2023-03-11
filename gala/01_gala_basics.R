setwd("//media//kswada//MyFiles//R//gala")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gala
#   - There are 30 Galapagos islands and 7 variables in the dataset.
#   - Variables
#       - Species: number of species found on the island
#       - Area:  the area of the island (km2)
#       - Elevation:  the highest elevation of the island (m)
#       - Nearest:  the distance from the nearest island (km)
#       - Scruz:  the distance from Santa Cruz Island (km)
#       - Adjacent:  the area of the adjacent island (km2)
#       - Endemics:  number of endemic species
# ------------------------------------------------------------------------------

data("gala", package = "faraway")

str(gala)

dim(gala)


car::some(gala)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------



