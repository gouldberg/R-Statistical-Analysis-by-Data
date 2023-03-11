setwd("//media//kswada//MyFiles//R//mack")

packages <- c("dplyr", "lattice", "gamair")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# This tutorial is based on
# Chapter 7. GAMs in Practive: mgcv from "Generalized Additive Models An Introduction with R 2nd Edition" (by Simon N. Wood)


# ------------------------------------------------------------------------------
# data:  mack
#   - Data from 1992 mackerel egg survey. The data were first modelled using GAMs by Borchers et al. (1997)
#   - As well as longitude and latitude, a number of other possible predicttors of egg abundance are available:
#        - salinity
#        - temp.surf: surface temperature of the ocean
#        - temp.20m:  water temperature at a depth of 20m
#        - b.depth:  depth of the ocean
#        - c.dist:  distance from the 200m seabed contour
#            --> this predictor reflects the biologists' belief that the fish like to spawn near the edge of the continental shelf, conventionally considered to end ata seabed depth of 200m.
#   - egg.count as response variable
#        - At each sampling location, a net was hauled vertically through the water column from well below the depth at which eggs are found to the surface:
#          the mackerel eggs caught in the net were counted     
# ------------------------------------------------------------------------------

data(mack, package = "gamair")

data(mackp, package = "gamair")

data(coast, package = "gamair")


str(mack)

str(mackp)

str(coast)



# ----------
# mackp contains prediction grid data for 1992 mackerel egg model.
# A data from with 5 columns. Each row corresponds to one spatial location within the survey area.

# coast:  European coastline from -11 to 0 East and from 43 to 59 North



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
