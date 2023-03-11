setwd("//media//kswada//MyFiles//R//meuse")

packages <- c("dplyr", "maptools", "spdep", "RColorBrewer", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


pal = function(n = 9) brewer.pal(n, "Reds")



# ------------------------------------------------------------------------------
# data:  meuse.grid
#   - Usually, interpolation is done on a regular grid. For Meuse data set, coordinates of points on a regular grid
#     are already defined in the meuse.grid data.frame, and are converted into a SpatialPixelsDataFrame
# ------------------------------------------------------------------------------
library(sp)

data(meuse.grid)



# ----------
# This is data frame
str(meuse.grid)

car::some(meuse.grid)



# ----------
# Convert to SpatialPointsDataFrame
coordinates(meuse.grid) <- c("x", "y")


# Convert to SpatialPixelsDataFrame
meuse.grid <- as(meuse.grid, "SpatialPixelsDataFrame")

str(meuse.grid)



# ------------------------------------------------------------------------------
# Inverse Distance Weighted Interpolation
#
#   - apply weights for observations are computed according to their distance to the interpolation location
#     w(s(i)) = || s(i) - s(0) || ^ (-p)
#     with || || indicating Euclidean distance and p an inverse distance weighting power, defaulting to 2.
#     If s(0) coincides with an observation location, the observed value is returned to avoid infinite weights.
#   - The inverse distance power determines the degree to which the nearer point(s) are preferred over more distant points;
#     for large values IDW converges to the on-nearest-neighbout interpolation.
# ------------------------------------------------------------------------------
library(gstat)

( idw.out <- gstat::idw(zinc ~ 1, meuse, meuse.grid, idp = 2.5) )

as.data.frame(idw.out)[1:5,]


print(spplot(idw.out, col.regions = pal(), cuts = 8, colorkey=TRUE))



# -->
# Inverse distance interpolation results usually in maps that are very similar to kriged maps
# when a variogram with no or a small nugged it used.
# In contrast to krging, by only considering distances to the prediction location
# it ignores the spatial configuration of observations; this may lead to undesired effects if the observation locatinos are strongly clustered.

# Another difference is that weights are guaranteed to be between 0 and 1, resulting in interpolated values never
# outside the range of observed values.


