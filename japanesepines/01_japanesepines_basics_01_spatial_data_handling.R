setwd("//media//kswada//MyFiles//R//japanesepines")

packages <- c("dplyr", "spatstat", "maptools")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  japanesepines
#   - the Japanese black pine saplings, measured in a square sampling region in a natural forest
# ------------------------------------------------------------------------------

data("japanesepines", package = "spatstat")

class(japanesepines)

str(japanesepines)



# ----------
# convert to Spatial Points class
spjpines <- as(japanesepines, "SpatialPoints")

str(spjpines)

# Convert to unit square using the elide methods
spjpines1 <- elide(spjpines, scale=TRUE, unitsq=TRUE)



# ----------
data(redwoodfull, package = "spatstat")

class(redwoodfull)

spred <- as(redwoodfull, "SpatialPoints")



# ----------
data(cells, package = "spatstat")

class(cells)

spcells <- as(cells, "SpatialPoints")



# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------

# spatstat package uses ppp (spatial point pattern) object to store point patterns.
# In addition to the point coordinates, ppp objects include the boundary of the region where the point data have been observed,
# whilst sp classes do not, and it has to be stored separately.

summary(japanesepines)



# -->
# The summary shows the average intensity over the region of interest;
# this region, known as an observation window, is also reported in the summary;
# observation windows are stored in objects of class owin.
# In this case, the points have been scaled to the unit square already, but the size of the ampling square can be used to retrieve
# the actual measurements.

# Note that spatstat windows may be of several forms, here the window is a rectangle.



# -----------
# coerce a ppp object with a rectangular window to a SpatialPoints object,
# the point coordinates will by default be re-scaled to their original values

spjpines <- as(japanesepines, "SpatialPoints")

summary(spjpines)



# -----------
# We can get back to the unit square using the elide methods

spjpines1 <- elide(spjpines, scale=TRUE, unitsq=TRUE)

summary(spjpines1)



# -----------
# getting back to a ppp object

pppjap <- as(spjpines1, "ppp")

summary(pppjap)


