setwd("//media//kswada//MyFiles//R//yellow-billed_cuckoo_habitat")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# read ESRI shapefile
# ------------------------------------------------------------------------------
data.Set1.landcover <- st_read("//media//kswada//MyFiles//data//yellow-billed_cuckoo_habitat//landcover.shp")

str(data.Set1.landcover)

plot(data.Set1.landcover)



# ------------------------------------------------------------------------------
# Convert to SpatialPointsDataFrame
# ------------------------------------------------------------------------------
library(maptools)


# Convert to SpatialPointsDataFrame
# The data in this object are contained in slots
data.Set1.landcover.sp <- as(data.Set1.landcover, "Spatial")

str(data.Set1.landcover.sp)



# ----------
class(slot(data.Set1.landcover.sp, "data")) 

slot(data.Set1.landcover.sp, "data")$VegType[1:10]

data.Set1.landcover.sp@data$VegType[1:10]


# ----------
# The Polygons slot contains a list with the geometrical information.
# It is in the form of a list, with on lie element per polygon.
data.Set1.landcover.sp@polygons[[1]]



lapply(data.Set1.landcover.sp@polygons, slot, "ID")



# ------------------------------------------------------------------------------
# extracting the label points (it is tricky)
# ------------------------------------------------------------------------------
y <- lapply(data.Set1.landcover.sp@polygons, slot, "labpt")

data.Set1.landcover.sp.loc <- matrix(0, length(y), 2)

for (i in 1:length(y)) data.Set1.landcover.sp.loc[i,] <- unlist(y[[i]])

data.Set1.landcover.sp.loc



# ------------------------------------------------------------------------------
# coordinates()
# ------------------------------------------------------------------------------
data.Set1.obs <- read.csv("//media//kswada//MyFiles//data//yellow-billed_cuckoo_habitat//obspts.csv", header = TRUE)
data.Set1.sp <- data.Set1.obs
class(data.Set1.sp)

data.Set1.sp



# ----------
# coordinates() converts an existing data from in to a sp object automatically
coordinates(data.Set1.sp) <- c("Easting", "Northing") 

class(data.Set1.sp)

data.Set1.sp


proj4string(data.Set1.sp) <- CRS("+proj=utm +zone=10 +ellps=WGS84")

