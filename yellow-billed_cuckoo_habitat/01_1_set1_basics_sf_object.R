setwd("//media//kswada//MyFiles//R//yellow-billed_cuckoo_habitat")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  yellow_billd_cuckoo_habitat  --  obspts.csv
#
#  - ID:  identification number of the location
#  - PresAbs:  1 if one or more birds were observed at this location and 0 otherwise
#  - Abund:  the estimated total number of birds observed
#  - Easting:  the UTM Easting in Zone 10N of the observation site
#  - Northing:  the UTM Northing
# ------------------------------------------------------------------------------
data.Set1.obs <- read.csv("//media//kswada//MyFiles//data//yellow-billed_cuckoo_habitat//obspts.csv", header = TRUE)

str(data.Set1.obs)



proj4string(data.Set1.obs) <- CRS("+proj=utm +zone=10 +ellps=WGS84")



# ------------------------------------------------------------------------------
# sf (simple features) object
#  - "feature" is used here in the sense of a GIS representation of a spatial entity
# ------------------------------------------------------------------------------
library(sf)

data.Set1.sf <- st_as_sf(data.Set1.obs, coords = c("Easting", "Northing"))



# ----------
# sf objects is still a data frame, but it has a geometry object attached to it.
str(data.Set1.sf)

str(data.Set1.sf$geometry)

data.Set1.sf$geometry



# ------------------------------------------------------------------------------
# assign map projection
# ------------------------------------------------------------------------------
# Projections are handled in the package sf in 2 ways.
# The first is by using the PROJ.4 Cartographic Projections library originally written by Gerald Evenden
# crs:  coordinate reference system
# UTM Zone 10 using the WGS84 ellipsoid.
st_crs(data.Set1.sf) <- "+proj=utm +zone=10 +ellps=WGS84"

st_crs(data.Set1.sf)



# ----------
# st_crs() accepts codes like epsg (a set of numeric codes created by the European Petroleum Survey Group)
st_crs(data.Set1.sf) <- 32601

st_crs(data.Set1.sf)



# ------------------------------------------------------------------------------
# boundary
# ------------------------------------------------------------------------------
# boundary points
N <- 4267873
S <- 4267483
E <- 592860
W <- 592080

N - S


( coords.mat <- matrix(c(W, E, E, W, W, N, N, S, S, N), ncol = 2) )

coords.list <- list(coords.mat)


# create sfc object for boundary points
coords.pol <- st_sfc(st_polygon(coords.list))


# create sf polygon object 
Set42bdry = st_sf(z = 1, coords.pol)


# we must assign a coordinate system to the boundary (use EPSC code)
st_crs(Set42bdry) <- 32601


# ----------
Set42bdry

plot(Set42bdry)



# ------------------------------------------------------------------------------
# read ESRI shapefile
# ------------------------------------------------------------------------------
data.Set1.landcover <- st_read("//media//kswada//MyFiles//data//yellow-billed_cuckoo_habitat//landcover.shp")

str(data.Set1.landcover)

plot(data.Set1.landcover)

