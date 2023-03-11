setwd("//media//kswada//MyFiles//R//factors_underlying_yield_in_two_fields")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Field 1 (Sand)
#  - Field 1 has the shape of a trapezoid about twice as long in the north-south direction as in the east-west direction
# ------------------------------------------------------------------------------
data.Set4.1 <- read.csv("//media//kswada//MyFiles//data//factors_underlying_yield_in_two_fields//Set4.196sample.csv", header = TRUE)

str(data.Set4.1)

xtabs(Sand ~ Row + Column, data = data.Set4.1)



# ----------
# The data for plotting with the function persp() must be in the form of a matrix
Sand <- matrix(nrow = 13, ncol = 7)
for (i in 1:nrow(data.Set4.1)) Sand[data.Set4.1$Row[i], data.Set4.1$Column[i]] <- data.Set4.1$Sand[i]

head(Sand)



# ------------------------------------------------------------------------------
# perspective plot
# ------------------------------------------------------------------------------
North <- 3 * 1:13
West <- 3 * 1:7
persp(North, West, Sand, theta = 30, phi = 20, zlim = c(0,45), scale = FALSE)


# --> 
# There is a strong north-south trend in sand content.
# Also, some interaction between x (east-west) and y (north-south) direction



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
# Create an sf polygon object describing the boundary of this field
# ------------------------------------------------------------------------------
# The UTM coordinates
W <- 592025
S <- 4270452
N <- 4271132
EN <- 592470
ES <- 592404


( coords.mat <- matrix(c(W, EN, ES, W, W, N, N, S, S, N), ncol = 2) )

coords.list <- list(coords.mat)


# create sfc object for boundary points
( coords.pol <- st_sfc(st_polygon(coords.list)) )


# create sf polygon object 
Set4.19697bdry_kw = st_sf(z = 1, coords.pol)


# we must assign a coordinate system to the boundary (use EPSC code)
# st_crs(Set42bdry) <- 32601


# ----------
Set4.19697bdry_kw

plot(Set4.19697bdry_kw)



# ------------------------------------------------------------------------------
# save as ESRI shapefile
# ------------------------------------------------------------------------------
st_write(Set4.19697bdry_kw, "Created_kw//Set419697bdry_kw.shp")



# ------------------------------------------------------------------------------
# Check and compare
# ------------------------------------------------------------------------------
Set4.1.bdry.sf <- st_read("//media//kswada//MyFiles//references//SpatialDataAnalysisInEcologyAndAgricultureUsingR//Created//Set419697bdry.shp")
Set4.1.bdry.sf.kw <- st_read("//media//kswada//MyFiles//R//factors_underlying_yield_in_two_fields//Created_kw//Set419697bdry_kw.shp")



