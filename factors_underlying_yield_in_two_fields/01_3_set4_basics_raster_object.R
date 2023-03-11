setwd("//media//kswada//MyFiles//R//factors_underlying_yield_in_two_fields")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# read tiff image as a raster stack
# ------------------------------------------------------------------------------
# Read the three bands of a tiff image as a raster stack
library(raster)

data.4.2.May <- brick("//media//kswada//MyFiles//data//factors_underlying_yield_in_two_fields//Set4.20596.tif")
data.4.2.May



# ------------------------------------------------------------------------------
# assign coordinate reference system
# ------------------------------------------------------------------------------
projection(data.4.2.May) <- CRS("+proj=utm +zone=10 +ellps=WGS84")

data.4.2.May



# ------------------------------------------------------------------------------
# plot raster object
# ------------------------------------------------------------------------------
# all three layers of the RasterBrick
plot(data.4.2.May)



# Only the first layger of the brick (infrated layer)
plot(data.4.2.May[[1]])



# ------------------------------------------------------------------------------
# plot raster object
# ------------------------------------------------------------------------------
# create an interpolated EC grid
library(gstat)
library(maptools)

data.Set4.2EC <- read.csv("//media//kswada//MyFiles//data//factors_underlying_yield_in_two_fields//Set4.2EC.csv", header = TRUE)

data.Set4.2EC$ID <- 1:nrow(data.Set4.2EC)



# ----------
# Establish the coordinates of the rectangular boundary
N <- 4267868
S <- 4267513
E <- 592867
W <- 592082
cell.size <- 10


# Create the interpolation grid
grid.xy <- expand.grid(x = seq(W, E, cell.size),  y = seq(N, S, -cell.size))

coordinates(grid.xy) <- ~x + y

gridded(grid.xy) <- TRUE

proj4string(grid.xy) <- CRS("+proj=utm +zone=10 +ellps=WGS84")

grid.xy


grid.ras <- raster(grid.xy)

grid.ras


# Select every (cell.size)th data value
data.vgm <- data.Set4.2EC[-which(data.Set4.2EC$ID %% cell.size != 0),]

coordinates(data.vgm) <- c("Easting", "Northing")

projection(data.vgm) <- projection(grid.ras)

EC.vgm <- variogram(ECto30 ~ 1, data.vgm)

EC.fit <- fit.variogram(EC.vgm, model = vgm(100000, "Sph", 700,10000))

plot(EC.vgm, EC.fit, col = "black") # Fig.1.2a



EC.krig <- gstat(formula = ECto30 ~ 1, locations = grid.xy, data = data.vgm, model = EC.fit)
EC.ras <- interpolate(grid.ras, EC.krig)
extent(EC.ras)
extent(data.4.2.May)
IR.4.2.May <- resample(data.4.2.May[[1]], EC.ras)
data.4.2.May.stack <- stack(IR.4.2.May, EC.ras)
plot(data.4.2.May.stack)
