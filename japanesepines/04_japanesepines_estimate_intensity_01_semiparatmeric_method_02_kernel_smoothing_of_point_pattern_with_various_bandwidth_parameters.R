setwd("//media//kswada//MyFiles//R//japanesepines")

packages <- c("dplyr", "spatstat", "maptools")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  japanesepines
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



# ----------
summary(spjpines1)

summary(spred)

summary(spcells)



# ------------------------------------------------------------------------------
# Kernel smoothing of point pattern of redwood by different bandwidth
#  - It must be noted that when estimating the intensity by kernel smoothing, the key choice is not that of the kernel function but the bandwidth.
#    Different kernels will produce very similar estimates for equivalent bandwidths, but the same kernel with different bandwidths will produce
#    dramatically different results.
# ------------------------------------------------------------------------------


# set spatial grids
poly <- as.points(list(x = c(0, 0, 1, 1), y = c(0, 1, 1, 0)))

sG <- Sobj_SpatialGrid(spred, maxDim=100)$SG

grd <- slot(sG, "grid")

summary(grd)



# ----------
# Kernel smoothing of point pattern using a quartic kernel can be performed with splancs::spkernel2d()
# h0:  kernel width parameter
# returns a numeric vector with the value of the kernel function stored in the order required by sp package SpatialGridDataFrame objects

k0 <- spkernel2d(spred, poly, h0=bw, grd)

k1 <- spkernel2d(spred, poly, h0=.05, grd)

k2 <- spkernel2d(spred, poly, h0=.1, grd)

k3 <- spkernel2d(spred, poly, h0=.15, grd)


k3


# ----------
df <- data.frame(k0=k0, k1=k1, k2=k2, k3=k3) 


head(df)



# ----------
kernels <- SpatialGridDataFrame(grd, data=df)


summary(kernels)


