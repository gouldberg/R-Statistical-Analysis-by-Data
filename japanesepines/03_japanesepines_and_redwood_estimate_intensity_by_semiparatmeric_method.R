setwd("//media//kswada//MyFiles//R//japanesepines")

packages <- c("dplyr", "spatstat", "maptools")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  japanesepines
# ------------------------------------------------------------------------------
data("japanesepines", package = "spatstat")

str(japanesepines)

summary(japanesepines)


# convert to Spatial Points class
spjpines <- as(japanesepines, "SpatialPoints")

str(spjpines)

summary(spjpines)



# ------------------------------------------------------------------------------
# Plot point patterns (spatial distribution) re-scaled to fit in the unit square
# ------------------------------------------------------------------------------
# Convert to unit square using the elide methods
spjpines1 <- elide(spjpines, scale=TRUE, unitsq=TRUE)

summary(spjpines1)



# ----------
# Also for comparison, read and convert redwoodfull and cells data
data(redwoodfull)
spred <- as(redwoodfull, "SpatialPoints")

data(cells)
spcells <- as(cells, "SpatialPoints")



# ----------
summary(spjpines1)
summary(spred)
summary(spcells)



# ----------
dpp <- data.frame(rbind(coordinates(spjpines1), coordinates(spred),  coordinates(spcells)))

njap <- nrow(coordinates(spjpines1))
nred <- nrow(coordinates(spred))
ncells <- nrow(coordinates(spcells))

dpp <- cbind(dpp,c(rep("JAPANESE",njap), rep("REDWOOD", nred), rep("CELLS", ncells))) 

names(dpp) <- c("x", "y", "DATASET")

head(dpp)



# ----------
library(lattice)

# Spatial distribution of the location of cell centres, japanese black pine saplings, and saplings of California redwood trees
print(xyplot(y ~ x | DATASET, data=dpp, pch=19, aspect=1))



# ------------------------------------------------------------------------------
# Estimation of the Intensity:  selection of kernel bandwidth for redwood data
#
#  - We apply the approach proposed by Berman and Diggle (1989) which is implemented in functions splancs::mse2d() and spatstat::bw.diggle(),
#    based on the criterion of minimising the Mean Square Error (MSE) of the kernel smoothing estimator when the underlying point process in a stationary Cox process.
#
#  - Note that these two functions can provide different optimal bandwidths because they rely on kernel2d (which implements a quartic kernel) and
#    density (which implements a Gaussian kernel), respectively
#
#  - It must be noted that when estimating the intensity by kernel smoothing, the key choice is not that of the kernel function but the bandwidth.
#    Different kernels will produce very similar estimates for equivalent bandwidths, but hte same kernel with different bandwidths will produce
#    dramatically different results.
# ------------------------------------------------------------------------------
library(splancs)


mserwq <- mse2d(as.points(coordinates(spred)), as.points(list(x=c(0,1,1,0), y=c(0,0,1,1))), 100, .15)
bwq <- mserwq$h[which.min(mserwq$mse)]
bwq


mserw <- bw.diggle(as(spred, "ppp"))
bw <- as.numeric(mserw)
bw


# ----------
par(mfrow=c(1,2))
plot(mserwq$h, mserwq$mse, xlab="Bandwidth", ylab="MSE", type="l", ylim=c(-2,50), main="Quartic kernel")
i <- which.min(mserwq$mse)
points(mserwq$h[i], mserwq$mse[i])

plot(mserw, main="Gaussian kernel", xlab="Bandwidth", ylab="MSE")
points(attr(mserw, "h")[attr(mserw, "iopt")], bw)



# -->
# The value that minimises MSE for the Gaussian kernel is 0.01981, but it should be noted that the curve is very flat around that point,
# which means that many other values of the bandwidth are plausible.



# ------------------------------------------------------------------------------
# Estimation of the Intensity:  by different bandwidth for redwood data
#  - It must be noted that when estimating the intensity by kernel smoothing, the key choice is not that of the kernel function but the bandwidth.
#    Different kernels will produce very similar estimates for equivalent bandwidths, but hte same kernel with different bandwidths will produce
#    dramatically different results.
# ------------------------------------------------------------------------------
# Kernel smoothing using a quartic kernel can be performed with splancs::spkernel2d()

poly <- as.points(list(x = c(0, 0, 1, 1), y = c(0, 1, 1, 0)))
sG <- Sobj_SpatialGrid(spred, maxDim=100)$SG
grd <- slot(sG, "grid")
summary(grd)

k0 <- spkernel2d(spred, poly, h0=bw, grd)
k1 <- spkernel2d(spred, poly, h0=.05, grd)
k2 <- spkernel2d(spred, poly, h0=.1, grd)
k3 <- spkernel2d(spred, poly, h0=.15, grd)

df <- data.frame(k0=k0, k1=k1, k2=k2, k3=k3) 
kernels <- SpatialGridDataFrame(grd, data=df)

summary(kernels)



# ----------
# When calling density on a ppp object (which in fact calls density.ppp), we have used the additional arguments dimxy and xy to make sure that the grid
# used to compute the estimates is compatible with that stored in kernels.
# Finally, the kernel estimate is returned in an im class that is converted into a SpatialGridDataFrame and the values incorporated into kernels.
cc <- coordinates(kernels)
xy <- list(x=cc[,1], y=cc[,2])
k4 <- density(as(spred, "ppp"), .5*bw, dimyx=c(100, 100), xy=xy)
kernels$k4 <- as(k4, "SpatialGridDataFrame")$v

k5 <- density(as(spred, "ppp"), .5*.05, dimyx=c(100, 100), xy=xy)
kernels$k5 <- as(k5, "SpatialGridDataFrame")$v

k6 <- density(as(spred, "ppp"), .5*.1, dimyx=c(100, 100), xy=xy)
kernels$k6 <- as(k6, "SpatialGridDataFrame")$v

k7 <- density(as(spred, "ppp"), .5*.15, dimyx=c(100, 100), xy=xy)
kernels$k7 <- as(k7, "SpatialGridDataFrame")$v

summary(kernels)


library(RColorBrewer)
gp <- brewer.pal(8, "Reds")
print(spplot(kernels, at=c(0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000), col.regions=colorRampPalette(gp)(15)[1:12], 
             names.attr=c(paste("Q bw=",round(bw, digits=4), sep="", collapse=""),
                          "Q bw=0.05", "Q bw=0.1","Q bw=0.15", paste("G bw=", round(.5*bw, digits=4),
                                                                     sep="", collapse=""), "G bw=0.025", "G bw=0.05","G bw=0.075"), cex=0.7, colorkey=FALSE))



# ------------------------------------------------------------------------------
# Estimation of the Intensity for japanesepine data
# ------------------------------------------------------------------------------
mserwq <- mse2d(as.points(coordinates(spjpines1)), as.points(list(x=c(0,1,1,0), y=c(0,0,1,1))), 100, .15)
bwq <- mserwq$h[which.min(mserwq$mse)]
bwq


mserw <- bw.diggle(as(spjpines1, "ppp"))
bw <- as.numeric(mserw)
bw


# ----------
par(mfrow=c(1,2))
plot(mserwq$h, mserwq$mse, xlab="Bandwidth", ylab="MSE", type="l", ylim=c(-2,50), main="Quartic kernel")
i <- which.min(mserwq$mse)
points(mserwq$h[i], mserwq$mse[i])

plot(mserw, main="Gaussian kernel", xlab="Bandwidth", ylab="MSE")
points(attr(mserw, "h")[attr(mserw, "iopt")], bw)



# ----------
poly <- as.points(list(x = c(0, 0, 1, 1), y = c(0, 1, 1, 0)))
sG <- Sobj_SpatialGrid(spjpines1, maxDim=100)$SG
grd <- slot(sG, "grid")
summary(grd)

k0 <- spkernel2d(spjpines1, poly, h0=bw, grd)
k1 <- spkernel2d(spjpines1, poly, h0=.05, grd)
k2 <- spkernel2d(spjpines1, poly, h0=.1, grd)
k3 <- spkernel2d(spjpines1, poly, h0=.15, grd)

df <- data.frame(k0=k0, k1=k1, k2=k2, k3=k3) 
kernels <- SpatialGridDataFrame(grd, data=df)

summary(kernels)



# ----------
cc <- coordinates(kernels)
xy <- list(x=cc[,1], y=cc[,2])
k4 <- density(as(spjpines1, "ppp"), .5*bw, dimyx=c(100, 100), xy=xy)
kernels$k4 <- as(k4, "SpatialGridDataFrame")$v

k5 <- density(as(spjpines1, "ppp"), .5*.05, dimyx=c(100, 100), xy=xy)
kernels$k5 <- as(k5, "SpatialGridDataFrame")$v

k6 <- density(as(spjpines1, "ppp"), .5*.1, dimyx=c(100, 100), xy=xy)
kernels$k6 <- as(k6, "SpatialGridDataFrame")$v

k7 <- density(as(spjpines1, "ppp"), .5*.15, dimyx=c(100, 100), xy=xy)
kernels$k7 <- as(k7, "SpatialGridDataFrame")$v

summary(kernels)


library(RColorBrewer)
gp <- brewer.pal(8, "Reds")
print(spplot(kernels, at=c(0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000), col.regions=colorRampPalette(gp)(15)[1:12], 
             names.attr=c(paste("Q bw=",round(bw, digits=4), sep="", collapse=""),
                          "Q bw=0.05", "Q bw=0.1","Q bw=0.15", paste("G bw=", round(.5*bw, digits=4),
                                                                     sep="", collapse=""), "G bw=0.025", "G bw=0.05","G bw=0.075"), cex=0.7, colorkey=FALSE))

