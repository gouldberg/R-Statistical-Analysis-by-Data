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
# Estimation of the Intensity for cells data
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Estimate the mean square error for a Kernel Smoothing
# ------------------------------------------------------------------------------

mserwq <- mse2d(as.points(coordinates(spcells)), as.points(list(x=c(0,1,1,0), y=c(0,0,1,1))), 100, .15)

bwq <- mserwq$h[which.min(mserwq$mse)]

bwq


# ------------------------------------------------------------------------------
# Cross validated bandwidth selection for Kernel Density  (Gaussian Kernel)
# ------------------------------------------------------------------------------

mserw <- bw.diggle(as(spcells, "ppp"))

bw <- as.numeric(mserw)

bw



# ------------------------------------------------------------------------------
# plot mean square erros by bandwidth
# ------------------------------------------------------------------------------

par(mfrow=c(1,2))

plot(mserwq$h, mserwq$mse, xlab="Bandwidth", ylab="MSE", type="l", ylim=c(-2,50), main="Quartic kernel")

i <- which.min(mserwq$mse)

points(mserwq$h[i], mserwq$mse[i])


plot(mserw, main="Gaussian kernel", xlab="Bandwidth", ylab="MSE")

points(attr(mserw, "h")[attr(mserw, "iopt")], bw)




# ------------------------------------------------------------------------------
# Kernel smoothing of point pattern by different bandwidth
# ------------------------------------------------------------------------------

poly <- as.points(list(x = c(0, 0, 1, 1), y = c(0, 1, 1, 0)))

sG_c <- Sobj_SpatialGrid(spcells, maxDim=100)$SG

grd_c <- slot(sG_c, "grid")

summary(grd_c)



# ----------
k0 <- spkernel2d(spcells, poly, h0=bw, grd_c)

k1 <- spkernel2d(spcells, poly, h0=.05, grd_c)

k2 <- spkernel2d(spcells, poly, h0=.1, grd_c)

k3 <- spkernel2d(spcells, poly, h0=.15, grd_c)


df <- data.frame(k0=k0, k1=k1, k2=k2, k3=k3) 

kernels_c <- SpatialGridDataFrame(grd_c, data=df)

summary(kernels_c)



# ------------------------------------------------------------------------------
# Kernel density
# ------------------------------------------------------------------------------

cc <- coordinates(kernels_c)

xy_c <- list(x=cc[,1], y=cc[,2])

k4 <- density(as(spjpines1, "ppp"), .5*bw, dimyx=c(100, 100), xy=xy_c)
kernels_c$k4 <- as(k4, "SpatialGridDataFrame")$v

k5 <- density(as(spjpines1, "ppp"), .5*.05, dimyx=c(100, 100), xy=xy_c)
kernels_c$k5 <- as(k5, "SpatialGridDataFrame")$v

k6 <- density(as(spjpines1, "ppp"), .5*.1, dimyx=c(100, 100), xy=xy_c)
kernels_c$k6 <- as(k6, "SpatialGridDataFrame")$v

k7 <- density(as(spjpines1, "ppp"), .5*.15, dimyx=c(100, 100), xy=xy_c)
kernels_c$k7 <- as(k7, "SpatialGridDataFrame")$v

summary(kernels_c)



# ------------------------------------------------------------------------------
# plot kernel density of point pattern
# ------------------------------------------------------------------------------

library(RColorBrewer)

gp <- brewer.pal(8, "Reds")

print(spplot(kernels_c, at=c(0, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000), col.regions=colorRampPalette(gp)(15)[1:12], 
             names.attr=c(paste("Q bw=",round(bw, digits=4), sep="", collapse=""),
                          "Q bw=0.05", "Q bw=0.1","Q bw=0.15", paste("G bw=", round(.5*bw, digits=4),
                                                                     sep="", collapse=""), "G bw=0.025", "G bw=0.05","G bw=0.075"), cex=0.7, colorkey=FALSE))

