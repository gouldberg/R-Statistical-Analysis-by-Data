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

# ------------------------------------------------------------------------------
# Estimate the mean square error for a Kernel Smoothing
# ------------------------------------------------------------------------------

library(splancs)


# mse2d:  estimate the mean square error for a Kernel Smoothing
mserwq <- mse2d(as.points(coordinates(spred)), as.points(list(x=c(0,1,1,0), y=c(0,0,1,1))), 100, .15)


mserwq


# ----------
bwq <- mserwq$h[which.min(mserwq$mse)]


bwq



# ------------------------------------------------------------------------------
# Cross validated bandwidth selection for Kernel Density  (Gaussian Kernel)
# ------------------------------------------------------------------------------

mserw <- bw.diggle(as(spred, "ppp"))


mserw


# ----------
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



# -->
# The value that minimises MSE for the Gaussian kernel is 0.01981, but it should be noted that the curve is very flat around that point,
# which means that many other values of the bandwidth are plausible.

