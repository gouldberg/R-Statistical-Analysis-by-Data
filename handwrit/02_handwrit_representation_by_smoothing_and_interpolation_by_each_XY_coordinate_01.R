setwd("//media//kswada//MyFiles//R//handwrit")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  handwrit
#   - handwriting of "fda" of 20 smaples
# ------------------------------------------------------------------------------
data("handwrit", package = "fda")

str(handwrit)



# ------------------------------------------------------------------------------
# Plot raw data
# ------------------------------------------------------------------------------

par(mfrow=c(2,1), mar=c(2,2,2,2))

i <- 1

matplot(1:1401, 100 * handwrit[, i, 1], type="l", lty = "solid", las=1, xlab='', ylab='')

matplot(1:1401, 100 * handwrit[, i, 2], type="l", lty = "solid", las=1, xlab='', ylab='')



# ------------------------------------------------------------------------------
# define time values and smoothing and interpolation
# ------------------------------------------------------------------------------
# Define time values and order 6 spline basis with 105 basis functions
# This places a knot at every 23rd observation point, and is found to correspond closely to spline smoothing results.

fdabasis = create.bspline.basis(c(0, 2300), 105, 6)

fdatime = seq(0, 2300, len=1401)



# ----------
#  set up the functional data structure
fdafd = smooth.basis(fdatime, handwrit, fdabasis)$fd

fdafd$fdnames[[1]] = "Milliseconds"

fdafd$fdnames[[2]] = "Replications"

fdafd$fdnames[[3]] = list("X", "Y")



# ----------
# plot the data
op <- par(mfrow=c(2,1), mar=c(2,2,2,2))
plot(fdafd)
par(op)

