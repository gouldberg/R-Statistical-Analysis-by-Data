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
# Apply:  norder = 7 and lambda = 1e8
# ------------------------------------------------------------------------------
fdaarray = handwrit

fdatime  <- seq(0, 2300, len=1401)

fdarange <- c(0, 2300)



# The basis functions will be B-splines, with a spline placed at each knot.
# One may question whether so many basis functions are required, but this decision is found to be essential
# for stable derivative estimation up to the third order at and near the boundaries.

# Order 7 was used to get a smooth third derivative, which requires penalizing the size of the 5th derivative,
# which in turn requires an order of at least 7.
# This implies norder + no. of interior knots = 1399 + 7 = 1406  basis functions.  

nbasis   <- 1406

norder   <-    7

fdabasis <- create.bspline.basis(fdarange, nbasis, norder)



# ----------
# The smoothing parameter value 1e8 was chosen to obtain a fitting error of about 0.5 mm, the known error level in
# the OPTOTRACK equipment.

fdafd  <- fd(array(0, c(nbasis, 20, 2)), fdabasis)

lambda <- 1e8

fdaPar <- fdPar(fdafd, 5, lambda)



# ----------
# set up the functional data structure
smoothList <- smooth.basis(fdatime, fdaarray, fdaPar)

fdafd <- smoothList$fd 

df    <- smoothList$df 

gcv   <- smoothList$gcv



# Add suitable names for the dimensions of the data.
fdafd$fdnames[[1]] <- "Milliseconds"
fdafd$fdnames[[2]] <- "Replications"
fdafd$fdnames[[3]] <- "Metres"



# display degrees of freedom and total GCV criterion
df  #  about 115

totalgcv <- sum(gcv)

totalgcv

RMSgcv <- sqrt(totalgcv) * 1000 # about 0.3 mm

RMSgcv



# ----------
# plot the fit to the data
par(mfrow=c(2,1))
plotfit.fd(fdaarray, fdatime, fdafd)



# ----------
# plot all curves
par(mfrow=c(2,1), ask=FALSE)
plot(fdafd)



