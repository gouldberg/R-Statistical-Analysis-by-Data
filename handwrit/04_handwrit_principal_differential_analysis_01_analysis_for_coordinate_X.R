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
# Principal Differential Analysis
#   - A third order equation forced by a constant and time is estimated for X and Y coordinates separately.
#   - Forcing with constant and time is required to allow for an arbitrary origin and the left-right motion in X.
# ------------------------------------------------------------------------------
# Define two basis systems for the derivative weight functions.
# One for a background analysis used as a baseline and using constant weight functions,
# and another using 125 basis functions that will be used to generate the equations.



# ------------------------------------------------------------------------------
# Preparation
# ------------------------------------------------------------------------------

fdaarray = handwrit
fdatime  <- seq(0, 2300, len=1401)
fdarange <- c(0, 2300)
cycle  <- seq(0, 2300, 119)
featureindex   <- c(3, 5, 7, 10, 13, 16, 19)


# ----------
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

fdafd$fdnames[[1]] <- "Milliseconds"
fdafd$fdnames[[2]] <- "Replications"
fdafd$fdnames[[3]] <- "Metres"


# -----------
fdameanfd  <- mean(fdafd)
fdamat     <- eval.fd(fdatime, fdafd)
fdameanmat <- apply(fdamat, c(1,3), mean)


# ----------
# evaluate 3 derivatives and its means
D1fdamat <- eval.fd(fdatime, fdafd, 1)
D2fdamat <- eval.fd(fdatime, fdafd, 2)
D3fdamat <- eval.fd(fdatime, fdafd, 3)
D1fdameanmat <- apply(D1fdamat, c(1,3), mean)
D2fdameanmat <- apply(D2fdamat, c(1,3), mean)
D3fdameanmat <- apply(D3fdamat, c(1,3), mean)



# ------------------------------------------------------------------------------
# ufdlist:  Set up the two forcing functions
#   - x(t) is univariate:  a list of functional data objectsthat act as external influences on the system
#   - x(t) is multivariate:  a two-level list of functional data objects, ufdlist[[i]] represents the list of input functions that affect euqation i
# ------------------------------------------------------------------------------
ufdlist <- vector("list", 2)


# constant forcing
constbasis <- create.constant.basis(fdarange)

constfd    <- fd(matrix(1, 1, 20), constbasis)

ufdlist[[1]] <- constfd               



# -----------
# time forcing
linbasis   <- create.monomial.basis(fdarange, 2)

lincoef    <- matrix(0, 2, 20)

lincoef[2,] <- 1

ufdlist[[2]] <- fd(lincoef, linbasis) 



# ------------------------------------------------------------------------------
# awtlist:  Set up the corresponding weight functions
#   - x(t) is univariate:  a list of functional parameter objects defining the coefficient functions for the inputs, this should be the same length as ufdlist and may be NULL
#   - x(t) is multivariate:  a list of lists functional parameter objects. awtlist[[i]][[j]] represents the coefficient of ufdlist[[i]][[j]] in equation i
# ------------------------------------------------------------------------------
constbasis <- create.constant.basis(fdarange)

constfd    <- fd(1, constbasis)

awtlist    <- vector("list", 2)

awtlist[[1]] <- fdPar(constfd)

awtlist[[2]] <- fdPar(constfd)



# ------------------------------------------------------------------------------
# xfdlist:  Define the variable
#   - x(t) is univariate:  a functional data object defining x(t)
#   - x(t) is multivariate:  a list of functional data objects, the ith entry defining xi(t)
# ------------------------------------------------------------------------------

# The basis functions will be B-splines, with a spline placed at each knot.
# One may question whether so many basis functions are required,
# but this decision is found to be essential for stable derivative estimation
# up to the third order at and near the boundaries.

# Order 7 was used to get a smooth third derivative, which requires penalizing the size of the 5th derivative,
# which in turn requires an order of at least 7.
# This implies norder + no. of interior knots = 1399 + 7 = 1406 basis functions.  

nbasis   <- 1406

norder   <-    7

fdabasis <- create.bspline.basis(fdarange, nbasis, norder)



# ----------
# The smoothing parameter value 1e8 was chosen to obtain a fitting error of about 0.5 mm,
# the known error level in the OPTOTRACK equipment.

fdafd  <- fd(array(0, c(nbasis, 20, 2)), fdabasis)

lambda <- 1e8

fdaPar <- fdPar(fdafd, 5, lambda)



# ----------
# Define the variable
# take x coordinate fdaarray[,,1]
fdafdX <- smooth.basis(fdatime, fdaarray[,,1], fdaPar)$fd

xfdlist <- vector("list", 1)

xfdlist[[1]] <- fdafdX



# ------------------------------------------------------------------------------
# bwtlist:  Set up derivative weight function with constant basis
#   - x(t) is univariate:  a list of functional parameter objects, the jth element of each should specify the basis and smoothing penalty for betaj(t)
#   - x(t) is multivariate:  a list of lists of lists of functional parameter objects.  bwtlist[[i]][[j]][[k]] should define the basis and smoothing penalty for betaijk(t)
# ------------------------------------------------------------------------------

constbasis <- create.constant.basis(fdarange)

bfd     <- fd(matrix(0, 1, 1), constbasis)


bwtlist <- vector("list", 3)
bwtlist[[1]] <- fdPar(bfd, 1, 0)
bwtlist[[2]] <- fdPar(bfd, 1, 0)
bwtlist[[3]] <- fdPar(bfd, 1, 0)



# ------------------------------------------------------------------------------
# Carry out principal differential analysis
# ------------------------------------------------------------------------------
pdaList <- pda.fd(xfdlist, bwtlist, awtlist, ufdlist)


# ----------
plot(pdaList)



# ------------------------------------------------------------------------------
# evaluate forcing functions
# ------------------------------------------------------------------------------

MSY <- mean(eval.fd(fdatime, pdaList$resfdlist[[1]]) ^ 2)

MSY



# ------------------------------------------------------------------------------
# bwtlist:  Update derivative weight function with the number of basis functions to 125
# ------------------------------------------------------------------------------

# Set the number of basis functions to 125,
# found to maximize the RSQ measure, and corresponding to DF above,
# the equivalent degrees of freedom in the smooth.

nbasis <- 125
wbasis125 <- create.bspline.basis(fdarange, nbasis)


bfd     <- fd(matrix(0,125,1), wbasis125)

bwtlist <- vector("list", 3)
bwtlist[[1]] <- fdPar(bfd, 1, 0)
bwtlist[[2]] <- fdPar(bfd, 1, 0)
bwtlist[[3]] <- fdPar(bfd, 1, 0)



# ------------------------------------------------------------------------------
# carry out principal differential analysis
# order of equation = 3
# ------------------------------------------------------------------------------

difeorder  <- 3
pdaList2 <- pda.fd(xfdlist, bwtlist, awtlist, ufdlist, difeorder)



# ----------
graphics.off()
par(mfrow = c(1,2))
plot(pdaList)
plot(pdaList2)



# ----------
# A list of length equal to the number of variables or equations.
# Each member is a functional data object giving the residual functions or forcing functions defined as the left side of the equation
# (the derivative of order m of a variable) minus the linear fit on the right side.
# The number of replicates for each residual functional object is the same as that for the variables
pdaList2$resfdlist[[1]][[1]]



# ----------
# A list array of the same dimensions as the corresponding argument, containing the estimated or fixed weight functions
# defining the system of linear differential equations
pdaList2$bwtlist



# ----------
# A list of the same dimensions as the corresponding argument.
# Each member is an estimated or fixed weighting function for a forcing function.
pdaList2$awtlist[[1]]
pdaList2$awtlist[[2]]




# ------------------------------------------------------------------------------
# Evaluate forcing functions
#   - Compute a squared multiple correlation measure of fit
# ------------------------------------------------------------------------------

# evaluate forcing functions
( resmat <- eval.fd(fdatime, pdaList2$resfdlist[[1]]) )



# ----------
# Used only with constant basis
# Uncomment this line when the constant basis is used, and comment it out otherwise.

# MSY = mean(mean(resmat^2))

MSE <- mean(resmat ^ 2)

RSQ <- (MSY - MSE) / MSY

RSQ



# ----------
graphics.off()
par(mfrow=c(1,1))
plot(pdaList2$resfdlist[[1]])



# ------------------------------------------------------------------------------
# Plot the weight functions
# ------------------------------------------------------------------------------
par(mfrow=c(3,1), ask=FALSE)

for (j in 1:3) plot(pdaList2$bwtlist[[j]]$fd, cex=1, ylab = paste("Weight function ", j - 1))



# ------------------------------------------------------------------------------
# Plot the second derivative weight, defining the period of a harmonic oscillator
# ------------------------------------------------------------------------------

b2vecX   <- eval.fd(fdatime, pdaList2$bwtlist[[2]]$fd)

b2meanX  <- mean(b2vecX)



# ----------
par(mfrow=c(1,1), pty="m")
plot(fdatime, b2vecX, type="l", cex=1.2, xlim=c(0, 2300), ylim=c(0, 6e-3))
abline(h = b2meanX, lty=3)

plotrange <- c(0, 6e-3)
for (k in 1:length(cycle)) abline(v = cycle[k], lty = 2)
for (j in 1:length(featureindex)) abline(v = cycle[featureindex[j]], lty = 1)



# ------------------------------------------------------------------------------
# Plot forcing functions
# ------------------------------------------------------------------------------

# display coefficients for forcing weight functions
pdaList2$awtlist[[1]]$fd$coefs

pdaList2$awtlist[[2]]$fd$coefs



# ----------
# plot all forcing functions
par(mfrow=c(1,1), pty="m")

matplot(fdatime, 1e9 * resmat, type = "l", cex = 1.2,
        xlim = c(0, 2300), ylim = c(-200, 200),
        xlab = "Milliseconds", ylab = "Meters/sec/sec/sec")

lines(fdatime, 1e9 * D3fdameanmat[,1], lty = 2)



# ----------
# plot the mean forcing function along with third deriv.
resmeanfd  <- mean(pdaList2$resfdlist[[1]])

resmeanvec <- eval.fd(fdatime, resmeanfd)

par(mfrow = c(1,1), pty="m")

plot(fdatime, 1e9 * resmeanvec, type = "l", cex = 1.2, col = 2,
     xlim = c(0, 2300), ylim = c(-200, 200),
     xlab = "Milliseconds", ylab = "Meters/sec/sec/sec")

lines(fdatime, 1e9 * D3fdameanmat[,1], lty = 2, col = 3)

