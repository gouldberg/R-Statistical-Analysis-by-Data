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
# Compute pointwise eigenvalues of system
#   - For higher-dimensional systems, bifurcation diagram is no longer feasible.
#     Instead, we consider the pointwise eigenvalues of the system.
# ------------------------------------------------------------------------------

fdaarray = handwrit

fdatime  <- seq(0, 2300, len=1401)

fdarange <- c(0, 2300)



breaks = seq(0, 2300, length.out = 116)

norder = 6

fdabasis = create.bspline.basis(fdarange, norder = norder, breaks = breaks)



# ----------
# parameter object for coordinates
fdaPar = fdPar(fdabasis, int2Lfd(4), 1e-8)



# ----------
# coordinate functions and a list tontaining them
Xfd = smooth.basis(fdatime, fdaarray[,,1], fdaPar)$fd
Yfd = smooth.basis(fdatime, fdaarray[,,2], fdaPar)$fd


xfdlist = list(Xfd, Yfd)



# ----------
# basis and parameter object for weight functions

nbasis = 31

fdabasis2 = create.bspline.basis(fdarange, norder = norder, nbasis = nbasis)

fdafd2    = fd(matrix(0, nbasis, 2), fdabasis2)

pdaPar    = fdPar(fdafd2, 1, 1e-8)

pdaParlist = list(pdaPar, pdaPar)

bwtlist = list( list(pdaParlist,pdaParlist), list(pdaParlist,pdaParlist) )



# ----------
# do the second order pda
pdaList = pda.fd(xfdlist, bwtlist)



# ----------
# plot the results
eigres = eigen.pda(pdaList)



# -->
# Nonzero eigenvalues sometimes come in conjugate pairs.
# With nonzero imaginary parts, the system oscillates.
# When the real part of any eigenvaue is positive, the system experiences exponential growth or a growing oscillation.
# otherwise it is stable or decaying.

# Here, with the real parts of the eigenvalues staying close to zero, indicating that the writing is dominated by ellipsoidal motion.


