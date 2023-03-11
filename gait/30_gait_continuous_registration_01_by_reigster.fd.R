setwd("//media//kswada//MyFiles//R//gait")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gait
# ------------------------------------------------------------------------------

data("gait", package = "fda")

str(gait)

dim(gait)



# ------------------------------------------------------------------------------
# Register the angular acceleration of the gait data
# ------------------------------------------------------------------------------

# compute the acceleration and mean acceleration
D2gaitfd <- deriv.fd(gaitfd, 2)

D2gaitmeanfd  <- mean.fd(D2gaitfd)



# ----------
# set up basis for warping function
nwbasis   <- 7

wbasis    <- create.bspline.basis(gaitrange, nwbasis, 3)

Warpfd    <- fd(matrix(0, nwbasis, 39), wbasis)

WarpfdPar <- fdPar(Warpfd)



# ----------
# register the functions (continuous registration)
regstr <- register.fd(y0fd = D2gaitmeanfd, yfd = D2gaitfd, WfdParobj = WarpfdPar, periodic = TRUE)

xfine        <- seq(0, 1, len=101)

D2gaitregfd  <- regstr$regfd

D2gaitregmat <- eval.fd(xfine, D2gaitregfd)

warpfd       <- regstr$Wfd

shift        <- regstr$shift

warpmat      <- eval.monfd(xfine, warpfd)

warpmat      <- warpmat / outer(rep(1,101), warpmat[101,])



# ----------
print(round(shift,1))

hist(shift)


