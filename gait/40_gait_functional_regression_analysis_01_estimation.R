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
# Functional Regression Analysis
#   - How the hip motion is coupled to knee motion ?
# ------------------------------------------------------------------------------

( gaittime <- as.numeric(dimnames(gait)[[1]]) * 20 )

gaitrange <- c(0, 20)

gaitbasis <- create.fourier.basis(gaitrange, nbasis = 21)

harmaccelLfd <- vec2Lfd(c(0, (2*pi/20)^2, 0), rangeval=gaitrange)


# ---------
gaitfd <- smooth.basisPar(gaittime, gait, gaitbasis, Lfdobj=harmaccelLfd, lambda=1e-2)$fd

str(gaitfd)

names(gaitfd$fdnames) <- c("Normalized time", "Child", "Angle")

gaitfd$fdnames[[3]] <- c("Hip", "Knee")



# ----------
hipfd  <- gaitfd[,1]
kneefd <- gaitfd[,2]
ncurve <- dim(kneefd$coefs)[2]



# ----------
# set up the list of covariate objects
conbasis <- create.constant.basis(c(0, 20))
constfd  <- fd(matrix(1, 1, ncurve), conbasis)
xfdlist  <- list(constfd, hipfd)
# xfdlist   = list(const = rep(1, 39), hip = hipfd)



# define the functional parameter object for regression functions
betafdPar = fdPar(gaitbasis, harmaccelLfd)
betalist  = list(const = betafdPar, hip = betafdPar)



# ----------
gaitRegress = fRegress(kneefd, xfdlist, betalist)


gaitRegress


