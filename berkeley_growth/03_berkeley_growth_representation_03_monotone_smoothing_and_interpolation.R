setwd("//media//kswada//MyFiles//R//berkeley_growth")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  berkeley growth
#   - The data from the Berkeley Growth Study (Tuddenham and Snyder, 1954).
# ------------------------------------------------------------------------------

data("growth", package = "fda")

str(growth)



# ----------
# 39 boys and 31 points
# a 31 by 39 numeric matrix giging the heights in centimeters of 39 boys at 31 ages
dim(growth$hgtm)


# 54 girls and 31 points
# a 31 by 54 numeric matrix giging the heights in centimeters of 54 girls at 31 ages
dim(growth$hgtf)



# ----------
head(growth$hgtm)


head(growth$hgtf)



# ----------
growth$age


# -->
# The ages are not equally spaced;
# There are four measurements while the child is one year old, annual measurements from two to eight years,
# followed by heights measured biannually



# ------------------------------------------------------------------------------
# Monotone smoothing and interpolations
#  - smooth.monotone that fits the data with a function of the form: f(x) = b_0 + b_1 D^{-1} exp W(x)
#    where  W  is a function defined over the same range as X,  W + ln b_1 = log Df and w = D W = D^2f/Df.
#  - The constant term b_0 in turn can be a linear combinations of covariates:  b_0 = zmat * c.
#  - The fitting criterion is penalized mean squared error:  PENSSE(lambda) = \sum [y_i - f(x_i)]^2 + \lambda * \int [L W(x)]^2 dx
#    where L is a linear differential operator defined in argument Lfdobj.
#    The function W(x) is expanded by the basis in functional data object
#  - Because the fit must be calculated iteratively, and because S-PLUS is so slow with loopy calculations, these fits are VERY slow.
# ------------------------------------------------------------------------------
# We use b-spline basis functions of order 6
# Knots are positioned at the ages of observation.

knots  <- growth$age

rng <- range(age)

nage <- length(age)

ncasem <- ncol(hgtm)

ncasef <- ncol(hgtf)



# ----------
norder <- 6

nbasis <- length(knots) + norder - 2

wbasis <- create.bspline.basis(rng, nbasis, norder, knots)



# ----------
# starting values for coefficient
cvec0 <- matrix(0, nbasis, 1)

Wfd0  <- fd(cvec0, wbasis)

Lfdobj    <- 3          #  penalize curvature of acceleration

lambda    <- 10^(-0.5)  #  smoothing parameter

growfdPar <- fdPar(Wfd0, Lfdobj, lambda)



# ----------
# Set up design matrix and wgt vector
zmat  <- matrix(1, nage, 1)

wgt   <- rep(1,nage)




# ------------------------------------------------------------------------------
# Smooth Males
# ------------------------------------------------------------------------------

cvecm <- matrix(0, nbasis, ncasem)

betam <- matrix(0, 2,      ncasem)

RMSEm <- matrix(0, 1,      ncasem)

attach(growth)

children <- 1:ncasem

for(icase in children){
  hgt     <- hgtm[, icase]
  smoothList <- smooth.monotone(age, hgt, growfdPar, zmat, conv=0.001, dbglev=0)
  Wfd     <- smoothList$Wfdobj
  beta    <- smoothList$beta
  Flist   <- smoothList$Flist
  cvecm[,icase] <- Wfd$coefs
  betam[,icase] <- beta
  hgthat <- beta[1] + beta[2] * monfn(age, Wfd)
  RMSE   <- sqrt(mean((hgt - hgthat)^2 * wgt) / mean(wgt))
  RMSEm[icase] <- RMSE
  print(paste(icase, "  ", round(Flist$f, digits = 4), "  ", round(RMSE, digits = 4)))
}



# ------------------------------------------------------------------------------
# Smooth Females:
# ------------------------------------------------------------------------------

cvecf <- matrix(0, nbasis, ncasef)

betaf <- matrix(0, 2, ncasef)

RMSEf <- matrix(0, 1, ncasef)

children <- 1:ncasef

for(icase in children){
  hgt    <- hgtf[, icase]
  smoothList <- smooth.monotone(age, hgt, growfdPar, zmat, conv=0.001, dbglev=0)
  Wfd     <- smoothList$Wfdobj
  beta    <- smoothList$beta
  Flist   <- smoothList$Flist
  cvecf[,icase] <- Wfd$coefs
  betaf[,icase] <- beta
  hgthat <- beta[1] + beta[2] * monfn(age, Wfd)
  RMSE   <- sqrt(mean((hgt - hgthat) ^ 2 * wgt) / mean(wgt))
  RMSEf[icase] <- RMSE
  print(paste(icase, "  ", round(Flist$f, digits = 4), "  ", round(RMSE, digits = 4)))
}


