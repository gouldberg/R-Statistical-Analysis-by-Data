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
# B-spline basis smoothing with knots at age values and order 6
# ------------------------------------------------------------------------------

# A single call to smooth.basisPar would give us a cubic spline.  
# However, to get a smooth image of acceleration, we need a quintic spline (degree 5, order 6) 

hgtm = growth$hgtm

hgtf = growth$hgtf

rng = range(growth$age)

knots  <- growth$age



# ----------
norder <- 6

nbasis <- length(knots) + norder - 2

hgtbasis <- create.bspline.basis(rng, nbasis, norder, knots)




# ----------
# Smooth with penalizing the 4th derivative
# This gives a smoother estimate of the acceleration functions

Lfdobj <- 4

lambda <- 1e-2

growfdPar <- fdPar(hgtbasis, Lfdobj, lambda)

hgtmfd <- smooth.basis(growth$age, growth$hgtm, growfdPar)$fd

hgtffd <- smooth.basis(growth$age, growth$hgtf, growfdPar)$fd



# ------------------------------------------------------------------------------
# Male and Female smoothed curve
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow=c(1,2))

plot(hgtmfd, main = "Male smoothed curve")

plot(hgtffd, main = "Female smoothed curve")



