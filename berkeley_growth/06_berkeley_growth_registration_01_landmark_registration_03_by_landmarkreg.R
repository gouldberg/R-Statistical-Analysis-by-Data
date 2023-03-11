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
# Landmark registration automatically by landmarkreg()
# ------------------------------------------------------------------------------
#  We use the minimal basis function sufficient to fit 3 points
#  remember that the first coefficient is set to 0, so there are three free coefficients, and the data are two boundary
#  values plus one interior knot.
#  Suffix LM means "Landmark-registered".

PGSctr

PGSctrmean = mean(PGSctr)



# ----------
#  Define the basis for the function W(t).
wbasisLM = create.bspline.basis(c(1,18), 4, 3, c(1, PGSctrmean, 18))

WfdLM    = fd(matrix(0,4,1), wbasisLM)

WfdParLM = fdPar(WfdLM, 1, 1e-12)



# ----------
# Carry out landmark registration.
# monwrd = TRUE:  we requires the warping functions to themselves be strictly monotone functions
regListLM = landmarkreg(fdobj = accelfdUN, ximarks = PGSctr, x0marks = PGSctrmean, WfdPar = WfdParLM, monwrd = TRUE)

accelfdLM     = regListLM$regfd

accelmeanfdLM = mean(accelfdLM)



# ----------
# warping function and w-functions
regListLM$warpfd

regListLM$Wfd



# ------------------------------------------------------------------------------
# plot registered curves
# ------------------------------------------------------------------------------

plot(accelfdLM, xlim=c(1,18), ylim=c(-4,3), lty=1, lwd=1, cex=2, xlab="Age", ylab="Acceleration (cm/yr/yr)")

lines(accelmeanfdLM, col=1, lwd=2, lty=2)

lines(c(PGSctrmean, PGSctrmean), c(-4,3), lty=2, lwd=1.5)




# ------------------------------------------------------------------------------
# compare unregistered and registered curves
# ------------------------------------------------------------------------------

accelmeanfdUN10 = mean(accelfdUN[children])

accelmeanfdLM10 = mean(accelfdLM[children])



# ----------
op = par(mfrow=c(2,1), mar = c(2,2,2,2))

plot(accelfdUN[children], xlim=c(1,18), ylim=c(-3,1.5), lty=1, lwd=1, cex=2, xlab="", ylab="Acceleration (cm/yr/yr)")
lines(accelmeanfdUN10, col=1, lwd=2, lty=2)
lines(c(PGSctrmean, PGSctrmean), c(-3,1.5), lty=2, lwd=1.5)

plot(accelfdLM[children], xlim=c(1,18), ylim=c(-3,1.5), lty=1, lwd=1, cex=2, xlab="Age (Years)", ylab="Acceleration (cm/yr/yr)")
lines(accelmeanfdLM10, col=1, lwd=2, lty=2)
lines(c(PGSctrmean, PGSctrmean), c(-3,1.5), lty=2, lwd=1.5)

par(op)



# -->
# We see that the curves are now exactly aligned at the mean PGS (pubertal growth spurt) age,
# but that there is stiall some misalignment for the maximum and minimum acceleration ages.

# Girl7's acceleration minimum is substantially later than the others and who has still not reached zero acceleration by age 18.
# The long period of near zero acceleration for girl four prior to puberty also stands out as unusual.
# The mean curve is now much more satisfactory as a summary of the typical shape of growth acceleration curves.


