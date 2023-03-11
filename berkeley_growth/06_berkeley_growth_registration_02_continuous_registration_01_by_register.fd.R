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
# Continuous registration
#   - We may need registration methods that use the entire curves rather than their values at specified points, if landmarks are not visible in all curves.
#     A number of such methods have been developed, and the problem continues to be actively researched.
# ------------------------------------------------------------------------------

#  Set up a cubic spline basis for continuous registration
nwbasisCR = 15

norderCR  =  5

wbasisCR  = create.bspline.basis(c(1,18), nwbasisCR, norderCR)




# ----------
# Because the continuous registration process requires iterative numerical optimization techniques,
# we have to supply starting values for the coefficients defining the funtions W.
# We do this by using zeros in defining the initial functional data object
Wfd0CR    = fd(matrix(0, nwbasisCR, ncasef), wbasisCR)

lambdaCR  = 1
WfdParCR  = fdPar(fdobj = Wfd0CR, Lfdobj = 1, lambda = lambdaCR)




# ----------
# carry out the registration
# y0fd:  a functional data object defining one or more target functions for registering the functions in afgument yfd.
# yfd:  a functional data object defining the functions to be registered to target y0fd.
# WfdParobj:  a functional parameter object containing either a single function or the same number of functions as are caontained in yfd.
registerlistCR = register.fd(y0fd = accelmeanfdLM, yfd = accelfdLM, WfdParobj = WfdParCR)

accelfdCR = registerlistCR$regfd
WfdCR     = registerlistCR$Wfd



# ------------------------------------------------------------------------------
# plot landmark and continuously registered curves for the first 10 children
# ------------------------------------------------------------------------------

accelmeanfdLM10 = mean(accelfdLM[children])

accelmeanfdCR10 = mean(accelfdCR[children])



# ----------
op = par(mfrow=c(2,1), mar=c(2,2,2,2))

plot(accelfdLM[children], xlim=c(1,18), ylim=c(-4,3), lty=1, lwd=1, cex=2, xlab="Age (Years)", ylab="Acceleration (cm/yr/yr)")
lines(accelmeanfdLM10, col=1, lwd=2, lty=2)
lines(c(PGSctrmean, PGSctrmean), c(-4,3), lty=2, lwd=1.5)

plot(accelfdCR[children], xlim=c(1,18), ylim=c(-4,3), lty=1, lwd=1, cex=2, xlab="Age (Years)", ylab="Acceleration (cm/yr/yr)")
lines(accelmeanfdCR10, col=1, lwd=2, lty=2)
lines(c(PGSctrmean, PGSctrmean), c(-4,3), lty=2, lwd=1.5)

par(op)



# ------------------------------------------------------------------------------
# plot mean function
# ------------------------------------------------------------------------------

accelmeanfdUN = mean(accelfdUN)

accelmeanfdLM = mean(accelfdLM)

accelmeanfdCR = mean(accelfdCR)



# ----------
op = par(mfrow=c(2,1), mar=c(2,2,2,2))

plot(accelfdCR, xlim=c(1,18), ylim=c(-4,3), lty=1, lwd=1, cex=2, xlab="Age (Years)", ylab="Acceleration (cm/yr/yr)")
lines(accelmeanfdCR, col=1, lwd=2, lty=2)
lines(c(PGSctrmean, PGSctrmean), c(-4,3), lty=2, lwd=1.5)

plot(accelmeanfdCR, xlim=c(1,18), ylim=c(-3,1.5), lty=1, lwd=2, cex=1.2, xlab="Years", ylab="Height Acceleration")
lines(accelmeanfdLM, lwd=1.5, lty=1, col = "blue")
lines(accelmeanfdUN, lwd=1.5, lty=2)

par(op)



# -->
# Top panel:  the continuous registration of the landmark-registered height acceleration curves
# The vertical dashed line indicates the target landmark age used in the landmark registration.

# Bottom panel:  the mean of the continuously registered acceleration curves in heavy solid line
# while that of the landmark-registered curves is a light solid line.
# The light dashed line is the mean of the unregistered curves
