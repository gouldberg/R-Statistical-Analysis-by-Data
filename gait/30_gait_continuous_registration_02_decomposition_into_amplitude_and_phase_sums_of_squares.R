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
# Decomposition into amplitude and phase sums of squares
# ------------------------------------------------------------------------------

# xfd: a functional data object containing the unregistered curves.
# yfd: a functional data object containing the registered curves.
# hfd: a functional data object containing the strictly monotone warping functions.
# rng: we use the decomposition over range [0, 20]

AmpPhasList = AmpPhaseDecomp(xfd = D2gaitfd, yfd = D2gaitregfd, hfd = warpfd, rng = c(0, 20))



# measure of pure amplitude variation fo the extent that the registration has been successful
MS.amp      = AmpPhasList$MS.amp



# measure of how much phase variation has been removed from the yi's by registration
MS.pha      = AmpPhasList$MS.pha



# squared multiple correlation index of the proportion of the total variation due to phase
RSQRCR      = AmpPhasList$RSQR

CCR         = AmpPhasList$C

print(paste("Total MS =",     round(MS.amp+MS.pha,2), 
            "Amplitude MS =", round(MS.amp,2),
            "Phase MS =",     round(MS.pha,2)))

print(paste("R-squared =", round(RSQRCR,3), ",  C =", round(CCR,3)))



# -->
# R-squared: X.XXX
# C value is almost close to 1, indicating that covariation between deformation functions and the squared registered functions yi^2 are C - 1 = 0.01,
# so it is independent.


# -----------
# RSQR is:
MS.pha / (MS.amp + MS.pha)


