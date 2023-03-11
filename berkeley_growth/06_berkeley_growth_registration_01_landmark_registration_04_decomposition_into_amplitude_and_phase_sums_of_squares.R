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
# Decomposition into amplitude and phase sums of squares
# ------------------------------------------------------------------------------

# xfd: a functional data object containing the unregistered curves.
# yfd: a functional data object containing the registered curves.
# hfd: a functional data object containing the strictly monotone warping functions.
# rng: we use the decomposition over only the years from 3 to 18 years.

AmpPhasList = AmpPhaseDecomp(xfd = accelfdUN, yfd = accelfdLM, hfd = warpfdLM, rng = c(3, 18))



# measure of pure amplitude variation fo the extent that the registration has been successful
MS.amp      = AmpPhasList$MS.amp



# measure of how much phase variation has been removed from the yi's by registration
MS.pha      = AmpPhasList$MS.pha



# squared multiple correlation index of the proportion of the total variation due to phase
RSQRLM      = AmpPhasList$RSQR


CLM         = AmpPhasList$C


print(paste("Total MS =",     round(MS.amp+MS.pha,2), 
            "Amplitude MS =", round(MS.amp,2),
            "Phase MS =",     round(MS.pha,2)))


print(paste("R-squared =", round(RSQRLM,3), ",  C =", round(CLM,3)))




# -->
# After landmark registration of the growth acceleration curves yilds the value R^2 = 0.733, that is nearly 70% of the variation in acceleration
# over this period is due to phase.


# -----------
# RSQR is:
MS.pha / (MS.amp + MS.pha)


