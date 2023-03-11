setwd("//media//kswada//MyFiles//R//ceaq")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  CEAQ
# ------------------------------------------------------------------------------

data("CEAQ", package = "MPsychoR")


str(CEAQ)


car::some(CEAQ)



# ----------
# set the lowest category to zero in order to make it eRm compaatible
itceaq <- CEAQ[,1:16] - 1



# ----------
library(psych)

polcor <- polychoric(itceaq)


polcor

polcor$rho



# ------------------------------------------------------------------------------
# itemfit check by computing a X^2-based itemfit statistic
#   - X^2 statistics exhibit inflated Type I error rates.
#   - MSQ (Mean Square Fit):  related to the amount of misfit in the original data, should be within [0.7, 1.3] interval
#   - Outfit MSQ:  dividing the X^2 value by corresponding df.
#   - Infit MSQ:  Due to their sensitivity to outlying scores, a modified statistic called infit is typically prefered.
#   - Both infit and outfit can be standardized t-values which should be between -2 and 2.
# ------------------------------------------------------------------------------


# estimate the person parameters, and compute some X^2 itemfit statistics (for which we need the person parameters)

ppar <- person.parameter(fitrsm)


ifit0 <- eRm::itemfit(ppar)


ifit0



# -->
# Especially check the "p-value" and "Outfit t" simultaneously.
# 

# The results suggest that ceaq3, ceaq10, ceaq15 are not fit
# ceaq10 has the lowest p-value and high outfit/infit statistics.

# ceaq3:  p-value = 0.006 and Outfit t = 1.305
# ceaq10:  p-valueo = 0.000 and Outfit t = 4.549
# ceaq15:  p-value = 0.018 and Outfit t = 1.883


# Note that ceaq9 and ceaq14 are NOT bad.



# ----------
library(Gifi)
prin <- princals(itceaq)
plot(prin)


