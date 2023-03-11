setwd("//media//kswada//MyFiles//R//ricefarms")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  RiceFarms
# ------------------------------------------------------------------------------

data("RiceFarms", package = "splm")


str(RiceFarms)


dim(RiceFarms)



# ------------------------------------------------------------------------------
# BSK tests:
#   - test for either random effects or spatial correlation in the remainder errors
# ------------------------------------------------------------------------------


# here we assume the presence of farm individual effects, perhaps representing parcel quality, farmers's ability,
# or other time-invarying idiosyncrasies.

# test = "LMH": joint test for random effects and spatial error correlation
bsktest(riceprod, data = Rice, listw = ricelw, test = "LMH")



# -->
# but this is of little use, because it will reject in the presence of either effect, giving no further directions



# ----------
# More interestingly, the conditional test for random farm effects
# allowing for spatial error correlation (test = "CLMmu") does in turn reject.

bsktest(riceprod, data = Rice, listw = ricelw, test = "CLMmu")



# the conditional test for spatial test, allowing for random effects

bsktest(riceprod, data = Rice, listw = ricelw, test = "CLMlambda")



# -->
# comprehensive SEMRE specification (spatial autoregressive model with random effects) is appropriate







