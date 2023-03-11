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
# Wald tests for SEM or SAR
#   - Wald-type tests can be made conditional to (i.e., valid in the presen of) individual random effects by including them in the specification
# ------------------------------------------------------------------------------


# spml: highest wrapper for spatial panel estimation by maximum likelihood,
# allowing for either fixed, random, or no effects (in the random or none cases, it calls spreml internally).
# spatial.error = "b" for "Baltagi", which selects the SEMRE specification
# spatial.error = "kpp" for SEM2RE specification


saremremod <- spml(riceprod, data = Rice, listw = ricelw, lag = TRUE, model = "random", spatial.error = "b")


summary(saremremod)



# -->
# From the estimation results, we gather that the SEMRE is the better specification
# the estimated SAR term lambda is not significant, while the SEM coefficient rho is of considerable magnitude and highly significant.



