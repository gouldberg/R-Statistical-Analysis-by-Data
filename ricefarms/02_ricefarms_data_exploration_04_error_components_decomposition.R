setwd("//media//kswada//MyFiles//R//ricefarms")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  RiceFarms
# ------------------------------------------------------------------------------

data("RiceFarms", package = "splm")


str(RiceFarms)


dim(RiceFarms)



# ----------
Rice <- pdata.frame(RiceFarms, index = "id")



# ------------------------------------------------------------------------------
# Error components decomposition
# (by GLS estimator to remove individual means)
# ------------------------------------------------------------------------------

rice.eq <- log(goutput) ~ log(seed) + log(totlabor) + log(size)

ercomp(rice.eq, Rice)



# -->
# in previous analysis, the variation of the dependent variable ("log(goutput)") is mainly inter-individual (64.4%).
# Here for the error, the share of the individual effect and that of the idiosyncratic effect are 10% and 90%, respectively

# theta = 0.225:  GLS estimator removes about only 22.5% of the individual mean

