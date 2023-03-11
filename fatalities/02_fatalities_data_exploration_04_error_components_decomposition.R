setwd("//media//kswada//MyFiles//R//fatalities")

packages <- c("dplyr", "AER")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Fatalities
# ------------------------------------------------------------------------------

data("Fatalities", packages = "AER")


str(Fatalities)

dim(Fatalities)



# ----------
Fatalities$frate <- with(Fatalities, fatal / pop * 10000)



# ----------
Fa <- pdata.frame(Fatalities)



# ------------------------------------------------------------------------------
# Error components decomposition
# (by GLS estimator to remove individual means)
# ------------------------------------------------------------------------------

ercomp(frate ~ beertax, Fa)



# -->
# in previous analysis, the variation of the covariate ("beertax") is mainly inter-individual (98.7%).
# Here for the error, the share of the individual effect and that of the idiosyncratic effect are 88.1%
# This suggest that OLS and "between" estimators are very close.

# theta = 0.8622:  GLS estimator removes about only 86.2% of the individual mean
# GLS estimator is about close to OLS estimator

