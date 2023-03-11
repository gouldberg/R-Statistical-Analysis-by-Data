setwd("//media//kswada//MyFiles//R//tobinq")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  TobinQ
# ------------------------------------------------------------------------------

data("TobinQ", package = "pder")


str(TobinQ)

dim(TobinQ)



# ----------
pTobinQ <- pdata.frame(TobinQ)



# ------------------------------------------------------------------------------
# Error components decomposition
# (by GLS estimator to remove individual means)
# ------------------------------------------------------------------------------

ercomp(ikn ~ qn, pTobinQ)



# -->
# in previous analysis, the variation of the covariate ("qn") is mainly inter-individual but less than half (43.1%).
# Here for the error, the share of the individual effect and that of the idiosyncratic effect are 72.5%
# This suggest that OLS and "between" estimators are close.

# theta = 0.73.5:  GLS estimator removes about 73.5% of the individual mean
# GLS estimator is about close to OLS estimator

