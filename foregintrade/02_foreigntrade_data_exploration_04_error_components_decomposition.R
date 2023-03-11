setwd("//media//kswada//MyFiles//R//foreigntrade")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Foreign Trade
# ------------------------------------------------------------------------------

data("ForeignTrade", package = "pder")


str(ForeignTrade)


dim(ForeignTrade)


car::some(ForeignTrade)



# ----------
FT <- pdata.frame(ForeignTrade)



# ------------------------------------------------------------------------------
# Error components decomposition
# (by GLS estimator to remove individual means)
# ------------------------------------------------------------------------------

ercomp(imports ~ gnp, FT)



# -->
# in previous analysis, the variation of the covariate ("output") is mainly inter-individual (93%).
# here for the error, the share of the individual effect and that of the idiosyncratic effect are 92.3% and 7.4%, respectively

# theta = 0.9423:  GLS estimator removes about only 94.23% of the individual mean
# GLS estimator is about close to OLS estimator

