setwd("//media//kswada//MyFiles//R//foreigntrade")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Turkish Banks
# ------------------------------------------------------------------------------

data("TurkishBanks", package = "pder")


str(TurkishBanks)


dim(TurkishBanks)


car::some(TurkishBanks)



# ----------
summary(TurkishBanks)



# -->
# Many NA's ... omit NAs

TurkishBanks <- na.omit(TurkishBanks)

summary(TurkishBanks)



# ----------
TB <- pdata.frame(TurkishBanks)



# ------------------------------------------------------------------------------
# Error components decomposition
# (by GLS estimator to remove individual means)
# ------------------------------------------------------------------------------

ercomp(log(cost) ~ log(output), TB)



# -->
# in previous analysis, the variation of the covariate ("output") is mainly inter-individual (85%).
# But here for the error, the share of the individual effect and that of the idiosyncratic effect are similar (40% and 60%)
# This suggest that OLS and "between" estimators are very close.

# median of theta = 0.65:  GLS estimator removes about 65% of the individual mean
# GLS estimator is about halfway between the OLS and the within estimators

