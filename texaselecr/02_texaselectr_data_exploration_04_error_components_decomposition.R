setwd("//media//kswada//MyFiles//R//texaselectr")
packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Texas Electr
#  - This data were used by Kumbhakar (1996) and Horrace and Schmidt (1996) and concern the production cost of electric firms in Texas.
# ------------------------------------------------------------------------------

data("TexasElectr", package = "pder")


str(TexasElectr)


dim(TexasElectr)


car::some(TexasElectr)



# ----------
TexasElectr$cost <- with(TexasElectr, explab + expfuel + expcap)



# ----------
TE <- pdata.frame(TexasElectr)



# ------------------------------------------------------------------------------
# Error components decomposition
# (by GLS estimator to remove individual means)
# ------------------------------------------------------------------------------

ercomp(log(cost) ~ log(output), TE)



# -->
# in previous analysis, the variation of the covariate ("output") is mainly inter-individual (82%).
# But here for the error, the share of the individual effect and that of the idiosyncratic effect are 99% !!!
# This suggest that OLS and "between" estimators are very close.

# theta = 0.08:  GLS estimator removes about only 8% of the individual mean
# GLS estimator is about close to OLS estimator

