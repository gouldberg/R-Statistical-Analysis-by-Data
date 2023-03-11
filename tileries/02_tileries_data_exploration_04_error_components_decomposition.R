setwd("//media//kswada//MyFiles//R//tileries")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Tileries
# ------------------------------------------------------------------------------

data("Tileries", package = "pder")


str(Tileries)


dim(Tileries)


car::some(Tileries)



# ----------
Tl.p <- pdata.frame(Tileries)



# ------------------------------------------------------------------------------
# Error components decomposition
# (by GLS estimator to remove individual means)
# ------------------------------------------------------------------------------

Tl.eq <- log(output) ~ log(labor) + log(machine)

ercomp(Tl.eq, Tl.p)



# -->
# in previous analysis, the variation of the covariate ("log(output)") is mainly inter-individual (51.6%).
# But here for the error, the share of the individual effect and that of the idiosyncratic effect are 19.1% and 80.9%

# theta = 0.582 at median:  GLS estimator removes about only 58.2% of the individual mean

