setwd("//media//kswada//MyFiles//R//jsp")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  jsp
# ------------------------------------------------------------------------------
data("jsp", package = "faraway")

str(jsp)

car::some(jsp)



# ----------
library(plm)

jsp.p <- pdata.frame(jsp, index = c("id", "year"))




# ------------------------------------------------------------------------------
# Error components decomposition
# (by GLS estimator to remove individual means)
# ------------------------------------------------------------------------------

ercomp(math ~ english, jsp.p)



# -->
# in previous analysis, the variation of math is mainly inter-individual (76%).
# But here for the error, the share of the individual effect and that of the idiosyncratic effect are 39% and 61%, respectively
# This suggest that OLS and "between" estimators are close.

# theta = 0.41 (at median):  GLS estimator removes about only 41% of the individual mean

