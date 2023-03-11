setwd("//media//kswada//MyFiles//R//houseprices_us")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  House Prices US
# ------------------------------------------------------------------------------

data("HousePricesUS", package = "pder")


str(HousePricesUS)


dim(HousePricesUS)



# ----------
php <- pdata.frame(HousePricesUS)



# ------------------------------------------------------------------------------
# Error components decomposition
# (by GLS estimator to remove individual means)
# ------------------------------------------------------------------------------

ercomp(log(price) ~ log(income), php)



# -->
# For the error, the share of the individual effect and that of the idiosyncratic effect are 43.5% and 56.5%

# theta = 0.793:  GLS estimator removes about only 79.3% of the individual mean

