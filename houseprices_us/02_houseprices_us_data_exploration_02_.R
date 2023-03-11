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
# data exploration
# ------------------------------------------------------------------------------

