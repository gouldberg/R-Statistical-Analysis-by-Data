setwd("//media//kswada//MyFiles//R//houseprices_us")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  House Prices US
# ------------------------------------------------------------------------------

data("HousePricesUS", package = "pder")


str(HousePricesUS)


dim(HousePricesUS)




# ------------------------------------------------------------------------------
# Define Panel Dataframe
# ------------------------------------------------------------------------------

php <- pdata.frame(HousePricesUS)



# ----------
# This is balanced panel
pdim(php)



# ----------
head(index(php))



# ----------
table(index(php)$state, useNA = "always")

table(index(php)$year, useNA = "always")

table(index(php))


