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
# Variance decomposition between individuals and time
# ------------------------------------------------------------------------------

# Variance decomposition:
# "log(income)" is covariate,  "log(price)" is dependent variable in this example
summary(log(php$income))


# -->
# The variation is almost same for id and time !!



# ----------
summary(log(php$price))



# -->
# The variation is mainly due to inter-individual but less than half (48.4%)


