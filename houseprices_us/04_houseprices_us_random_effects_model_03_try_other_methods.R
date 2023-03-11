setwd("//media//kswada//MyFiles//R//houseprices_us")

packages <- c("dplyr", "plm")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  House Prices US
# ------------------------------------------------------------------------------

data("HousePricesUS", package = "pder")


str(HousePricesUS)


dim(HousePricesUS)



# ------------------------------------------------------------------------------
# Compare the results obtained with the 4 estiamtion methods
# ------------------------------------------------------------------------------

php.walhus <- update(php.swar, random.method = "swar")


php.amemiya <- update(php.swar, random.method = "amemiya")


php.nerlove <- update(php.swar, random.method = "nerlove")


php.models <- list(swar = php.swar, walhus = php.walhus, amemiya = php.amemiya, nerlove = php.nerlove)



# ----------
sapply(php.models, function(x) ercomp(x)$theta)


sapply(php.models, coef)



# -->
# These are very close to each other, and consequently, the estimated coefficients for the 4 models are almost identical
