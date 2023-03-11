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
# Compare coefficients of four models
# ------------------------------------------------------------------------------

models <- c("within", "random", "pooling", "between")


sapply(models, function(x) coef(plm(log(price) ~ log(income), data = php, model = x))["log(income)"])



# -->
# The GLS estimator is close to within estimator
