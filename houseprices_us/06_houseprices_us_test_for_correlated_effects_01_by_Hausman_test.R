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
# Hausman test for correlated effects
#   - H0:  no correlation between the regressors and the individual effects
# ------------------------------------------------------------------------------

php <- pdata.frame(HousePricesUS)


# ----------
pdim(php)


index(php)



# ----------
php.eq <- log(price) ~ log(income)



# ----------
php.w <- plm(php.eq, data = php, model = "within")


php.r <- update(php.w, model = "random")



# ----------
# The test of the null hypothesis of no correlation between the regressors and the individual effects
# (the statistic is distributed as a X^2 with 1 degrees of freedom)

phtest(php.w, php.r)



# -->
# Not rejected, indicating NOT that there are some correlation

