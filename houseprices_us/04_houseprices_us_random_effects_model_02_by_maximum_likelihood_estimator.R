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
# Random Effects Model by Maximum Likelihood Estimator
# ------------------------------------------------------------------------------

php <- pdata.frame(HousePricesUS)


# ----------
pdim(php)


index(php)



# ----------
php.eq <- log(price) ~ log(income)


library(pglm)


# Estimate the Swamy and Arora model
php.ml <- pglm(php.eq, data = php, family = gaussian)


summary(php.ml)

summary(php.swar)


# -->
# The coefficients are very similar to those obtained with the GLS estimator.

# The 2 parameters sd.idios and sd.id are the estimated standard deviations of the idiosyncratic and of the individual parts of the error
# Those too are almost close to GLS estimator.


