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
# F tests for individual and/or time effects
#   - The test of the null hypothesis of no individual and time effects:  two-ways within model vs pooling model
#   - The test of the null hypothesis of no individual effects:  within model vs. pooling model
#   - ...
# ------------------------------------------------------------------------------

php <- pdata.frame(HousePricesUS)


# ----------
pdim(php)


index(php)



# ----------
php.eq <- log(price) ~ log(income)



# ----------
php.w <- plm(php.eq, data = php, model = "within")


php.p <- update(php.w, model = "pooling")


php.wd <- plm(php.eq, data = php, effect = "twoways")



# ----------
# The test of the null hypothesis of no individual effects

pFtest(php.w, php.p)


# -->
# Unsurprisingly, the absence of individual effects is strongly rejected.



# ----------
# The test of the null hypothesis of no individual and no time effects

pFtest(php.wd, php.p)



# ----------
# The test of the null hypothesis of no time effects allowing for the presence of individual effects

pFtest(php.wd, php.w)




