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
# Test of Cross-Sectional Dependence
#   - Pairwise-correlations-based tests to use the residuals of separate estimation of one time-series regression for each cross-sectional unit.
# ------------------------------------------------------------------------------

php <- pdata.frame(HousePricesUS)


pdim(php)


head(index(php))



# ----------
# pcdtest default setting is "cd", which is appropriate in a large panel setting

# We follow Pesaran (2004)'s suggestion to remove any by serial correlation by specifying a univariate AR(2) model
# of the variable of interest and proceed testing the residuals of the latte for cross-sectional dependence.

# We test cross-sectional correlation in log house prices drawing on the residuals of an AR(2) model in order to control for any persistence in the data.

pcdtest(diff(log(price)) ~ diff(lag(log(price))) + diff(lag(log(price), 2)), data = php)



# -->
# The test strongly rejects the null hypothesis, confirming substantial cross-sectional comovement in house prices.



