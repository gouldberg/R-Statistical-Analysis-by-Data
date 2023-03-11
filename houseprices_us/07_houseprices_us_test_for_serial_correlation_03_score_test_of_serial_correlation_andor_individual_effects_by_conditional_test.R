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
# Tests for Serial Correlation:  Score test 
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Conditional test of Baltagi and Li for serial correlation
#   - testing the null hypothesis of no serial correlation, the hypothesis of the presence of individual effects being maintained
#     under both the null and the alternative hypothesis
#   - This test allow for random effects of any magnitude
#   - pbltest function uses residuals of the random effects maximum likelihood estimator
# ------------------------------------------------------------------------------

php <- pdata.frame(HousePricesUS)


# ----------
pdim(php)


index(php)



# ----------
php.eq <- log(price) ~ log(income)


# ----------
pbltest(php.eq, php, alternative = "onesided")



# -->
# Serial correaltion is detected
