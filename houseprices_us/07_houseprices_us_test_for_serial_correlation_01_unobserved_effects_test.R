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
# Tests for Serial Correlation:  Unobserved Effects Test  (Wooldridge)
#   - semi-parametric test for the null hypothesis that sigma(ega)^2 i.e., that there are no unobserved effects in the residuals
#   - The test statistic is (N-) asymptotically distributed as a standard normal regardless of the distribution of the errors.
#     It does also not rely on homoscedasticity.
#   - It has power both against the standard random effects specification, where the unobserved effects are constant within every group,
#     as well as against any kind of serial correlation.
#     As such it "nests" both individual effects and serial correlation tests, trading some power against more specific alternatives in exchange for robustness.
# ------------------------------------------------------------------------------


php <- pdata.frame(HousePricesUS)


# ----------
pdim(php)


index(php)



# ----------
php.eq <- log(price) ~ log(income)



# ----------
pwtest(php.eq, php)


# -->
# The null hypothesis of no unobserved effects IS REJECTED !!!

