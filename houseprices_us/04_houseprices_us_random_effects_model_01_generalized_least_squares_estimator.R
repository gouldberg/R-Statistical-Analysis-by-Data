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
# Random Effects Model
#   - Fixed effects model is appropriate when the unobserved heterogeneity term is correlated with the independent variables.
#   - When this is not the case, we can use either OLS, or, when OLS is not enough, the random effects model
# ------------------------------------------------------------------------------

php <- pdata.frame(HousePricesUS)


# ----------
pdim(php)


index(php)



# ----------
php.eq <- log(price) ~ log(income)


# Estimate the Swamy and Arora model
php.swar <- plm(php.eq, data = php, model = "random", random.method = "swar")


# Same Swamy and Arora model
# random.models = c("within", "between"):  use the within residuals to estimate sigma(v) and the between residuals to estimate sigma(eta)
# random.dfcor = c(2,2):  indicating what is the denominator of the two quadratic forms
#  - 0: the number of observations is used (NT, N)
#  - 1: the numerators of the theoretical formulas are used (N(T - 1), N)
#  - 2: the number of estimated parameters are deduced (N(T - 1) - K, N - K - 1)

php.swar2 <- plm(php.eq, data = php, model = "random", random.models = c("within", "between"), random.dfcor = c(2,2))



# ----------
summary(php.swar)



# -->
# theta = 0.7931:  part of the individual mean that is removed from each variable for the GLS estimator = 79.3%
# This implies that the GLS estimator is closer to the Within estimator (theta = 1) than to the OLS estimator (theta = 0),
# (Note that T = 29, long time series)

# Note that the coefficient of log(income) is 0.35, much smaller than 1



# ----------
# This can be obtained from ercomp()
ercomp(php.eq, HousePricesUS)

ercomp(php.eq, php)$theta

