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
# Heterogeneous Coefficients
#   - The long panles allow to estimate separate regressions for each unit. Hence it is natural to question the assumption of parameter homogeneity.
#     In general, it can be said that imposing the pooling restriction reduces the variance of the pooled estimator but may introduce bias if these restrictions are false.
#   - The hypothesis of interest is whether house prices have an income elasticity of one.
# ------------------------------------------------------------------------------

php <- pdata.frame(HousePricesUS)


# ----------
pdim(php)


index(php)



# ----------
php.eq <- log(price) ~ log(income)


# ----------
# Swamy estimator:  model with all individual specific coefficients
swmod <- pvcm(php.eq, data = HousePricesUS, model = "random")



# ----------
# estimate a static specification by mean groups estimator ("mg")
# assuming only exogeneity of the regressors and independently sampled errors
# In general MG estimator is a special case with equal GSL weighting of the Swamy estimator, to which it converges as T grows sufficiently large.
mgmod <- pmg(php.eq, data = HousePricesUS, model = "mg")




# ----------
summary(swmod)


summary(mgmod)



# ----------
coefs <- cbind(coef(swmod), coef(mgmod))


dimnames(coefs)[[2]] <- c("Swamy", "MG")


coefs



# -->
# One can see that for T = 29, the efficient Swamy estimator and the simpler MG are already very close,
# moreover, both are statistically very far from one ...


