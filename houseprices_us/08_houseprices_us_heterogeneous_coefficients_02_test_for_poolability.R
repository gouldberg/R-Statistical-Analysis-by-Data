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
# Test for Poolablity
#   - Heterogeneous estimators relax the assumption made in the error components model, which imposes homogeneity of all model parameters (but the intercept)
#     across individuals.
#     Under this assumption, one can estimate a single model for the whole sample, at most including individual-specific constant terms.
#     This restriction, called poolability, can be tested
# ------------------------------------------------------------------------------

php <- pdata.frame(HousePricesUS)


# ----------
pdim(php)


index(php)



# ----------
php.eq <- log(price) ~ log(income)



# ----------
# unrestricted model estimated by pvcm()
# This function allows to estimate two different models, depending on the parameter "model"
php.np <- pvcm(php.eq, data = HousePricesUS, model = "within")


php.pool <- plm(php.eq, data = HousePricesUS, model = "pooling")


php.within <- plm(php.eq, data = HousePricesUS, model = "within")



# ----------
summary(php.np)



# distribution of individual coefficients
par(mfrow = c(1,2))
hist(php.np$coefficients[,1], xlab = "", ylab = "", col = gray(0.8), main = "Intercept", xlim = c(-2, 8), ylim = c(0, 20))
hist(php.np$coefficients[,2], xlab = "", ylab = "", col = gray(0.8), main = "log(income)", xlim = c(-2, 3), ylim = c(0, 20))




# ----------
# The test for poolability
# Note that different degrees of freedom
pooltest(php.pool, php.np)


pooltest(php.within, php.np)



# -->
# Coefficient stability is very strongly rejected, even in its weakest form (specific constants)




