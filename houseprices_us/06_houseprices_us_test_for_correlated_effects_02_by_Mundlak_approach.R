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
# Mundlak approach for test for correlated effects
#   - The regular Hausman test is based on homoskedasticity assumptions and can yield negative test statistics.
#     These inconveniences can be overcome by the Mundlak version of the test, which is based on a regression model of the form:
#     y(it) = delta1 + beta2 * x2(it) + alpha1 * omega(1i) + gamma2 * mean(x2(i)) + ( c(i) + e(it) )
#     where mean(x2(i)) is the time average of the independent variable
# ------------------------------------------------------------------------------

php <- pdata.frame(HousePricesUS)


# ----------
pdim(php)


index(php)


# ----------
php.eq <- log(price) ~ log(income)



# ----------
php.w <- plm(php.eq, data = php, model = "within")


php.b <- update(php.w, model = "between")



# ----------
cp <- intersect(names(coef(php.b)), names(coef(php.w)))

dcoef <- coef(php.w)[cp] - coef(php.b)[cp]

V <- vcov(php.w)[cp, cp] + vcov(php.b)[cp, cp]


# Chi-square test statistics
( test.stats <- as.numeric(t(dcoef) %*% solve(V) %*% dcoef) )


# p-values
1 - pchisq(test.stats, df = 1)


# -->
# almost close to Hausman test.
# the null hypothesis of no correlation is NOT rejected



# ----------
# correlation coefficient between the individual effects (estiamted by the fixed effects of the within model)
# and the individual means of the explanatory variables obtained by applying the between function to the series

cor(fixef(rice.w), between(log(Rice$goutput)))



# -->
# The correlation is positive buyt moderate


