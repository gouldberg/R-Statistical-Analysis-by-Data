# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "Hmisc", "rms", "caret", "corrplot", "lattice", "ggplot2", "AER", "foreign", "car", "lmtest")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  hprice1 (Housing Price)
#
# Regression Equation Specification Error Test (RESET):  H0 is that all coeffs including "fitted" are = 0
# ------------------------------------------------------------------------------
data <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/hprice1.dta")
dim(data)
str(data)
describe(data)

# the price distribution is skewed
hist(data$price)

mod <- lm(price ~ lotsize + sqrft + bdrms, data = data)
summary(mod)
lmtest::coeftest(mod)


# H0:  all coeffs including "fitted" are = 0
# H0 is weakly rejected.
mod_reset <- lm(price ~ lotsize + sqrft + bdrms + I(fitted(mod)^2) + I(fitted(mod)^3), data = data)
summary(mod_reset)

car::linearHypothesis(mod_reset, matchCoefs(mod_reset, "fitted"))

# automatic RESET
lmtest::resettest(mod)


# tests of non-nested models
# Both models are rejected against comprehensive model ("encompassing model" E) containing the all regressors
mod <- lm(price ~ lotsize + sqrft + bdrms, data = data)
mod2 <- lm(price ~ log(lotsize) + log(sqrft) + bdrms, data = data)
lmtest::encomptest(mod, mod2, data = data)

mod_encomp <- lm(price ~ lotsize + sqrft + bdrms + log(lotsize) + log(sqrft), data = data)
lmtest::coeftest(mod_encomp)


