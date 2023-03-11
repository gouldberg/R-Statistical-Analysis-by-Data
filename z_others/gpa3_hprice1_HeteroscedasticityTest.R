# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
setwd("/media/kswada/MyFiles/R")

packages <- c("dplyr", "Hmisc", "rms", "caret", "corrplot", "lattice", "ggplot2", "AER", "foreign", "car", "lmtest")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  gpa3
# Regression by OLS (only for spring data)
# 
# Refiend White heteroscedasticity-robust standard errors
# linearHypothesis F-tests using White VCOV
#
# The results generally do not differ a lot between the usual version and White version
# This is an indication that heteroscedasticity might not be a big issue in this data
# ------------------------------------------------------------------------------
data <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/gpa3.dta")
dim(data)
str(data)
describe(data)

mod <- lm(cumgpa ~ sat + hsperc + tothrs + female + black + white, data = data, subset = (spring == 1))
summary(mod)


# usual standard errors and residuals
lmtest::coeftest(mod)
plot(resid(mod))

# Refined White heteroscedasticity-robust standard errors, classical version of White's robust SE
lmtest::coeftest(mod, vcov=hccm)
lmtest::coeftest(mod, vcov=hccm(mod, type="hc0"))


# F-Tests using usual VCOV
linearHypothesis(mod, c("black", "white"))

# Refined White VCOV and classical White VCOV
linearHypothesis(mod, c("black", "white"), vcov = hccm)
linearHypothesis(mod, c("black", "white"), vcov = hccm(mod, type="hc0"))


# ------------------------------------------------------------------------------
# data:  hprice1 (Housing Price)
#
# Beusch-Pagan test / White test for heteroscedasticity 
#
# If we transform the variables by log, heteroscedasticity is gone ...
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
plot(resid(mod))

# Beusch-Pagan test for heteroscedasticity  --> rejected, indicating that the residuals shows heteroscedasticity
lmtest::bptest(mod)

# equivalent BP test by manual
summary(lm( resid(mod)^2 ~ lotsize + sqrft + bdrms, data = data))

# special form of the White test
lmtest::bptest(mod, ~ fitted(mod) + I(fitted(mod)^2))


# if we transform by log, the result does not show heteroscedasticity
mod2 <- lm(log(price) ~ log(lotsize) + log(sqrft) + bdrms, data = data)
summary(mod2)

lmtest::coeftest(mod)
lmtest::coeftest(mod2)

par(mfrow=c(2,1))
plot(resid(mod));  plot(resid(mod2))

lmtest::bptest(mod2, ~ fitted(mod2) + I(fitted(mod2)^2))

