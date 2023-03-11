setwd("//media//kswada//MyFiles//R//cmort")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)


# ------------------------------------------------------------------------------
# data:  cmort (Pollution, Temperature and Mortality)
# ------------------------------------------------------------------------------

data(cmort, package = "astsa")
data(tempr, package = "astsa")
data(part, package = "astsa")


str(cmort)
str(tempr)
str(part)


cmort
tempr
part



# ------------------------------------------------------------------------------
# Based on the scatterplot matrix,
# we entertain, tentatively, four models
#   - preparation
# ------------------------------------------------------------------------------

# Mt:  cardiovascular mortality
# Tt:  temperature
# Pt:  particulate levels


temp <- tempr - mean(tempr)

temp2 <- temp ^ 2

trend <- time(cmort)


# Here we use of na.action in lm() is to retain the time series attributes for the residuals and fitted values



# ------------------------------------------------------------------------------
# 4 models:
#   (1)  cmort ~ trend
#   (2)  cmort ~ trend + temp
#   (3)  cmort ~ trend + temp + temp2
#   (4)  cmort ~ trend + temp + temp2 + part
# ------------------------------------------------------------------------------

# trend only model:  Mt = beta0 + beta1 * t + wt
fit1 <- lm(cmort ~ trend, na.action = NULL)
summary(fit1)



# liniear temperature:  Mt = beta0 + beta1 * t + beta2 * (Tt - T.) + wt
fit2 <- lm(cmort ~ trend + temp, na.action = NULL)

summary(fit1)

summary(fit2)



# -->
# As expected, a negative trend is present in time as well as a negative coefficient for adjusted temperature.



# ----------
# curvilinear temperature:  Mt = beta0 + beta1 * t + beta2 * (Tt - T.) + beta3 * (Tt - T.)^2 + wt
fit3 <- lm(cmort ~ trend + temp + temp2, na.action = NULL)


# -->
# The quadratic effect of temperature can clearly be seen in the scatterplots



# ----------
# curvilinear temperature and pollution:  Mt = beta0 + beta1 * t + beta2 * (Tt - T.) + beta3 * (Tt - T.)^2 + beta4 * Pt + wt
fit4 <- lm(cmort ~ trend + temp + temp2 + part, na.action = NULL)


summary(fit4)



# -->
# Pollution weights positively and can be interpreted as the incremental contribution to daily deaths per unit of particulate pollution.



# ------------------------------------------------------------------------------
# Compare coefficients and significance
# ------------------------------------------------------------------------------

library(stargazer)

stargazer(fit1, fit2, fit3, fit4, type = "text")



# -->
#   (1)  cmort ~ trend
#   (2)  cmort ~ trend + temp
#   (3)  cmort ~ trend + temp + temp2
#   (4)  cmort ~ trend + temp + temp2 + part



# ------------------------------------------------------------------------------
# Model comparison by AIC
# ------------------------------------------------------------------------------

AIC(fit1, fit2, fit3, fit4, k = log(length(cmort)))



# -->
# fit4 (curvillinear temperature and pollution) is best in terms of BIC



# ----------
# AICc
# because of the large sample size, AIC and AICc are nearly the same

num <- length(cmort)

AIC(fit4) / num - log(2 * pi)
BIC(fit4) / num - log(2 * pi)
log(sum(resid(fit4) ^ 2) / num) + (num + 5) / (num - 5 - 2)




# ------------------------------------------------------------------------------
# Check the significance of model terms
# ------------------------------------------------------------------------------

library(car)


Anova(fit4)


drop1(fit4)




# ------------------------------------------------------------------------------
# Variability accounted by each term
# ------------------------------------------------------------------------------


summary(aov(fit4))



# -->
# temperature squared, and particulates does the best, accounting for some 60% of the variability and with the best value for BIC.



# ------------------------------------------------------------------------------
# residuals
# ------------------------------------------------------------------------------


par(mfrow = c(2,2))


plot(fit4)



# ----------
car::residualPlots(fit4)



# ------------------------------------------------------------------------------
# model understanding:  effect plot
# ------------------------------------------------------------------------------


library(effects)


plot(predictorEffects(fit4))




