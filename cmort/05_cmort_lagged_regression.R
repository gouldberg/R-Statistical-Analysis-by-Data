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
# Regression with lagged variables with align lagged series
#   - preparation
# ------------------------------------------------------------------------------


temp <- tempr - mean(tempr)


temp2 <- temp ^ 2


trend <- time(cmort)


part4 <- stats::lag(part, -4)




# ----------
dat <- ts.intersect(cmort, trend, temp, temp2, part4, part)


head(dat)



length(cmort)

nrow(dat)



# -->
# note that # of records are reduced




# ------------------------------------------------------------------------------
# Regression with lagged variables with align lagged series
# ------------------------------------------------------------------------------

fit4 <- lm(cmort ~ trend + temp + temp2 + part, data = dat)




# ----------
# including 4 weeks prior Pollution (particulate count)

fit5 <- lm(cmort ~ trend + temp + temp2 + part4 + part, data = dat)


# without part

fit6 <- lm(cmort ~ trend + temp + temp2 + part4, data = dat)





# ----------
summary(fit4)


summary(fit5)


summary(fit6)





# ----------

anova(fit6, fit5)



# -->
# fit5 (including "part") is better





# ------------------------------------------------------------------------------
# Compare coefficients and significance
# ------------------------------------------------------------------------------

library(stargazer)

stargazer(fit4, fit5, fit6, type = "text")






# ------------------------------------------------------------------------------
# Model comparison by AIC
# ------------------------------------------------------------------------------

AIC(fit4, fit5, fit6, k = log(nrow(dat)))



# -->
# fit5 (curvillinear temperature and pollution) is best in terms of BIC




# ------------------------------------------------------------------------------
# residuals
# ------------------------------------------------------------------------------


par(mfrow = c(2,2))


plot(fit5)



# ----------
car::residualPlots(fit5)



