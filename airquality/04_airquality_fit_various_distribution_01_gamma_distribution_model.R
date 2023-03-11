setwd("//media//kswada//MyFiles//R//airquality")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  airquality
# ------------------------------------------------------------------------------
data("airquality")


str(airquality)

car::some(airquality)



# ------------------------------------------------------------------------------
# Fit gamma GLM
#   - The GAMLSS model as implemented in the gamlss package does not use canonical links as default for mu as in the glm() function,
#     but generally uses links reflecting the range of the parameter values, i.e. "identity" for (-inf, inf), "log" for (0, inf), "logit" for (0, 1), etc.
# ------------------------------------------------------------------------------

# gamlss() function does not work with NA's, so before fitting the model the cases with missing values have to be removed.
da <- na.omit(airquality)


r2 <- gamlss(Ozone ~ Temp + Wind + Solar.R, data = da, family = GA)


l2 <- glm(Ozone ~ Temp + Wind + Solar.R, data = airquality, family = Gamma(link = "log"))


summary(r2)


summary(l2)



# ----------
# extract sigma
exp(coef(r2, "sigma"))



# ----------
# extract phi
# gamlss uses maximum likelihood estimation where glm uses the method of moments
exp(coef(r2, "sigma"))^2

summary(l2)$dispersion

# under dispersion



# ----------
# The GLM deviance = -2 * log(LFitted / LSaturated)
# The gamlss deviance = -2 * log(LFitted), which is called global deviance (GDEV)
deviance(r2)

deviance(l2)



# ----------
par(mfrow = c(2,2))

plot(l2)


plot(r2)


# -->
# good fit but under dispersion



# ----------
par(mfrow = c(2,2))
term.plot(r2, shade = TRUE)

