setwd("//media//kswada//MyFiles//R//rent")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  rent
# ------------------------------------------------------------------------------
data("rent", package = "gamlss.data")


str(rent)

car::some(rent)



# ------------------------------------------------------------------------------
# Fit gamma GLM
#   - The GAMLSS model as implemented in the gamlss package does not use canonical links as default for mu as in the glm() function,
#     but generally uses links reflecting the range of the parameter values, i.e. "identity" for (-inf, inf), "log" for (0, inf), "logit" for (0, 1), etc.
# ------------------------------------------------------------------------------

r2 <- gamlss(R ~ Fl + A + H + loc, family = GA, data = rent)

l2 <- glm(R ~ Fl + A + H + loc, family = Gamma(link = "log"), data = rent)


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



# ----------
# The GLM deviance = -2 * log(LFitted / LSaturated)
# The gamlss deviance = -2 * log(LFitted), which is called global deviance (GDEV)
deviance(r2)
deviance(l2)



# ----------
plot(l2)
plot(r2)



# ------------------------------------------------------------------------------
# Fit inverse Gaussian distributions
# ------------------------------------------------------------------------------

r22 <- gamlss(R ~ Fl + A + H + loc, family = IG, data = rent)


plot(r22)



# ------------------------------------------------------------------------------
# Compare models
# ------------------------------------------------------------------------------

# In this case, because all three models have the same number of parameters, we could just compare global deviance by k = 0
GAIC(r1, r2, r22, k = 0)



# ----------
# The best model is Gamma distribution model
plot(r2)



# -->
# The residuals at this stage look a lot better than the residuals in liner regression in that 
# at least some of the hetrogeneity in the residuals has disappeared.
# Also the curvature in the normal Q-Q plot has been substantially reduced.







