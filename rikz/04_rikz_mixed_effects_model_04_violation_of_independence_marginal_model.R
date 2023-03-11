# setwd("//media//kswada//MyFiles//R//rikz")
setwd("//media//kswada//MyFiles//R//Mixed_effects_and_multilevel_model//rikz")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  RIKZ
# ------------------------------------------------------------------------------

RIKZ <- read.table(file = "RIKZ.txt", header = TRUE)


str(RIKZ)


car::some(RIKZ)



# ------------------------------------------------------------------------------
# The Marginal Model
#   - Including random effects induces a correlation structure between observations from the same beach.
#     With the random intercept model, the induced correlation structure was fairly simple with the correlation between any two observations from the 
#     same beach given as d^2 / (d^2 + sigma^2)
#   - Surprisingly, we can get the same correlation structure and estimated parameters in a different way,
#     and it does not contain any random effects.
#     We allow for dependence between residuals
# ------------------------------------------------------------------------------

RIKZ$fBeach <- factor(RIKZ$Beach)


# the marginal model with compound symmetric structure (the most restrictive correlation structure)
# In this case, the covariance between any two observations on the same beach i is some constant rho.
M.gls <- gls(Richness ~ NAP, method = "REML", correlation = corCompSymm(form = ~ 1 | fBeach), data = RIKZ)



# ----------
# equivalent random intercept mixed effects model
M.mixed <- lme(Richness ~ NAP, random = ~1 | fBeach, method = "REML", data = RIKZ)



# ----------
summary(M.gls)

summary(M.mixed)

summary(Mlme1)



# -->
# The estimated Rho = 0.48 = phi / sigma^2 = phi / 4.25^2

