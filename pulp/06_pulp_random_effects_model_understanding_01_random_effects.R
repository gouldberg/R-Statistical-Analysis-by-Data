setwd("//media//kswada//MyFiles//R//pulp")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  pulp
# ------------------------------------------------------------------------------

data("pulp", package = "faraway")

str(pulp)

car::some(pulp)



# ----------
lmod <- aov(bright ~ operator, data = pulp)

mmod <- lmer(bright ~ 1 + (1 | operator), data = pulp, REML = TRUE)

mod_obj <- mmod



# ------------------------------------------------------------------------------
# Model understanding:  operator effect
#   - y(ij) = mu + alpha(i) + e(ij) 
#     In a model with random effects, alpha(i) are no longer parameters, but random variables.
#     A purely Bayesian approach would specify the parameters of the priors (or specify priors for these) and compute a posterior distribution for alpha(i).
# ------------------------------------------------------------------------------

# alpha(i)
( oprf <- ranef(mod_obj)$operator[[1]] )

summary(oprf)



# -->
# The difference between the best and the worst is about 0.47



# ------------------------------------------------------------------------------
# Model understanding:  random effects are type of shrinkage estimate
# ------------------------------------------------------------------------------

# fixed effects
( cc <- model.tables(lmod) )



# ----------
# The predicted random effects are related to the fixed effects.
# Ratio to random effects

cc[[1]]$operator / ranef(mod_obj)$operator



# -->
# We see that the predicted random effects are exactly in proportion to the fixed effects.
# Typically predicted random effects are smaller and could be viewed as a type of shrinkage estimate.



# ------------------------------------------------------------------------------
# Model understanding:  95% confidence intervals for the random effects
# ------------------------------------------------------------------------------

library(lattice)

dotplot(ranef(mod_obj, condVar = TRUE))

