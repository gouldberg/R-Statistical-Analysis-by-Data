setwd("//media//kswada//MyFiles//R//pulp")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  pulp
# ------------------------------------------------------------------------------

data("pulp", package = "faraway")

str(pulp)

car::some(pulp)



# ------------------------------------------------------------------------------
# One-way ANOVA by restricted maximum likelihood estimators
# ------------------------------------------------------------------------------

library(lme4)


# The fixed effect here is just the intercept represented by the first 1 in the model formla
# The random effect is represented by (1 | operator) indicating that the data is groupd by operator and the 1 indicating that the random effect is constant
# within each group.


# Default fitting method is REML
mmod <- lmer(bright ~ 1 + (1 | operator), data = pulp)


summary(mmod)



# --> 
# We see that this gives identical estmates to the ANOVA method:
# sigma^2 = 0.106  sigma(alpha)^2 (random effect part) = 0.068 and mu = 60.4.

# For unbalanced designs, the REML and ANOVA estimators are not necessarily identical.



# ------------------------------------------------------------------------------
# For reference:  by maximum likelihood estimators (MLE is biased)
# ------------------------------------------------------------------------------

smod <- lmer(bright ~ 1 + (1 | operator), data = pulp, REML = FALSE)

summary(smod)



# -->
# The between-subjects SD = 0.21 is smaller than with the REML method as the ML method biases the estimates towards zero.
# The fixed effects are unchanged.



# ------------------------------------------------------------------------------
# Diagnostics
# ------------------------------------------------------------------------------

# The default predicted values and residuals use the estimated random effects.
# This means these residuals can be regarded as estimates of error which is usually what we want.

par(mfrow = c(1, 2))
qqnorm(residuals(mmod), main = "")

plot(fitted(mmod), residuals(mmod), xlab = "Fitted", ylab = "Residuals")
abline(h = 0)



# -->
# The plots indicate no particular problems.
# Random effects models are particularly sensitive to outliers, because they depend on variance components that can be substantially
# inflated by unusual points.

