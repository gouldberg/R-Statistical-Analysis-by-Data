setwd("//media//kswada//MyFiles//R//penicillin")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  penicillin
# ------------------------------------------------------------------------------
data("penicillin", package = "faraway")

str(penicillin)

car::some(penicillin)



# ------------------------------------------------------------------------------
# Mixed model:  fixed treatment effects + random blend effects
# ------------------------------------------------------------------------------

library(lme4)


# The fixed effect here is just the intercept represented by the first 1 in the model formla
# The random effect is represented by (1 | operator) indicating that the data is groupd by operator and the 1 indicating that the random effect is constant
# within each group.


# Default fitting method is REML
op <- options(contrasts = c("contr.sum", "contr.poly"))

mmod <- lmer(yield ~ treat + (1 | blend), data = penicillin)

options(op)

summary(mmod)
summary(lmod)



# --> 
# The residual variance is the same in both models = 18.8
# Because we have a balanced design and so REML is equivalent to the ANOVA estimator.
# The treatment effects are also the same as is the overall mean.



# ----------
# By maximum likelihood estimates  (MLE is biased)
op <- options(contrasts = c("contr.sum", "contr.poly"))

smod <- lmer(yield ~ treat + (1 | blend), data = penicillin, REML = FALSE)

options(op)

summary(mmod)
summary(smod)


# -->
# The fixed effects are unchanged, but teh standard errors of fixed effects and variance of random effects are smaller.



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

