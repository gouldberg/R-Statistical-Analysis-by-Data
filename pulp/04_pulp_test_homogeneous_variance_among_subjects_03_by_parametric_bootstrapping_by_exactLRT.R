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
# Null Hypothesis:  the variance between the operators is zero
# Parametric Bootstrap Approach by RLRsim package
#   - This package can be used to test random effect with REML base.
# ------------------------------------------------------------------------------

nullmod <- lm(bright ~ 1, data = pulp)


smod <- lmer(bright ~ 1 + (1 | operator), data = pulp, REML = FALSE)



library(RLRsim)


# This is REML base
exactLRT(smod, nullmod)



# -->
# The difference in the outcomes is within the sampling error.
# It is slightly better to use REML when testing the random effects (although remember that REML would be invalid for testing fixed effects.)



# ------------------------------------------------------------------------------
# As suggested by the output prints out above,
# we use REML based model
# ------------------------------------------------------------------------------

mmod <- lmer(bright ~ 1 + (1 | operator), data = pulp, REML = TRUE)

exactRLRT(mmod)



# -->
# Notice that the testing function is now exactRLRT and that only the alternative model needs to be specified
# as there is only one random effect component.

