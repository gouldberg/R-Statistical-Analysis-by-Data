setwd("//media//kswada//MyFiles//R//attenu")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  attenu
# ------------------------------------------------------------------------------

data("attenu")


str(attenu)

dim(attenu)


car::some(attenu)



# ----------
attenu <- na.exclude(attenu)



# ------------------------------------------------------------------------------
# Test random effects for "station" by exactRLRT()
# ------------------------------------------------------------------------------


mmod <- lmer(accel^0.25 ~ log(dist) + mag + (1 | station) + (1 | event), data = attenu)


m0 <- lmer(accel^0.25 ~ log(dist) + mag + (1 | event), data = attenu)


mA <- lmer(accel^0.25 ~ log(dist) + mag + (1 | station) + (1 | event), data = attenu)



# ----------
# we can obtain p-value also by exactRLRT()
# m: fitted model under the alternative or, for testing in models with multiple variance components, the reduced model containing only the random effect to be tested.
# mA: the full model under the alternative for testing in models with multiple variance components
# m0: the model under the null for testing in models with multiple variance components

library(RLRsim)


exactRLRT(m = mmod, mA = mA, m0 = m0)



# -->
# The evidence for a station variation effect is NOT significant ....



# ------------------------------------------------------------------------------
# Test random effects for "event" by exactRLRT()
# ------------------------------------------------------------------------------

mmod <- lmer(accel^0.25 ~ log(dist) + mag + (1 | station) + (1 | event), data = attenu)


m0 <- lmer(accel^0.25 ~ log(dist) + mag + (1 | station), data = attenu)


mA <- lmer(accel^0.25 ~ log(dist) + mag + (1 | station) + (1 | event), data = attenu)


exactRLRT(m = mmod, mA = mA, m0 = m0)



# -->
# The evidence for a event variation effect IS significant ....
