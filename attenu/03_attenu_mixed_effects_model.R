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
# Mixed effects model
# ------------------------------------------------------------------------------

library(lme4)


mmod <- lmer(accel^0.25 ~ log(dist) + mag + (1 | station) + (1 | event), data = attenu)



# ----------
summary(mmod)




# -->
# Fixed effects:
# accel^0.25 decrease about 0.132 for addtional log(dist)

# Random effects:
# The standard deviation for the intercept of station and event is 0.0286 and 0.0279 respectively.


