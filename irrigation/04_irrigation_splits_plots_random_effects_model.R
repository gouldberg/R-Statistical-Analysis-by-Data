setwd("//media//kswada//MyFiles//R//irrigation")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  irrigation
# ------------------------------------------------------------------------------

data("irrigation", package = "faraway")

str(irrigation)

car::some(irrigation)




# ------------------------------------------------------------------------------
# Interaction + random effect model
#   -  The irrigation and variety are fixed effects, but the field is clearly a random effect.
#      We must also consider the interaction between field and variety, which is necessarily also a random effect because one of the two components is random.
# ------------------------------------------------------------------------------

library(lme4)


# y(ijk) = mu + i(i) + v(j) + (iv)(ij) + f(k) + (vf)(jk) + e(ijk)
lmer(yield ~ irrigation * variety + (1 | field) + (1| field : variety), data = irrigation)



# -->
# IT will fail, because it is not possible to distinguish the variety within the field variation.



# ----------
# y(ijk) = mu + i(i) + v(j) + (iv)(ij) + f(k) + e(ijk)
cmod <- lmer(yield ~ irrigation * variety + (1 | field), data = irrigation)


summary(cmod)



# --> 
# We can see that the largest variance component is that due to the field effect, sigma(field) = 4.25
# with sigma(error) = 1.452


# -->
# The relatively large standard errors compared to the fixed effect estimates suggest that there may be no significant fixed effects.





