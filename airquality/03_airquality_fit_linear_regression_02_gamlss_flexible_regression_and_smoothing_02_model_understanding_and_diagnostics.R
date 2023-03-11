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
# Model understanding for gamlss model:  goodness of fit
# ------------------------------------------------------------------------------


# R^2 are same for l1 and r1
Rsq(r1)


summary(l1)





# ------------------------------------------------------------------------------
# Model understanding for gamlss model:  fiited model terms (main effect plot)
# ------------------------------------------------------------------------------

par(mfrow = c(1,3))
term.plot(r1)



# -->
# parial effect is meaned



library(effects)
plot(predictorEffects(l1))





# ------------------------------------------------------------------------------
# Model diagnostics
# ------------------------------------------------------------------------------

plot(r1)




par(mfrow = c(1,1))
wp(r1, ylim.all = 2)



# -->
# Fitted normal distribution seems not to be correct.

