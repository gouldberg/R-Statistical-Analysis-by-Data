setwd("//media//kswada//MyFiles//R//ozone")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ozone
# ------------------------------------------------------------------------------

data(ozone, package="faraway")

str(ozone)



# ------------------------------------------------------------------------------
# simple linear model for reference purposes
# ------------------------------------------------------------------------------
# to simplify matters, we will reduce the predictors to just three:
#  - temp:  temperature measured at El Monte
#  - ibh:  inversion base height at LAX
#  - ibt:  inversion top temperature at LAX

olm <- lm(O3 ~ temp + ibh + ibt, ozone)

summary(olm)


# -->
# Note that ibt is not significant in this model.



# ----------
par(mfrow = c(2,2))
plot(olm)



# ------------------------------------------------------------------------------
# Check effect plot
# ------------------------------------------------------------------------------

# We fix the other predictors at some typical value (the mean by default) and allow only the chosen predictor to vary.
library(effects)

plot(Effect("temp", olm, partial.residuals = TRUE))
plot(Effect("ibh", olm, partial.residuals = TRUE))
plot(Effect("ibt", olm, partial.residuals = TRUE))



# -->
# The dashed line represents a smooth fit to these residuals and gives a suggestion of the extent of nonlinearity
# We can see that ibt does not have much effect after adjusting for the other 2 predictors, although there is some suggestion of a quadratic effect.

