setwd("//media//kswada//MyFiles//R//abdom")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  abdom
# ------------------------------------------------------------------------------

data("abdom", package = "gamlss.data")


str(abdom)

car::some(abdom)



# ------------------------------------------------------------------------------
# ordinary regression
# ------------------------------------------------------------------------------

mod.lm <- lm(y ~ x, data = abdom)


summary(mod.lm)




# ----------
par(mfrow = c(2,2))

plot(mod.lm)




# ------------------------------------------------------------------------------
# Check residuals
# ------------------------------------------------------------------------------

car::residualPlots(mod.lm)


ncvTest(mod.lm)


# -->
# Non-constant variance changes with the level fo the response.



# ------------------------------------------------------------------------------
# Data Exploration:  data transformation for "y"
# ------------------------------------------------------------------------------

# check density
car::densityPlot( ~ y, data = abdom)



# transforming for symmetry
car::symbox(~ y, data = abdom)



# check Box-Cox power family transformation
# Rounded Pwr is the first value among {1, 0, -1, 0.5, 0.33, -0.5, -0.33, 2, -2} that is included in the confidence interval for lambda
# The test for the log transformation has a very large p-value, indicating that the log transformation is consistent with the data,

p1 <- car::powerTransform(y ~ 1, data = abdom, family = "bcnPower")

summary(p1)


