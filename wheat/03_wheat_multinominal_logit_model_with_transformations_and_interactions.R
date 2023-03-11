setwd("//media//kswada//MyFiles//R//wheat")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wheat
# ------------------------------------------------------------------------------

wheat <- read.csv(file = "//media//kswada//MyFiles//references//AnalysisOfCategoricalDataWithR//Chapter3//wheat.csv")

str(wheat)

car::some(wheat)



# ------------------------------------------------------------------------------
# multinominal logit model
# ------------------------------------------------------------------------------

levels(wheat$type)


library(nnet)

methods(class = multinom)

mod.fit <- multinom(formula = type ~ class + density + hardness + size + weight + moisture, data = wheat)


summary(mod.fit)



# ------------------------------------------------------------------------------
# multinominal logit model with transformations and interactions
# ------------------------------------------------------------------------------

# Quadtratic and interaction terms example
mod.fit.trans1 <- multinom(formula = type ~ class + density + I(density^2) + density:class, data=wheat) 


# -->
# Does not converge (notice says "stopped after 100")

summary(mod.fit.trans1)



# ----------
# Increase iterations maxit to 10000
mod.fit.trans2 <- multinom(formula = type ~ class + density + I(density^2) + density:class, data=wheat, maxit = 1000)

summary(mod.fit.trans2)



# ----------
AIC(mod.fit, mod.fit.trans2)


plot(predictorEffects(mod.fit.trans2))


