setwd("//media//kswada//MyFiles//R//orings")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  orings
# ------------------------------------------------------------------------------

data("orings", package = "faraway")

str(orings)

dim(orings)


car::some(orings)



# ----------
lmod <- glm(cbind(damage, 6 - damage) ~ temp, family = binomial, orings)

summary(lmod)



# ----------
mod_obj <- lmod



# ------------------------------------------------------------------------------
# Pearson residual, Standardized residuals
#   - Plots the residuals versus each term in a mean function and versus fitted values
#   - Also computes a curvature test for each of the plots by adding a quadratic term and testing the quadratic to be zero
#   - For linear models, this is Tukey's test for nonadditivity when plotting against fitted values
# ------------------------------------------------------------------------------

residualPlots(mod_obj)


residualPlots(mod_obj, type = "rstandard")



# ----------
# only by linear predictors
residualPlots(mod_obj, terms = ~1)

