# setwd("//media//kswada//MyFiles//R//owls")
setwd("//media//kswada//MyFiles//R//Mixed_effects_and_multilevel_model//owls")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Owls
# ------------------------------------------------------------------------------

Owls <- read.table(file = "Owls.txt", header = TRUE)


str(Owls)

dim(Owls)

car::some(Owls)


# ----------
M.lm <- lm(NegPerChick ~ SexParent * FoodTreatment + SexParent * ArrivalTime, data = Owls)


mod_obj <- M.lm



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



# -->
# The graph indicates heterogeneity because the residual spread increases along the horizontal axis.



# ----------
# group by against linear predictor
residualPlot(mod_obj, type = "rstandard", groups = Owls$Nest, linear = FALSE)




