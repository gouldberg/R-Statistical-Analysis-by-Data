setwd("//media//kswada//MyFiles//R//arbuthnot")

packages <- c("dplyr", "vcd", "MASS", "HistData")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Arbuthnot data
# ------------------------------------------------------------------------------

data("Arbuthnot", package = "HistData")


str(Arbuthnot)


dim(Arbuthnot)


car::some(Arbuthnot)



# ------------------------------------------------------------------------------
# Effect plots
#  - For more than two variables, full-model plots of the fitted response surface can be cumbersome, particularly when the model contains
#    interactions or when the main substantive interest is focused on a given main effect or interaction, controlling for all other explanatory variables.
#  - All other variables are "controlled" by being fixed at typical values.
# ------------------------------------------------------------------------------

# Main effects model's main effect
arb.eff <- effects::allEffects(arb.mod, partial.residuals = TRUE)



# ----------
names(arb.eff)


# Main effect by each predictor
arb.eff[["Year"]]
arb.eff[["Year"]]$model.matrix


arb.eff[["Plague"]]
arb.eff[["Plague"]]$model.matrix


arb.eff[["Mortality"]]
arb.eff[["Mortality"]]$model.matrix



# ----------
# Plot of all effects (each high-order term) in the main effects model
# Partial residuals and their loess smooth are also shown for the continuous predictor
graphics.off();

plot(arb.eff, rows = 1, cols = 3, type="response", residuals.pch = 15)





