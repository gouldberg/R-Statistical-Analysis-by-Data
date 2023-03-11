setwd("//media//kswada//MyFiles//R//accident")

packages <- c("dplyr", "vcd", "vcdExtra", "MASS", "gpairs")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Accident
# ------------------------------------------------------------------------------

data("Accident", package = "vcdExtra")

dim(Accident)

str(Accident)


car::some(Accident)


Accident$result <- factor(Accident$result, levels = c("Injured", "Died"))

str(Accident)



# ------------------------------------------------------------------------------
# Full model effects plot for 2-way interactions
# ------------------------------------------------------------------------------

library(effects)


# plot each high-order term
acci.main <- allEffects(acci.mod2)


plot(acci.main, grid = TRUE)




# ----------
# full model plots by using all predictors in the model
acci.full <- Effect(c("age", "mode", "gender"), acci.mod2)

acci.full



plot(acci.full, grid = TRUE)


