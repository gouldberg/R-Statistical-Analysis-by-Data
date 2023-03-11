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
# Fit main effects model
# ------------------------------------------------------------------------------

acci.mod <- glm(result ~ age + mode + gender, weight = Freq, data = Accident, family = binomial)


summary(acci.mod)



# ----------
anova(acci.mod, test = "Chisq")

car::Anova(acci.mod)



# ------------------------------------------------------------------------------
# plot main effect
# ------------------------------------------------------------------------------

library(effects)


# acci.eff <- allEffects(acci.mod, partial.residuals = TRUE)
acci.eff <- allEffects(acci.mod)


acci.eff



# ----------
acci.eff[["age"]]

acci.eff[["age"]]$model.matrix



# ----------
plot(acci.eff)


