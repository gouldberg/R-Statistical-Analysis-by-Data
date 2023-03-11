setwd("//media//kswada//MyFiles//R//biodiversity")

packages <- c("dplyr", "lattice", "nlme")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Data:  Biodiversity
# ------------------------------------------------------------------------------

Biodiv <- read.table(file = "Biodiversity.txt", header = TRUE)

str(Biodiv)



# ----------
Biodiv$fTreatment <- factor(Biodiv$Treatment)

Biodiv$fNutrient <- factor(Biodiv$Nutrient)



# ------------------------------------------------------------------------------
# Plot regression terms
#   - plots regression terms against their predictors, optionally with standard errors and partial residuals added.
# ------------------------------------------------------------------------------

termplot(M0, partial.resid = TRUE, rug = TRUE, se = TRUE)



# ------------------------------------------------------------------------------
# main effect
# ------------------------------------------------------------------------------

graphics.off()


plot(effects::Effect("Biomass", M0), lwd = 3, ci.style = "bands")

plot(effects::Effect("fNutrient", M0), lwd = 3, ci.style = "bands")

plot(effects::Effect("fTreatment", M0), lwd = 3, ci.style = "bands")




# ------------------------------------------------------------------------------
# effects of all higher-order terms
# ------------------------------------------------------------------------------

graphics.off()

# allEffects() calculates the effects for all high-order terms in a given model.
plot(effects::allEffects(M0), lwd = 3, ci.style = "bands")




# ------------------------------------------------------------------------------
# effects of interaction
# ------------------------------------------------------------------------------

plot(effects::Effect(c("fNutrient", "fTreatment"), M0),
     lwd = 3, multiline = TRUE, ci.style = "bands")


plot(effects::Effect(c("fNutrient", "Biomass"), M0),
     lwd = 3, multiline = TRUE, ci.style = "bands")


plot(effects::Effect(c("fTreatment", "Biomass"), M0),
     lwd = 3, multiline = TRUE, ci.style = "bands")

