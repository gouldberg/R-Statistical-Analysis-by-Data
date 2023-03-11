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
# Generalized Least Squares (GLS) models with different variance covariates
# ------------------------------------------------------------------------------

library(nlme)


f1 <- formula(Concentration ~ Biomass * fTreatment * fNutrient)


M0 <- gls(f1, data = Biodiv)


M1A <- gls(f1, data = Biodiv, weights = varIdent(form = ~1 | fTreatment * fNutrient))


M1B <- gls(f1, data = Biodiv, weights = varIdent(form = ~1 | fNutrient))


M1C <- gls(f1, data = Biodiv, weights = varIdent(form = ~1 | fTreatment))




# ----------
anova(M0, M1A, M1B, M1C)



# -->
# The AIC of the model with both nutrient and enrichment as variance covariates (M1A) is by far the best model,
# as judged by the AIC and BIC.

# Note that not all the likelihood ratio test make sense (not all comparisons are from nested models)



# ------------------------------------------------------------------------------
# Information of the significance of the fixed explanatory variables
# ------------------------------------------------------------------------------

anova(M1A)


summary(M1A)


# -->
# 3-way interaction is NOT significant
# anova function applies sequential testing, meaning that the p-values will change
# if you change the order of the main terms or the order of the 2-way interactions.



# ------------------------------------------------------------------------------
# plot standardized residuals versus fitted values
# ------------------------------------------------------------------------------

graphics.off()

plot(M0, col = Biodiv$fNutrient, pch = 20)

plot(M1A, col = Biodiv$fNutrient, pch = 20)


# -->
# There is no sign of heterogeneity in M1A



# ------------------------------------------------------------------------------
# effects of all higher-order terms
# ------------------------------------------------------------------------------

graphics.off()

# allEffects() calculates the effects for all high-order terms in a given model.
plot(effects::allEffects(M1A), lwd = 3, ci.style = "bands")


