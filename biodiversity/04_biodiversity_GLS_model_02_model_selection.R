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
# Test 3-way interaction by comparing nested models
# ------------------------------------------------------------------------------

library(nlme)

vfOptim <- varIdent(form = ~ 1 | fTreatment * fNutrient)


# full model (with 3-way interaction term)
M2A1 <- gls(Concentration ~ Biomass + fTreatment + fNutrient +
              Biomass:fTreatment + Biomass:fNutrient + fTreatment:fNutrient +
              Biomass:fTreatment:fNutrient,
            weights = vfOptim,
            method = "ML", data = Biodiv)


# full model minus 3-way interaction term
M2A2 <- gls(Concentration ~ Biomass + fTreatment + Nutrient +
              Biomass:fTreatment + Biomass:fNutrient + fTreatment:fNutrient,
            weights = vfOptim,
            method = "ML", data = Biodiv)



anova(M2A1, M2A2)



# -->
# 3-way interaction can be dropped.



# ------------------------------------------------------------------------------
# Assess significance of all three 2-way interactions
# ------------------------------------------------------------------------------

# Full model
fFull <- formula(Concentration ~ Biomass + fTreatment + fNutrient +
                 Biomass:fTreatment + Biomass:fNutrient + fTreatment:fNutrient)


M3.Full <- gls(fFull, weights = vfOptim, method = "ML", data = Biodiv)



# ----------
# Drop Biomass:fTreatment
M3.Drop1 <- update(M3.Full,. ~ . -Biomass:fTreatment)

anova(M3.Full, M3.Drop1)



# ----------
# Drop Biomass:fNutrient
M3.Drop2 <- update(M3.Full,. ~ . -Biomass:fNutrient)

anova(M3.Full, M3.Drop2)



# ----------
# fTreatment:fNutrient
M3.Drop3 <- update(M3.Full,. ~ . -fTreatment:fNutrient)

anova(M3.Full, M3.Drop3)



# -->
# Clearly the 2-way interaction term Biomass:fTreatment is not significant at the 5% level and should be dropped.



# ------------------------------------------------------------------------------
# 2nd round backward selection
# ------------------------------------------------------------------------------

M4.Full <- gls(Concentration ~ Biomass + fTreatment + fNutrient + 
               Biomass:fNutrient + fTreatment:fNutrient,
             weights = vfOptim,
             method = "ML", data = Biodiv)


# ----------
# Drop Biomass:fNutrient
M4.Drop1 <- update(M4.Full, .~. -Biomass:fNutrient)

anova(M4.Full, M4.Drop1)



# ----------
# Drop fTreatment:fNutrient
M4.Drop2 <- update(M4.Full, .~. -fTreatment:fNutrient)

anova(M4.Full, M4.Drop2)



# -->
# p-value of 0.04 for the Biomass:fNutrient interaction is not impressive, especially not with a series of hypothesis tests.
# So we decided to drop it as well and continue with the following full model.



# ------------------------------------------------------------------------------
# 3rd round backward selection
# ------------------------------------------------------------------------------

M5.Full <- gls(Concentration ~ Biomass + fTreatment + fNutrient + fTreatment:fNutrient,
             weights = vfOptim, method = "ML", data = Biodiv)


# ----------
# Drop fTreatment:fNutrient
M5.Drop1 <- update(M5.Full, .~. -fTreatment:fNutrient)

anova(M5.Full, M5.Drop1)



# ----------
# Drop Biomass
M5.Drop2 <- update(M5.Full, .~. -Biomass)

anova(M5.Full, M5.Drop2)



# -->
# Biomass term is not significant and can be dropped



# ------------------------------------------------------------------------------
# 4th round backward selection
# ------------------------------------------------------------------------------

M6.Full <- gls(Concentration ~ fTreatment + fNutrient + fTreatment:fNutrient,
             weights = vfOptim, method = "ML", data = Biodiv)

M6.Drop1 <- update(M6.Full, .~. -fTreatment:fNutrient)


anova(M6.Full, M6.Drop1)



# ------------------------------------------------------------------------------
# Final model with "REML"
# ------------------------------------------------------------------------------

# We reapplied this model with REML estimation. Normality and homogeneity can safely be assumed
MFinal <- gls(Concentration ~ fTreatment + fNutrient + fTreatment:fNutrient, weights = vfOptim, method = "REML", data = Biodiv)



