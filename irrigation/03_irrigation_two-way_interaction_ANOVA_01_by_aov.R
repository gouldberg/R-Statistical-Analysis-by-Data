setwd("//media//kswada//MyFiles//R//irrigation")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  irrigation
# ------------------------------------------------------------------------------

data("irrigation", package = "faraway")

str(irrigation)

car::some(irrigation)




# ------------------------------------------------------------------------------
# Fixed effects three-way ANOVA
# ------------------------------------------------------------------------------

lmod <- aov(yield ~ irrigation * variety, data = irrigation)

summary(lmod)



# -->
# All the effects are NOT significant
# Also Mean Sq of Residuals are largest




# ----------
lmod2 <- aov(yield ~ irrigation + variety + field, data = irrigation)

summary(lmod2)



# -->
# Are there any field effect by each field ???
# irrigation is also significant !!!  -->  really ??



# ----------
plot(lmod2)



# ----------
# We need coef() to extract the coefficient for aov() model
coef(lmod)

coef(lmod2)




# ------------------------------------------------------------------------------
# model.tables:  computes summary tables for model fits, espeically complex aov fits
# ------------------------------------------------------------------------------

model.tables(lmod2, type = "means")

model.tables(lmod2, type = "effects", se = TRUE)



# ----------
# plot(effects::predictorEffects(lmod2))

plot(effects::predictorEffects(lmod))
