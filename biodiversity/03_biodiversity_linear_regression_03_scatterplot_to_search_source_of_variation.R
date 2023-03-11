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
# Check relationship between response and continuous variable
# ------------------------------------------------------------------------------

car::scatterplot(Concentration ~ Biomass, data = Biodiv)



# -->
# A scatterplot of biomass versus concentration did not show any clear increase or decrease in spread.
# This indicates that the potential variance covariates are nutrient and/or enrichment.