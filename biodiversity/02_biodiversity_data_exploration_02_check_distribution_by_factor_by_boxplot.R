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
# Data exploration:  Check distribution by factor by boxplot
# ------------------------------------------------------------------------------

boxplot(Concentration ~ fTreatment * fNutrient, data = Biodiv)



# -->
# Boxplot for each nutrient-enrichment combination.
# The samples enriched with algae and with NH4, have higher concentrations and show more variation.