setwd("//media//kswada//MyFiles//R//biodiversity")

packages <- c("dplyr", "lattice", "nlme")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# Data:  Biodiversity
#   - Based on experimental protocols developed in Emmerson and Raffaelli (2000), Emmerson et al. (2001), Solan and Ford (2003),
#     and Ieno et al. (2006), among others, replicate mesocosm experiments (using plastic ice containers)
#     were carried out.
#   - Benthic macrofaunal single and/or multiple species (biodiversity) were manipulated in a multi-patch environment,
#     and the release of ammonium(NH4-N), nitrate(NOx-N) and phosphate(PO4-P) concentrates were recorded from the sediment (ecosystem processes).
#   - The experiment examines the effect of macrofauna density and habitat heterogeneity on sediment nutrient release.
#     The macrofaunal biomass (H. diversicolor) was fixed across the following levels(0, 0.5, 1, 1.5, and 2g) and replicated within each biomass level (n=3)
#     The response variable:  concentration of a particular nutrient.
#   - To study the effect of habitat heterogeneity, the previous procedure was repeated for algae-enriched sediment.
#     This gave 36 observations per nutrient, 18 enriched, and 18 non-enriched. --> 108 samples (containers)
# ------------------------------------------------------------------------------

Biodiv <- read.table(file = "Biodiversity.txt", header = TRUE)


str(Biodiv)



# ------------------------------------------------------------------------------
# Basics
# ------------------------------------------------------------------------------

summary(Biodiv)


psych::describe(Biodiv)

