setwd("//media//kswada//MyFiles//R//ceaq")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  CEAQ
# ------------------------------------------------------------------------------

data("CEAQ", package = "MPsychoR")


str(CEAQ)


car::some(CEAQ)



# ----------
# set the lowest category to zero in order to make it eRm compaatible
itceaq <- CEAQ[,1:16] - 1



# ----------
library(psych)

polcor <- polychoric(itceaq)


polcor

polcor$rho



# ------------------------------------------------------------------------------
# ANOVA with gender
# ------------------------------------------------------------------------------

ppar2 <- person.parameter(fitrsm2)


ppar2


CEAQ$theta <- ppar2$theta.table$`Person Parameter`



summary(aov(theta ~ grade + gender, data = CEAQ))



# -->
# Significant differences in theta by gender



# ----------
par(mfrow = c(1,2), mar = c(2,2,2,2))

boxplot(theta ~ gender, data = CEAQ)

car::densityPlot(theta ~ gender, data = CEAQ, col = c("darkgray", "blue"), lty = c(2,1))



# -->
# theta distribution for men are higher.

