setwd("//media//kswada//MyFiles//R//cpd")

packages <- c("dplyr", "faraway")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  cpd
# ------------------------------------------------------------------------------

data(cpd, package="faraway")

str(cpd)

car::some(cpd)



# ------------------------------------------------------------------------------
# inverse Gaussian GLM
# ------------------------------------------------------------------------------

igl <- glm(actual ~ log(projected) - 1, family = inverse.gaussian(link = "log"), data = cpd)
igi <- glm(actual ~ projected - 1, family = inverse.gaussian(link = "identity"), data = cpd)

summary(igl)
summary(igi)



# ----------
# pseudo R^2
1 - igl$deviance / igl$null.deviance
1 - igi$deviance / igi$null.deviance
