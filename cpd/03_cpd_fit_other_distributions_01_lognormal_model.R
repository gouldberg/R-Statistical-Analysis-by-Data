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
# lognormal model
# ------------------------------------------------------------------------------

llg <- glm(log(actual) ~ log(projected) - 1, data = cpd)

summary(llg)



# ----------
# pseudo R^2
1 - llg$deviance / llg$null.deviance

