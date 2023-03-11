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
# Gamma GLM model (log-link)
# ------------------------------------------------------------------------------

gl <- glm(actual ~ log(projected) - 1, family = Gamma(link = log), data = cpd)

summary(gl)



# ----------
# pseudo R^2
1 - gl$deviance / gl$null.deviance


