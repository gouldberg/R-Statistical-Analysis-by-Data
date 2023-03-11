setwd("//media//kswada//MyFiles//R//gala")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gala
# ------------------------------------------------------------------------------

data("gala", package = "faraway")

str(gala)

dim(gala)


car::some(gala)



# ------------------------------------------------------------------------------
# Goodness of Fit:  The proportion of deviance explained
# ------------------------------------------------------------------------------

# proportion of deviance explained by this model
1 - modp$deviance / modp$null.deviance

1 - modp.step2$deviance / modp.step2$null.deviance

1 - modp2$deviance / modp2$null.deviance



# -->
# modp2 has only 2 variables, but deviance explained is almost same  (and higher than lienar regression = 0.7658)
