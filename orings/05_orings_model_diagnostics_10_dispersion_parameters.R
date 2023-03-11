setwd("//media//kswada//MyFiles//R//orings")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  orings
# ------------------------------------------------------------------------------

data("orings", package = "faraway")

str(orings)


dim(orings)


car::some(orings)



# ------------------------------------------------------------------------------
# Dispersion parameter
# ------------------------------------------------------------------------------

sum(residuals(lmod, type = "pearson") ^ 2/ lmod$df.residual)
