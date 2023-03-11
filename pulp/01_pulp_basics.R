setwd("//media//kswada//MyFiles//R//pulp")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  pulp
#  - Data from an experiment to test the paper brightness depending on a shift operator described in Sheldon (1960).
# ------------------------------------------------------------------------------

data("pulp", package = "faraway")

str(pulp)

car::some(pulp)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
