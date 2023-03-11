setwd("//media//kswada//MyFiles//R//ethanol")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ethanol
# ------------------------------------------------------------------------------

data(ethanol, package="lattice")


str(ethanol)


car::some(ethanol)



# ------------------------------------------------------------------------------
# correlation between variables
# ------------------------------------------------------------------------------

psych::describe(ethanol)


psych::pairs.panels(ethanol)




