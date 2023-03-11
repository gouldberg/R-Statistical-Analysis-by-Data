setwd("//media//kswada//MyFiles//R//ethanol")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ethanol
#   - data from ethanol fuel burned in a single-cylinder engine. The emissions of nitrogen oxides should be considered as the response
#     and engine compression and equivalence ratio as the predictors

#   - NOx:  Concentration of nitrogen oxides (NO and NO2) in micrograms/J.
#   - C:  Compression ratio of the engine.
#   - E:  Equivalence ratio–a measure of the richness of the air and ethanol fuel mixture.
# ------------------------------------------------------------------------------

data(ethanol, package="lattice")


str(ethanol)


car::some(ethanol)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
