setwd("//media//kswada//MyFiles//R//whiteside")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  whiteside
#   - Variables
#       - Gas: the weekly gas consumption in 1000s of cubic feet
#       - Temp: the average outside temperature in degrees Celsius
#       - Insul: a factor, before or after insulation
# ------------------------------------------------------------------------------
data("whiteside", package = "MASS")


str(whiteside)

car::some(whiteside)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

car::scatterplot(Gas ~ Temp, data = whiteside)

car::scatterplot(Gas ~ Temp, data = whiteside%>% filter(Insul == "Before"))



