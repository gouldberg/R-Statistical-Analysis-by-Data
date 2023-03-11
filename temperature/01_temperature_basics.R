setwd("//media//kswada//MyFiles//R//temperature")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  temperature
#   - monthly temperatures (In Celsius) for the main European capitals and other major cities.
#   - The average annual temperature and the themal amplitude (difference between the maximum monthly average and the minimum monthly average of a city)
#     were also recorded for each city.
# ------------------------------------------------------------------------------

temperature <- read.table("temperature.csv", header = TRUE, sep = ";", dec = ".", row.names = 1)

str(temperature)

dim(temperature)


temperature



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
