setwd("//media//kswada//MyFiles//R//usair")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  usair
#   - US pollution data which has six explanatory variables x1:x6 and n = 41 observations
#   - Variables:
#        - y:  sulphur dioxide concentration in air in mg per cubic meter
#        - x1:  average annual temperature in degrees F
#        - x2:  number of manufacturers employing more than 20 workers
#        - x3:  population size in thousands
#        - x4:  average annual wind speed in miles per hour
#        - x5:  average annual rainfall in inches
#        - x6:  average number of days rainfall per year
# ------------------------------------------------------------------------------
data("usair", package = "gamlss.data")


str(usair)

car::some(usair)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

psych::describe(usair)


psych::pairs.panels(usair)
