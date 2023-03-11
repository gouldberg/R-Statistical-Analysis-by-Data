setwd("//media//kswada//MyFiles//R//airquality")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  airquality
#  - Daily airquality measurements in New York, from May to September 1973.
#  - Variables:
#       - Ozone (in ppb): Mean ozone in parts per billion from 1300 to 1500 hours at Roosevelt Island
#       - Solar.R (in lang): Solar radiation in Langleys in the frequency band 4000???7700 Angstroms from 0800 to 1200 hours at Central Park
#       - Wind (in mph): Average wind speed in miles per hour at 0700 and 1000 hours at LaGuardia Airport
#       - Temp (in F): Maximum daily temperature in degrees Fahrenheit at La Guardia Airport
#       - Month:  Month (1-12)
#       - Day of month (1-31)
# ------------------------------------------------------------------------------

data("airquality")


str(airquality)

car::some(airquality)




# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------


