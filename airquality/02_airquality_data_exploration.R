setwd("//media//kswada//MyFiles//R//airquality")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  airquality
# ------------------------------------------------------------------------------

data("airquality")


str(airquality)

car::some(airquality)




# ------------------------------------------------------------------------------
# data exploration
# ------------------------------------------------------------------------------

summary(airquality)



# -->
# Some data are missing for Ozone and Solar.R



# ----------
Hmisc::describe(airquality)



# -->
# Month is only 5,6,7,8,9



# ----------
psych::describe(airquality)



# -->
# response variable "Ozone" is really skewed in left.
# normal linear regression may not fit well  -->  Gamma regression ?



