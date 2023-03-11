setwd("//media//kswada//MyFiles//R//powerplant")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS", "timsac")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Power Plant
#   - 500 observations on 3 variables:  comamnd, temperature and fuel
# ------------------------------------------------------------------------------

data(Powerplant, package = "timsac")


dim(Powerplant)


str(Powerplant)


head(Powerplant)




# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

