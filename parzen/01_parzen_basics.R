setwd("//media//kswada//MyFiles//R//parzen")

packages <- c("dplyr", "gamlss")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data: parzen
#   - Variables:
#        - snowfall:  the annual snowfall in Buffalo, NY (inches) from 1910 to 1972 inclusive
# ------------------------------------------------------------------------------

data("parzen", package = "gamlss.data")


str(parzen)

car::some(parzen)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
