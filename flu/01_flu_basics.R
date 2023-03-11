setwd("//media//kswada//MyFiles//R//flu")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  flu
#   - monthly pneumonia and influenza deaths per 10,000 in the US. for 11 years, 1968 to 1978.
# ------------------------------------------------------------------------------

data(flu, package = "astsa")

str(flu)

flu



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

