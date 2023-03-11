setwd("//media//kswada//MyFiles//R//blood")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  blood
#   - Longitudinal series of monitored blood parameters
#     the level of several biomedical markers after a cancer patient undergoes a bone marrow transplant.
#   - Measurements made for 91 days on three variables:
#        - WBS:  log(white blood count)
#        - PLT:  log(platelet)
#        - HCT:  hematocrit
#   - Approximately 40% of the values are missing, with missing varies occurring  primarily after the 35th day.
# ------------------------------------------------------------------------------

data(blood, package = "astsa")

str(blood)

blood



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

