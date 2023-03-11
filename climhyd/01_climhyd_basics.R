setwd("//media//kswada//MyFiles//R//climhyd")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  climhyd
#   - The data set contains 454 months of measured values for six climatic variables at Lake Shasta in California
#        - Temp:  air temperature
#        - DewPt:  DewPt
#        - CldCvr:  cloud cover
#        - WndSpd:  wind speed
#        - Precip:  precipitation
#        - Infrlow:  inflow
# ------------------------------------------------------------------------------

data(climhyd, package = "astsa")


str(climhyd)

head(climhyd)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

