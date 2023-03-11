setwd("//media//kswada//MyFiles//R//jj")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Johnson & Johnson Quarterly Earnings
#   - Quarterly earnings per share for the U.S. company Johnson & Johnson, furnished by Professor Paul Griffin (personal communication)
#     of the Graduate School of Management, University of California, Davis.
#     THere are 84 quarters (21 years) measured from the first quarter of 1960 to the last quarter of 1980.
# ------------------------------------------------------------------------------

data(jj, package = "astsa")

str(jj)

jj



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

