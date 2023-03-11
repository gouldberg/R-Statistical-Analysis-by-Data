setwd("//media//kswada//MyFiles//R//varve")

packages <- c("dplyr", "astsa", "tseries", "forecast", "MTS")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  varve
#   - melting glaciers deposit yearly layers of sand and silt during the spring melting seasons, which can be reconstructed
#     yearly over a period ranging from the time deglaciation began in New England (about 12,600 years ago) to the time it ended
#     (about 6,000 years ago).
#     Such sedimentary deposits, called varves, can be used a proxies for paleoclimatic parameters, such as temperature,
#     because, in a warm year, more sand and silt are deposited from the receding glacier.
#   - Thicknesses of the yearly varves collected from one location in Massachusetts for 634 years, beginning 11,834 years ago.
#   - Because the variation in thicknesses increases in proportion to the amount deposited,
#     a logarithmic transformation could remove the nonstationarity observable in the variance as a function of time
# ------------------------------------------------------------------------------

data(varve, package = "astsa")

str(varve)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

