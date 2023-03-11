setwd("//media//kswada//MyFiles//R//rugged")

packages <- c("dplyr", "rethinking")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  rugged
#   - Each row in this data is a country, and the various columns are economic, geographic, and historical features.
#     The variable rugged is a Terrain Ruggedness Index that quantifies the topographic heterogeneity of a landscape.
#     The outcome variable here is the logarithm of real gross domestic product per capita, from the year 2000, rgdppc_2000.
#     We'll use the logarithm of it, since the logarithm is the magnitude of GDP. Since wealth generates wealth,
#     it tends to be exponentially related to anything that increase it.
# ------------------------------------------------------------------------------

data("rugged", package = "rethinking")

d <- rugged

dim(d)

str(d)


car::some(d)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------

