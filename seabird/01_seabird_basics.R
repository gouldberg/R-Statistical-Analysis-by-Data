setwd("//media//kswada//MyFiles//R//seabird")

packages <- c("dplyr", "fda")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  seabird
#   - Winter abundance of 13 seabird species has been monitored annually in a number of bays on Kodiak Island, Alaska, since 1979.
#   - Since 1986, these marine surveys have been conducted by the Kodiak National Wildlife Refuge using a standard protocol
#     revisiting a fixed set of transects in each bay.
#   - The bird counts analyzed here are from two bays, Uyak and Uganik, both influenced by the waters of the Shelikof Strait.
#     We focus on results from 1991 to 2005, less 1998 when the boat was in dry dock. We want to investigate potential differences in time
#     trends of bird species that primarily eat fish compared to those that primarily eat shellfish and mollusks.
# ------------------------------------------------------------------------------
data("seabird", package = "fda")

str(seabird)

car::some(seabird)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
