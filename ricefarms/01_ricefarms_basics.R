setwd("//media//kswada//MyFiles//R//ricefarms")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  RiceFarms
#  - Contains observations from 171 farms in Indonesia, observed over 6 growing seasons, 3 wet and 3 dry, between 1975 and 1983
#    The farms are located in 6 different villages of the Chimanuk River basin in West Java.
#    According toe Druska and Horrace (2004), 2 villages are flatlands on the north coast of the island, 3 in the highlands (600 - 1100 m)
#    in the central part of West Java, and the last is in the center of the island with an average altitude of 375 meters.
#    Roads are more in general proximity to big cities are extremely heterogeneous.
# ------------------------------------------------------------------------------

data("RiceFarms", package = "splm")


str(RiceFarms)


dim(RiceFarms)



# ------------------------------------------------------------------------------
# basics:  
# ------------------------------------------------------------------------------


