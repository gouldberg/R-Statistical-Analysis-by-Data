setwd("//media//kswada//MyFiles//R//gannets2")

packages <- c("dplyr", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  gannets2
#   - Camphuysen (2011) collected and analysed field observations on northern gannets (Morus bassanus) from the North Sea.
#     Searching and feeding tactics, along with foraging association with other predators, were investigated.
#     The behaviour of gannets at sea was described from visual observations during ship-based surveys and sessions of experimental
#     discarding on board fishery research vessels, in June and July of 1991 to 2004.
#   - The surveys covered most of the feeding range of gannets nesting on Bass Rock. Counts were conducted between periods when the 
#     survey ship was engaged in fishing. The survey periods were spaced some time apart with normally no more than 3-4 per day, when the vessel travelled at full
#     speed of 8 - 10 knots, and the number of gannets that followed the ship was low and could be controlled for.
#   - We analyse the number of gannets observed in a 300 m wide strip transect. It is assumed that all individual gannets were detected.
#     The underlying question is when and where gannets feed in the vicinity of the Bass Rock colonies.
#   - Variables:
#        - Day, Month, Year, Hours, and Minutes:  Day (of the month), month, year, hour, and minute of sampling
#        - Latitude, longitude, Y, X:  Survey location
#        - Area_surveyedkm_2:  Surveyed area
#        - Seastate:  Sea state
#        - Gannets_in_transect:  Number of gannets in 300 meter wide strip transect
# ------------------------------------------------------------------------------

Gannets <- read.table(file = "Gannets2.txt", header = TRUE)


str(Gannets)
names(Gannets)



# ------------------------------------------------------------------------------
# basics
# ------------------------------------------------------------------------------
