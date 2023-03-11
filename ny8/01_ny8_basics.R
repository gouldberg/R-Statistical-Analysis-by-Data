setwd("//media//kswada//MyFiles//R//ny8")

packages <- c("dplyr", "maptools", "spdep")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  ny8
#  - New York leukeamia data, used and documented extensively in Waller and Gotway (2004).
#    This is the New York state eight-county census tract data.
#    There are 281 census tract observations, including sparsely populated rural areas contrasting with dense, small, urban tracts.
#  - The numbers of incident leukaemia cases are recorded by tract, aggregated from census block groups,
#    but because some cases could not be placed, they were added proportionally to other block groups, leading to non-integer counts.
#    The counts are for the 5 years 1978-1982, while census variables, such as the tract population, are just for 1980.
#  - Other census variables are the percentage aged over 65, and the percentage of the population owning their own home.
#    Exposure to TCE waste sites is represented as the logarithm of 100 times the inverse of the distance from the tract centroid to the nearest site.
# ------------------------------------------------------------------------------
library(rgdal)
NY8 <- readOGR("//media//kswada//MyFiles//references//AppliedSpatialDataAnalysisWithR//lat_bundle//NY8_utm18.shp", "NY8_utm18")
TCE <- readOGR("//media//kswada//MyFiles//references//AppliedSpatialDataAnalysisWithR//lat_bundle//TCE.shp", "TCE")

library(spdep)
NY_nb <- read.gal("//media//kswada//MyFiles//references//AppliedSpatialDataAnalysisWithR//lat_bundle//NY_nb.gal", region.id = row.names(NY8))
cities <- readOGR("//media//kswada//MyFiles//references//AppliedSpatialDataAnalysisWithR//lat_bundle//NY8cities.shp", "NY8cities")



str(NY_nb)

summary(NY_nb)
print(NY_nb)

card(NY_nb)

length(NY_nb)



# ---------------------------------------------------------------------------
# For reference:  check NY8 (not NY_nb)
# ---------------------------------------------------------------------------
plot(NY8, border="grey60", axes=TRUE)
text(coordinates(cities), labels=as.character(cities$names), font=2, cex=0.9)
text(bbox(NY8)[1,1], bbox(NY8)[2,2], labels="a)", cex=0.8)


plot(NY8, border="grey60", axes=TRUE)
points(TCE, pch=1, cex=0.7)
points(TCE, pch=3, cex=0.7)
text(coordinates(TCE), labels=as.character(TCE$name), cex=0.7, font=1, pos=c(4,1,4,1,4,4,4,2,3,4,2), offset=0.3)
text(bbox(NY8)[1,1], bbox(NY8)[2,2], labels="b)", cex=0.8)



# ---------------------------------------------------------------------------
# plot NY_nb on map
# ---------------------------------------------------------------------------
plot(NY8, border="grey60", axes=TRUE)

plot(NY_nb, coordinates(NY8), pch=19, cex=0.6, add=TRUE)



