setwd("//media//kswada//MyFiles//R//doubs")

packages <- c("dplyr", "vegan", "RgoogleMaps", "googleVis", "gclus")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Doubs
# ------------------------------------------------------------------------------

load("./data/Doubs.RData")


dim(spe)
car::some(spe)


dim(env)
car::some(env)


dim(spa)
car::some(spa)


dim(fishtraits)
car::some(fishtraits)


dim(latlong)
car::some(latlong)



# ------------------------------------------------------------------------------
# Site location data (spatial data) on map
# ------------------------------------------------------------------------------

# Map of the 30 sampling sites along the Doubs River, Sites 1 and 2 are very close to each other

graphics.off();  par(mfrow=c(1,1));

plot(spa, asp = 1, type = "n", main = "Site Locations", xlab = "x coordinate (km)", ylab = "y coordinate (km)")

lines(spa, col = "light blue")

text(spa, row.names(spa), cex = 1, col = "red")
text(68, 20, "Upstream", cex = 1.2, col = "red")
text(15, 35, "Downstream", cex = 1.2, col = "red")




# ------------------------------------------------------------------------------
# Site location data (spatial data) on map
# 1. Using googleVis - dynamic map
# By default the plot method of the googleVis package uses the standard browser to display its output.
# ------------------------------------------------------------------------------

nom <- latlong$Site

latlong2 <- paste(latlong$LatitudeN, latlong$LongitudeE, sep = ":")

df <- data.frame(latlong2, nom, stringsAsFactors = FALSE)

mymap1 <- gvisMap(df, locationvar = "latlong2", tipvar = "nom", options = list(showTip = TRUE))

plot(mymap1)




# ------------------------------------------------------------------------------
# Site location data (spatial data) on map
# 2. Using RgoogleMaps
# This method provides a static image that is viewed in R.
# Map background is fetched from the Internet. The map limits are defined by the latitude and longitude of the site coordinates.
# Several types of maps are available. Here we use "terrain".
# ------------------------------------------------------------------------------

MapBackground(lat = latlong$LatitudeN, lon = latlong$LongitudeE, destfile =  "bckg", maptype = "terrain")


# Map is loaded into an R object
doubs.map <- ReadMapTile(destfile = "bckg")


# Site latitude-longitude coordinates are added
PlotOnStaticMap(doubs.map, lat = latlong$LatitudeN, lon = latlong$LongitudeE, cex = 0.7, col = "black", pch = 19)


# Plot of labels
TextOnStaticMap(doubs.map, latlong$LatitudeN - 0.0008 * abs(latlong$LatitudeN), latlong$LongitudeE, labels = latlong$Site, add = TRUE, cex = 0.5)



