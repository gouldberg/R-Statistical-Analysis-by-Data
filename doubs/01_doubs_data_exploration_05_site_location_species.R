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
# Site - Species data:  species richness mapped on spatial map
# ------------------------------------------------------------------------------

# the number of species at each site
# site 8 have zero species

sit.pres <- apply(spe > 0, 1, sum)
sort(sit.pres)


graphics.off()
par(mfrow = c(1, 2))



# ----------
# Plot species richness vs. position of the sites along the river
plot(sit.pres, type = "s", las = 1,  col = "gray", main = "Species Richness vs. \n Upstream-Downstream Gradient", xlab = "Site numbers",  ylab = "Species richness")
text(sit.pres, row.names(spe), cex = .8, col = "red")

plot(spa, asp = 1, main = "Map of Species Richness", pch = 21, col = "white", bg = "brown", cex = 5 * sit.pres / max(sit.pres), xlab = "x coordinate (km)", ylab = "y coordinate (km)")
lines(spa, col = "light blue")



# -->
# The site at middle of downstream (ste 23, 24, 25) have under 10 species, very small richness compared to neighbouring sites.



# ------------------------------------------------------------------------------
# Site - Species Data:  mapped on spatial map by each species
# ------------------------------------------------------------------------------

species <- names(spe)

func_plot <- function(x){
  plot(spa, asp = 1, cex.axis = 0.8, col = "brown", cex = x, xlab = "x coordinate (km)", ylab = "y coordinate (km)")
  lines(spa, col = "light blue")
}

graphics.off()
par(mfrow = c(2,2))
obj <- c(5,6,7,8)
apply(spe[,obj], 2, func_plot)



# ------------------------------------------------------------------------------
# Bubble maps of the abundances of 4 fish species: Satr (Brown trout),  Thth (Gralyling),  Baba (Barbel),  Abbr (Common bream)
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow = c(2,2))

plot(spa, asp = 1, cex.axis = 0.8, col = "brown", cex = spe$Satr, main = "Brown trout", xlab = "x coordinate (km)", ylab = "y coordinate (km)")
lines(spa, col = "light blue")

plot(spa, asp = 1, cex.axis = 0.8, col = "brown", cex = spe$Thth, main = "Gralyling", xlab = "x coordinate (km)", ylab = "y coordinate (km)")
lines(spa, col = "light blue")

plot(spa, asp = 1, cex.axis = 0.8, col = "brown", cex = spe$Baba, main = "Barbel", xlab = "x coordinate (km)", ylab = "y coordinate (km)")
lines(spa, col = "light blue")

plot(spa, asp = 1, cex.axis = 0.8, col = "brown", cex = spe$Abbr, main = "Common bream", xlab = "x coordinate (km)", ylab = "y coordinate (km)")
lines(spa, col = "light blue")



# ------------------------------------------------------------------------------
# Bubble maps of the abundances of other fish species
# ------------------------------------------------------------------------------

spe_name <- c("Bullhead", "Brown trout", "Eurasian minnow", "Stone loach", "Grayling", "Vairone", "Common nase", "South-west European nase",
              "Common dace", "European chub", "Barbel", "Schneider", "Gudgeon", "Northern pike", "European pearch", "European bitterling", 
              "Pumpkinseed", "Rudd", "Common carp", "Tench", "Freshwater bream", "Black bullhead", "Ruffe", "Roach", "White bream", "Bleak", "European eel")

par(mfrow = c(3,3))
for(i in 1:9){
  plot(spa, asp = 1, cex.axis = 0.8, col = "brown", cex = spe[,i], main = spe_name[i], xlab = "x coordinate (km)", ylab = "y coordinate (km)");  lines(spa, col = "light blue")
}

for(i in 10:18){
  plot(spa, asp = 1, cex.axis = 0.8, col = "brown", cex = spe[,i], main = spe_name[i], xlab = "x coordinate (km)", ylab = "y coordinate (km)");  lines(spa, col = "light blue")
}

for(i in 19:27){
  plot(spa, asp = 1, cex.axis = 0.8, col = "brown", cex = spe[,i], main = spe_name[i], xlab = "x coordinate (km)", ylab = "y coordinate (km)");  lines(spa, col = "light blue")
}

