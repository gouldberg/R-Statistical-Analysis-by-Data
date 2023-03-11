setwd("//media//kswada//MyFiles//R//doubs")

packages <- c("dplyr", "vegan", "cluster", "dendextend", "pvclust", "labdsv", "RColorBrewer")
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



# ----------
# remove site 8, which have zero species
spe <- spe[-8,]
env <- env[-8,]
spa <- spa[-8,]
latlong <- latlong[-8,]



# ------------------------------------------------------------------------------
# Correspondence Analysis (CA) biplot
# ------------------------------------------------------------------------------

# Scaling 1: sites are centroids of species
# Scaling 2 (default): species are centroids of sites
 
par(mfrow = c(1, 2))

plot(spe.ca, scaling = 1, main = "CA fish abundances - biplot scaling 1")

plot(spe.ca, main = "CA fish abundances - biplot scaling 2")



# -->
# 1st axis opposes the lower section of teh stream (sites 19-30) to the upper portion.
# Many species appear close to the sites 19-30, indicating that they are more abundant downstream. Many of them are actually absent from the upper part of the river.
# The second axis contrasts the 10 upstream sites with the intermediate ones. Both groups of sites, which display short gradients on their own, are associated with characteristic species.
#
# The scaling 2 biplot shows how small groups of species are distributed among the sites.
# The grayling (Thth), the bullhead (Cogo) and teh varione (Teso) are found in the intermediate group of sites (11-18), while the brown trout (Satr), 
# the Eurasian minnow (Phph) and the stone loach (Babl) are found in a longer portion of the stream (approximately sites 1-18)
#
# In both of biplot 1 nad 2, interpretation of the species found near the origin of the graph should be done with care.
# This proximity could mean either that the species is at its optimum in the mid-range of the ecological gradients represented by the axes,
# or that it is present everywhere along the gradient.



# ------------------------------------------------------------------------------
# For quick assessment
# ------------------------------------------------------------------------------

source("./functions/CA.newr.R")


spe.CA.PL <- CA.newr(spe)

par(mfrow = c(1,2))

biplot.CA(spe.CA.PL, scaling = 1, cex = 1)

biplot.CA(spe.CA.PL, scaling = 2, cex = 1)



# ----------
# ordering of the data table following the 1st CA axis
# the table is transposed, as in the vegemite() output

summary(spe.CA.PL)  ## ??

t(spe[order(as.vector(spe.CA.PL$scaling1$sites[,1])), 
      order(as.vector(spe.CA.PL$scaling1$species[,1]))])

