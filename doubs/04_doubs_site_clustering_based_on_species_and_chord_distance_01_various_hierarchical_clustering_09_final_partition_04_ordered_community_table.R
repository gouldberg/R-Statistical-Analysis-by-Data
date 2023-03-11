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
# Heat Map
# ------------------------------------------------------------------------------

# Heat map
heatmap(as.matrix(spe.ch), Rowv = dend, symm = TRUE, margin = c(3,3))




# ------------------------------------------------------------------------------
# Ordered Community Table
# Species are ordered by their weighted averages on site scores.  Dots represnet absences.
# In order to avoid large values to clog the pircture, vegemite() can use external information to reorder and display a site-by-species data table where abundance values can be recoded in different ways.
# If no specific order is provided for the species, these are ordered by their weighted averages on the site scores.
# Note that the abundance scale accepted by vegemite() must be made of one-digit counts only (from 0 to 9)
# ------------------------------------------------------------------------------

# not using cluster information
or0 <- vegan::vegemite(spe)


heatmap(t(spe[rev(or0$species)]), Rowv = NA, Colv = dend, col = c("white", brewer.pal(5, "Greens")), scale = "none", margin = c(4,4), ylab = "Species (weighted averages of sites)", xlab = "Sites")



# ----------
# spe.chwo:  reordered result of Ward's 4 cluster
or <- vegan::vegemite(spe, use = spe.chwo)


heatmap(t(spe[rev(or$species)]), Rowv = NA, Colv = dend, col = c("white", brewer.pal(5, "Greens")), scale = "none", margin = c(4,4), ylab = "Species (weighted averages of sites)", xlab = "Sites")



# -->  Compare with the maps of four fish species
#  - Brown trout (Satr):  abundant in cluster 1 and 2
#  - Gralyling (Thth):  abundant in cluster 1 only at intermediate part of the river
#  - Barbel (Baba):  abundant in cluster 2 and 3
#  - Common bream (Abbr):  abundant in cluster 3


