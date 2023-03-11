# setwd("//media//kswada//MyFiles//R//mite")
setwd("//media//kswada//MyFiles//R//Spatial_data_analysis//mite")

packages <- c("dplyr", "ape", "spdep", "ade4", "adegraphics", "adespatial", "vegan")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  mite
#  - Substrate (7 classes),  Shrubs (3 classes), and Microtopography (2 classes)
# ------------------------------------------------------------------------------

load("./data/mite.RData")


dim(mite)
car::some(mite)


dim(mite.env)
car::some(mite.env)


dim(mite.xy)
car::some(mite.xy)



source("./functions/plot.links.R")
source("./functions/sr.value.R")
source("./functions/quickMEM.R")
source("./functions/scalog.R")



# ------------------------------------------------------------------------------
# Examples of connectivity matrices in decreasing order of connectivity
# ------------------------------------------------------------------------------

# All these neighbourhood matrices are stored in objects of class nb Delaunay triangulation


mite.del <- tri2nb(mite.xy) 


# Gabriel graph
mite.gab <- graph2nb(gabrielneigh(as.matrix(mite.xy)), sym = TRUE)


# Relative neighbourhood
mite.rel <- graph2nb(relativeneigh(as.matrix(mite.xy)), sym = TRUE)


# Minimum spanning tree
mite.mst <- mst.nb(dist(mite.xy))



# ----------
# Plots of the connectivity matrices
par(mfrow = c(2,2))

plot(mite.del, mite.xy, col = "red", pch = 20, cex = 1)
title(main = "Delaunay triangulation ")

plot(mite.gab, mite.xy, col = "purple", pch = 20, cex = 1)
title(main = "Gabriel graph")

plot(mite.rel, mite.xy, col = "dark green", pch = 20, cex = 1)
title(main = "Relative neighbourhood")

plot(mite.mst, mite.xy, col = "brown", pch = 20, cex = 1)
title(main = "Minimum spanning tree")

