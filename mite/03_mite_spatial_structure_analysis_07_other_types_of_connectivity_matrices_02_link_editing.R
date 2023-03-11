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
# Link editing
# 1. Interactive
# ------------------------------------------------------------------------------

plot(mite.del, mite.xy, col = "red", pch = 20, cex = 2)

title(main = "Delaunay triangulation")

mite.del2 <- edit.nb(mite.del, mite.xy)


# To delete a link, click on its two nodes. Follow on-screen
# instructions.
# Wait until you have finished editing before entering the next 
# command line. Suggestion: edit a link to site 23
# (see below why).



# ------------------------------------------------------------------------------
# Link editing
# 2. Alternatively, links can also be removed by command lines, 
# ------------------------------------------------------------------------------

# after having converted the nb object into an editable matrix:
mite.del.mat <- nb2mat(mite.del, style = "B")



# ----------
# Remove connection between objects 23 and 35:
mite.del.mat[23,35] <- 0

mite.del.mat[35,23] <- 0



# ----------
# Back-conversion into nb object:
mite.del3 <- neig2nb(neig(mat01 = mite.del.mat))

dev.new(title = "Delaunay with edited links", noRStudioGD = TRUE)

plot(mite.del3, mite.xy)



# ------------------------------------------------------------------------------
# Link editing
# Example: list of neighbours of core #23 for the Delaunay 
#          triangulation:
# ------------------------------------------------------------------------------

mite.del[[23]]      # Before editing


mite.del2[[23]]     # After interactive editing - depends on what 

# you have edited above.

mite.del3[[23]]     # After command line editing


