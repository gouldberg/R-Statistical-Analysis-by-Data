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
# Delaunay triangulation weighted by a function of distance
# ------------------------------------------------------------------------------

# Distances are ranged to maximum 1, and raised to power y. 
# After transformation of the distances by function f2, values near 1 are attributed to pairs of sites with easy exchange,
# values near 0 mean difficult communication.

f2 <- function(D, dmax, y) 1 - (D/dmax) ^ y 



# ----------
# Largest Euclidean distance on links belonging to the Delaunay triangulation
( max.d1 <- max(unlist(nbdists(mite.del, as.matrix(mite.xy)))) )



# ----------
# Power y is set from 2 to 10
mite.del.f2 <- 
  test.W(mite.h.det, mite.del, 
         MEM.autocor = "positive", 
         f = f2, 
         y = 2:10, 
         dmax = max.d1, 
         xy = as.matrix(mite.xy))



# ----------
# Unadjusted R^2 of best model
( R2.delW <- 
    mite.del.f2$best$AIC$R2[which.min(mite.del.f2$best$AIC$AICc)] )


# Adjusted R^2 of best model
RsquareAdj(
  R2.delW, 
  n = nrow(mite.h.det), 
  m = which.min(mite.del.f2$best$AIC$AICc)
)


