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
# Search based on Delaunay triangulation.
# ------------------------------------------------------------------------------

# Delaunay triangulation

( mite.del <- tri2nb(mite.xy) ) 


mite.del



# ----------
# test.W:  compute and test eigenvectors of spatial weighting matrices
# select among the MEM variables constructed on the basis of the Delaunay triangulation
mite.del.res <- 
  test.W(mite.h.det, 
         mite.del, 
         MEM.autocor = "positive")


# -->
# best model has an AICc value of -93.87 and is based on 6 MEM variables


mite.del.res$best



# ----------
# Unadjusted R^2 of the model with the smallest AICc value
( R2.del <- 
    mite.del.res$best$AIC$R2[which.min(mite.del.res$best$AIC$AICc)] )


# Adjusted R^2 of the model with the smallest AICc value
RsquareAdj(
  R2.del, 
  n = nrow(mite.h.det), 
  m = which.min(mite.del.res$best$AIC$AICc)
)


