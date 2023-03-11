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
# Recode environmental variables 3 to 4 into dummy binary variabels
# ------------------------------------------------------------------------------

substrate <- model.matrix( ~ mite.env[ ,3])[ ,-1]


shrubs <- model.matrix( ~ mite.env[ ,4])[ ,-1]


topography <- model.matrix( ~ mite.env[ ,5])[ ,-1]


mite.env2 <- cbind(mite.env[ ,1:2], substrate, shrubs, topography)


colnames(mite.env2) <- 
  c("SubsDens", "WatrCont", "Interface", "Litter", "Sphagn1",
    "Sphagn2", "Sphagn3", "Sphagn4", "Shrubs_Many", "Shrubs_None", 
    "topography")



# ------------------------------------------------------------------------------
# Test and forward selection of the environmental variables
# ------------------------------------------------------------------------------

mite.env.rda <- rda(mite.h ~., mite.env2)


( mite.env.R2a <- RsquareAdj(mite.env.rda)$adj.r.squared )



# ----------
mite.env.fwd <- 
  forward.sel(mite.h, mite.env2,
              adjR2thresh = mite.env.R2a, 
              nperm = 9999)


mite.env.fwd


mite.env.fwd$order


# ----------
env.sign <- sort(mite.env.fwd$order)


env.red <- mite.env2[ ,c(env.sign)]


colnames(env.red)

