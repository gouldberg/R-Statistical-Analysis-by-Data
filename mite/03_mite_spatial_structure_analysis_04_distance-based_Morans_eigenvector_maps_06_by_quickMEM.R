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
# Single-step dbMEM analysis using function quickMEM()
#   - this function only requires a response data table and a table containing the site geographic coordinates (one or two dimensional)
#   - when the default arguments are applied, the function performs a complete dbMEM analysis: 
#     (1) checks whether the response data should be detrended and does it if a significant trend is identified
#     (2) constucts the dbMEM variabels and tests the global RDA
#     (3) runs forward seleciton using the dbMEM with positive spatial correlation
#     (4) runs RDA with the retained dbMEM variables and tests the canonical axes
#     (5) delivers the RDA results and plots maps of the significant canonical axes
# ------------------------------------------------------------------------------

mite.dbmem.quick <- quickMEM(mite.h, mite.xy)


summary(mite.dbmem.quick)



# ----------
# Eigenvalues
mite.dbmem.quick[[2]] 
# mite.dbmem.quick$eigenvalues



# ----------
# Results of forward selection
mite.dbmem.quick[[3]]
# mite.dbmem.quick$fwd.sel



# ----------
# Extract and plot RDA results from a quickMEM output (scaling 2)

par(mfrow = c(1,1))

plot(mite.dbmem.quick$RDA, scaling = 2)

sp.scores2 <- 
  scores(mite.dbmem.quick$RDA, 
         choices = 1:2, 
         scaling = 2, 
         display = "sp")

arrows(0, 0, 
       sp.scores2[ ,1] * 0.9, 
       sp.scores2[ ,2] * 0.9, 
       length = 0, 
       lty = 1, 
       col = "red"
)


# -->
# Scaling 2 shows the relationship of some species with some dbMEM variables.
# These correlations can be explored to reveal at which scale the species distributions are spatially structured.



