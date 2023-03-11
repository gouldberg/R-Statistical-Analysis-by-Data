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
# Try to identify groups of eigenfunctions
# Scalogram of the variance explained by all dbMEM eigenfunctions
# ------------------------------------------------------------------------------

graphics.off()
par(mfrow = c(1,1))

scalog(mite.dbmem.rda)



# -->
# explained variance (unadjusted R^2) of the detrended,
# Hellinger-transformed mite data explained by the dbMEM eigenfuncions, with color-coded permutation test results.
# The p-values are more liberal than the ones obtained with the forward selection based on the Balanchet et al. (2008a) double stopping criterion.


# -->
# 1,3,4:  p <= 0.001
# 6,7,11:  p <= 0.01
# 5, 10, 20:  p <- 0.05



# ------------------------------------------------------------------------------
# Check by Maps of the 8 significant dbMEM variables
# ------------------------------------------------------------------------------

par(mfrow = c(2, 4))

for(i in 1 : ncol(dbmem.red))
{
  sr.value(mite.xy, 
           dbmem.red[ ,i], 
           #             sub = paste("dbMEM",i), 
           sub = paste("dbMEM",dbmem.sign[i]), 
           csub = 2)
}




# Maps with s.value()
s.value(mite.xy, dbmem.red, symbol = "circle")



# -->
# dbMEM 1,3,4:  broad scale
# dbMEM 6,7,10,11:  medium scale
# dbMEM 20: fine   scale




