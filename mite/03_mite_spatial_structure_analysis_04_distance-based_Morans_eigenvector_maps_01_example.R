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
# Distance-based Moran eigenvector maps (dbMEM)
#
# 1. One-dimensional sampling
#    transect with 100 equispaced points.
#    The distance between adjacent points is 1. Function dbmem()
#    automatically computes the threshold value, 1 in this case.
# ------------------------------------------------------------------------------

# Generate transect points
tr100 <- 1 : 100



# ----------
# Creation of the dbMEM eigenfunctions with Moran's I corresponding to positive spatial correlation
# argument MEM.autocor = "positive" which is the default

tr100.dbmem.tmp <- dbmem(tr100, silent = FALSE)


( tr100.dbmem <- as.data.frame(tr100.dbmem.tmp) )



# ----------
# Display the eigenvalues
attributes(tr100.dbmem.tmp)$values



# ----------
# Number of (positive) eigenvalues
length(attributes(tr100.dbmem.tmp)$values)



# ----------
# Plot some dbMEM variables modelling positive spatial correlation along a transect.
par(mfrow = c(4, 2), mar = c(2,2,2,2))

somedbmem <- c(1, 2, 4, 8, 15, 20, 30, 40)

for(i in 1:length(somedbmem)){
  plot(tr100.dbmem[ ,somedbmem[i]], 
       type = "l", 
       xlab = "X coordinate", 
       ylab = c("dbMEM", somedbmem[i]))
}



# ------------------------------------------------------------------------------
# Distance-based Moran eigenvector maps (dbMEM)
#
# 2. Two-dimensional sampling
#    grid of equispaced points with 
#    smallest distance between points equal to 1
# ------------------------------------------------------------------------------

# Generate grid point coordinates
xygrid2 <- expand.grid(1:20, 1:20)



# ----------
# Creation of the dbMEM eigenfunctions with positive Moran's I 
xygrid2.dbmem.tmp <- dbmem(xygrid2)

xygrid2.dbmem <- as.data.frame(xygrid2.dbmem.tmp)



# ----------
# Count the eigenvalues
length(attributes(xygrid2.dbmem.tmp)$values)



# ----------
# Plot some dbMEM variables using s.value {adegraphics}

somedbmem2 <- c(1, 2, 5, 10, 20, 40, 80, 120, 189)

s.value(xygrid2, xygrid2.dbmem[ ,somedbmem2], 
        method = "color", 
        symbol = "circle", 
        ppoints.cex = 0.5
)



# Plot some dbMEM variables (slow)
# Using homemade function sr.value
# dev.new(title = "dbMEM variables (grid)", noRStudioGD = TRUE)
# par(mfrow = c(3,3))
# somedbmem2 <- c(1, 2, 5, 10, 20, 40, 80, 120, 189)
# for(i in 1:length(somedbmem2))
# {
#    sr.value(xygrid2, xygrid2.dbmem[ ,somedbmem2[i]], 
#             method = "greylevel", 
# 	  csize = 0.65,
#             sub = somedbmem2[i], 
#             csub = 1.8)
# }

