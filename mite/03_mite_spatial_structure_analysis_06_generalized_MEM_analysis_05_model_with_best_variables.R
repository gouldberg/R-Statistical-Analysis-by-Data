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
# RDA constrained by the significant MEM
# ------------------------------------------------------------------------------

( mite.MEM.rda <- rda(mite.h.det~., as.data.frame(MEM.select)) )





# ----------
( mite.MEM.R2a <- RsquareAdj(mite.MEM.rda)$adj.r.squared )



# -->
# adjusted R2 is similar = 0.258
# but the dbMEM model requires 8 variables to reach this value and is thus less parsimonious than the MEM model
# (which needs only 6)



# ----------
# global test and test by axis
anova(mite.MEM.rda)

( axes.MEM.test <- anova(mite.MEM.rda, by = "axis") )



# ----------
# Number of significant axes
(nb.ax <- 
    length(which(axes.MEM.test[ ,ncol(axes.MEM.test)] <=  0.05)))



# ----------
# Plot maps of the significant canonical axes
mite.MEM.axes <- 
  scores(mite.MEM.rda, 
         choices = 1:nb.ax, 
         display = "lc", 
         scaling = 1
  )


if(nb.ax <=  2) 
{
  par(mfrow = c(1,2))
} else {  
  par(mfrow = c(2,2)) 
}


for(i in 1:ncol(mite.MEM.axes)) sr.value(mite.xy, mite.MEM.axes[ ,i])



# -->
# The result closely resembles that of the dbMEM analysis
# showing that the structures revealed by the two analyses are the same.



# ----------
# Maps of the significant MEM variables
if(ncol(MEM.select) <=  6) 
{
  par(mfrow = c(2,3))
}  else  { 
  par(mfrow = c(3,3)) 
}


for(i in 1:ncol(MEM.select))
{
  sr.value(mite.xy, 
           MEM.select[ ,i], 
           sub = sort(MEMid)[i], 
           csub = 2)
}




# Correlation of the retained MEM and dbMEM variables
cor(MEM.select, dbmem.red)



# -->
# The MEM differ more from the dbMEM than one would have expected ...
# Indeed, the two groups of spatial variables are rather weakly correlated.


