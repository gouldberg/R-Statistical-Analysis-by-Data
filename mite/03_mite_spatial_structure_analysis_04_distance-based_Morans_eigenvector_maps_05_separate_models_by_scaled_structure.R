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
# dbMEM analysis:  broad scale
# ------------------------------------------------------------------------------

( mite.dbmem.broad <- rda(mite.h.det ~ ., data = mite.dbmem[ ,c(1,3,4)]) )



# ----------
# global test and test by axis
anova(mite.dbmem.broad)


( axes.broad <- anova(mite.dbmem.broad, by = "axis") )



# ----------
# Number of significant axes
( nb.ax.broad <- 
    length(which(axes.broad[ , ncol(axes.broad)] <=  0.05)) )



# ----------
# Plot of the two significant canonical axes
mite.dbmembroad.axes <- 
  scores(mite.dbmem.broad, 
         choices = c(1,2), 
         display = "lc", 
         scaling = 1)


par(mfrow = c(1, 2))

sr.value(mite.xy, mite.dbmembroad.axes[ ,1])

sr.value(mite.xy, mite.dbmembroad.axes[ ,2])



# ----------
# Interpreting the broad-scaled spatial variation: regression of the two significant spatial canonical axes on the environmental variables

mite.dbmembroad.ax1.env <- 
  lm(mite.dbmembroad.axes[ ,1] ~ ., data = mite.env)


summary(mite.dbmembroad.ax1.env)



mite.dbmembroad.ax2.env <- 
  lm(mite.dbmembroad.axes[ ,2] ~ ., data = mite.env)


summary(mite.dbmembroad.ax2.env)



# -->
# Significante terms:  ShrubNone (absence of shurbs) and TomoHummock (microtopography)  (+ ShrubMany)
# axes 1:  estimated coefficients is negative
# axes 2:  estimated coefficients is positive



# ------------------------------------------------------------------------------
# dbMEM analysis:  medium scale
# ------------------------------------------------------------------------------

( mite.dbmem.med <- 
    rda(mite.h.det ~ ., data = mite.dbmem[ ,c(6,7,10,11)]) )



# ----------
# global test and test by axis
anova(mite.dbmem.med)


( axes.med <- anova(mite.dbmem.med, by = "axis") )



# ----------
# Number of significant axes
( nb.ax.med <- length(which(axes.med[ ,ncol(axes.med)] <=  0.05)) )



# ----------
# Plot of the significant canonical axes
mite.dbmemmed.axes <- 
  scores(mite.dbmem.med, choices = c(1,2), 
         display = "lc", 
         scaling = 1)


par(mfrow = c(1, 2))

sr.value(mite.xy, mite.dbmemmed.axes[ ,1])

sr.value(mite.xy, mite.dbmemmed.axes[ ,2])



# ----------
# Interpreting the medium-scaled spatial variation: regression of the two significant spatial canonical axes on the environmental variables
mite.dbmemmed.ax1.env <- 
  lm(mite.dbmemmed.axes[ ,1] ~ ., data = mite.env)


summary(mite.dbmemmed.ax1.env)


mite.dbmemmed.ax2.env <- 
  lm(mite.dbmemmed.axes[ ,2] ~ ., data = mite.env)


summary(mite.dbmemmed.ax2.env)



# -->
# Significante terms for axes1:  ShrubMany and TomoHummock (+ SubsDens)
# Significante terms for axes2:  two types of soil coverage (SubstrateLitter, SubstrateSphagn1) and marginally ShrubMany, ShrubNone



# ------------------------------------------------------------------------------
# dbMEM analysis:  fine scale
# ------------------------------------------------------------------------------

( mite.dbmem.fine <- 
    rda(mite.h.det ~ ., data = as.data.frame(mite.dbmem[ ,20])) )



# ----------
# global test
anova(mite.dbmem.fine)


# -->
# Analysis stops here, since the RDA is not significant.


# Something has occurred here, which is often found in fine-scale dbMEM analysis
# The fine-scaled dbMEM model is not significant.
# When significant, the fine-scaled dbMEM variables are mostly signatures of local spatial correlation generated by community dynamics.


