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
# Multiscale Ordination on detrended mite and environmental data
# ------------------------------------------------------------------------------

# Detrend mite data on Y coordinate
mite.h.det2 <- resid(lm(as.matrix(mite.h) ~ mite.xy[ ,2]))



# ----------
# Detrend environmental data on Y coordinate
env2.det <- as.data.frame(
  resid(lm(as.matrix(mite.env2) ~ mite.xy[ ,2])))



# ----------
# RDA
mitedet.envdet.rda <- rda(mite.h.det2 ~., env2.det)



# ----------
# MSO
(miteenvdet.rda.mso <- 
    mso(mitedet.envdet.rda, 
        mite.xy, 
        grain = dmin, 
        perm = 999))



# ----------
msoplot(miteenvdet.rda.mso, alpha = 0.05/7, legend = FALSE)
# msoplot(miteenvdet.rda.mso, alpha = 0.05/7, ylim = c(0, 0.006))



# -->
# less the broad-scale gradient
# that has been removed prior to the analysis by detrending

# residual variance shows no spatial correlation,
# and 2nd, 4th, and 5th classes of the variogram of explained plus residual data fall outside the confidence intervals.
# So the overall variogram shows no trend, but some regional spatial variance is present.



# ------------------------------------------------------------------------------
# Multiscale Ordination on detrended mite and environmental data, controlling for MEM
# ------------------------------------------------------------------------------

# grain of the variogram (size of a distance classes) is chosen to be the truncation threshold used in the dbMEM analysis
dmin = 1.011187


mite.det.env.MEM <- 
  rda(mite.h.det2, 
      env2.det, 
      as.data.frame(MEM.select))


(mite.env.MEM.mso <- 
    mso(mite.det.env.MEM, 
        mite.xy, 
        grain = dmin, 
        perm = 999))



msoplot(mite.env.MEM.mso, alpha = 0.05/7, legend = FALSE)
# msoplot(mite.env.MEM.mso, alpha = 0.05/7, ylim = c(0, 0.005))


# -->
# MEM control successfully for spatial dependence.


# This example shows that the potential of combining multivariate geostatistical methods with
# canonical ordination and MEM covariables when the aim of the study is to test for and model species-environment
# relationships while discriminating between the two major sources of concern related to spatial structures: spatial dependence and spatial autocorrelation


# -----------
# MOST IMPROTANT !!!!:
# Pure spatial structures revealed by canonical ordination are real and not due to confounding effects.
# since unique fraction of variation partitioning was significant.

# Note that MSO itself doe not reveal spatial structures, since no formal way of quantifying variance components in MSO
