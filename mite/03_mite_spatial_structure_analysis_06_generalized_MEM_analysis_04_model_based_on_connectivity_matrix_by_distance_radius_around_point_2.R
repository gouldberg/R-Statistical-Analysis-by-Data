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
# Variant: connections weighted by the complement of the power of the distances, 1-(d/dmax)^y
# ------------------------------------------------------------------------------

# Again, after transformation of the distances by function f2, values near 1 are attributed to pairs of sites with easy exchange,
# values near 0 mean difficult communication.

mite.thresh.f2 <- 
  lapply(list10nb, 
         function(x) test.W(x, Y = mite.h.det, 
                            MEM.autocor = "positive", 
                            f = f2, 
                            y = 2:10, 
                            dmax = max(unlist(nbdists(x, as.matrix(mite.xy)))), 
                            xy = as.matrix(mite.xy)))


# ----------
# Lowest AIC, best model
mite.f2.minAIC <- 
  sapply(mite.thresh.f2, 
         function(x) min(x$best$AIC$AICc, na.rm = TRUE))



# ----------
# Smallest AICc (best model among the 10)
min(mite.f2.minAIC)


# Number of the model among the 10
(nb.bestmod <- which.min(mite.f2.minAIC))


# Actual dmax of best model
(dmax.best <- mite.thresh.f2[nb.bestmod][[1]]$all[1,2])



# ----------
# Plot the links using the function plot.links()
par(mfrow = c(1,1))

plot.links(mite.xy, thresh = dmax.best)



# ----------
# Extraction of the champion MEM model
mite.MEM.champ <- 
  unlist(mite.thresh.f2[which.min(mite.f2.minAIC)], 
         recursive = FALSE)


mite.MEM.champ



# ----------
# Number of MEM variables in best model
(nvars.best <- which.min(mite.MEM.champ$best$AIC$AICc))



# ----------
# Eigenvalues
# mite.MEM.champ$best$AIC$values # No longer in output object
# MEM variables by order of added R2
mite.MEM.champ$best$AIC$ord



# ----------
# MEM variables selected in the best model
MEMid <- mite.MEM.champ$best$AIC$ord[1:nvars.best]

sort(MEMid)

MEM.all <- mite.MEM.champ$best$MEM

MEM.select <- mite.MEM.champ$best$MEM[ , sort(c(MEMid))]

colnames(MEM.select) <- sort(MEMid)



# ----------
# Unadjusted R2 of best model
R2.MEMbest <- mite.MEM.champ$best$AIC$R2[nvars.best]


# Adjusted R2 of best model
RsquareAdj(R2.MEMbest, nrow(mite.h.det), length(MEMid))



# -->
# With an AICc of -100.96,
# this is the best result of all our attempts in terms of AICc.
# 6 selected MEM variables

# R2 adjusted = 0.258.

# If want to avoid overfitting could use the result of the AIC-based MEM analysis and run a forwrd selection using the Blanchet et al. double stoppng rule.
