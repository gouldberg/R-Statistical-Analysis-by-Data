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
# Multiscale Ordination
#   - Autocorrelated residuals can alter the results of statistical tests.
#     Wagner (2003, 2004) used geostatistical methods to devise diagnostic tools allowing
#       (1) the partitioning of ordination results into distance classes
#       (2) the distinction between induced spatial dependence and spatial autocorrelation
#       (3) the use of variograms to check important assumptions such as independence of residuals and stationarity
# ------------------------------------------------------------------------------

# undetrended mite data vs environment RDA
mite.undet.env.rda <- rda(mite.h~., mite.env2)



# ----------
# grain of the variogram (size of a distance classes) is chosen to be the truncation threshold used in the dbMEM analysis
dmin = 1.011187

( mite.env.rda.mso <- 
    mso(mite.undet.env.rda, 
        mite.xy, 
        grain = dmin, 
        perm = 999) )



# ----------
par(mfrow = c(1, 1))

msoplot(mite.env.rda.mso, alpha = 0.05/7)



# -->
# the monotonic increase of the dashed line (sum of explained and residual empirical variograms)
# is the signature of the strong linear gradient present in the data.

# Note however that the variogram of the residuals (squares, bottom of the graph) shows no distance class with
# significant spatial correlation (after a global Bonferroni correction for 7 simultaneous tests)
# and the variogram is essentially flat.

# This means that the broad scale linear gradient is well explained by the environmental variables.


# -->
# When the species-environment correlations do not vary with scale,
# the dashed line remains within the boundaries of the confidence envelopes (full lines)
# This is not the case here, suggesting that it is not appropriate to run a non-spatial, global species-environment analysis with the 
# implicit assumption that the relationships are scale-invariant.

# On the contrary, we can expect the regression parameters to vary with scale, so that a global estimation is meaningless
# unless one controls for the regional-scale spatial structure causing the problem.






# MSO of the undetrended mite data vs environment RDA, controlling 
# for MEM
mite.undet.env.MEM <- 
  rda(mite.h, 
      mite.env2, 
      as.data.frame(MEM.select))
(mite.env.MEM.mso <- 
    mso(mite.undet.env.MEM, 
        mite.xy, 
        grain = dmin, 
        perm = 999))
dev.new(
  title = "MSO plot of the undetrended mite-environment RDA 
  controlling for MEM", 
  noRStudioGD = TRUE
)
msoplot(mite.env.MEM.mso, alpha = 0.05/7) 
# msoplot(mite.env.MEM.mso, 
#        alpha = 0.05/7, 
#        ylim = c(0, 0.0045)  # Expanded height to clear legend
)

