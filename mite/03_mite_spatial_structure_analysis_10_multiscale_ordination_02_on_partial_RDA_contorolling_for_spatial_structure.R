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
# Multiscale Ordination on partial RDA, controlling for the spatial structure
# ------------------------------------------------------------------------------

# MEM.select:  here represented by the 6 dbMEM variables of our best model

mite.undet.env.MEM <- 
  rda(mite.h, 
      mite.env2, 
      as.data.frame(MEM.select))


# ----------
( mite.env.MEM.mso <- 
    mso(mite.undet.env.MEM, 
        mite.xy, 
        grain = dmin, 
        perm = 999) )



# ----------
msoplot(mite.env.MEM.mso, 
        alpha = 0.05/7, legend = FALSE)



# -->
# There is one significant spatial correlation left in the residuls (at distance class 1),
# and the variogram of the explained plus residual species-environmnet relationship stays within the confidence interval across all scales.

# MEM variables have also controlled for the major gradient in the data,
# resulting in a globally flat empirical variogram.

# The console message stating that the "Error variance of regression model underestimated by ...%"
# refers to the difference between the total residual variance and the sill of the residual variance.

# When the value is negative or Nan (not a number), no significant autocorrelation causes an underestimation of the global error value of the regressions.
# A positive value occuring if the residuals were significantly autocorrelated, would act as a warning that the condition of independent residuals is violated,
# thereby invalidating the statistical tests.




