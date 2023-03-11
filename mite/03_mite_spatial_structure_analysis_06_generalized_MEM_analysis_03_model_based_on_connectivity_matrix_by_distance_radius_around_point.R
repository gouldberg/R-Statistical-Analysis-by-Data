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
# Connectivity matrix based on a distance (radius around points)
# ------------------------------------------------------------------------------

# Assessment of the relevant distances based on a multivariate variogram of the detrended mite data,
# with 20 distance classes.

( mite.vario <- variogmultiv(mite.h.det, mite.xy, nclass = 20) )


plot(
  mite.vario$d, 
  mite.vario$var, 
  ty = 'b', 
  pch = 20, 
  xlab = "Distance", 
  ylab = "C(distance)"
)



# -->
# It consists in the sum of univariate variograms computed over all species.
# The variance increases from 0 to 4 m.



# ----------
# Since the shortest distance to keep all sites connected is 1.011187 m  (see dbMEM analysys),
# we will explore a range of 10 evenly distributed distances ranging from this threshold up to 4.0m
# (approximately 4 times the threshold)

# Construction of 10 neighbourhood matrices (class nb)
# Vector of 10 threshold distances

( thresh10 <- seq(give.thresh(dist(mite.xy)), 4, le = 10) )



# ----------
# Create 10 neighbourhood matrices.
# Each matrix contains all connexions with lengths smaller or equal to the threshold value

list10nb <- 
  lapply(thresh10, 
         dnearneigh, 
         x = as.matrix(mite.xy), 
         d1 = 0)


# Display an excerpt of the first neighbourhood matrix
print(
  listw2mat(nb2listw(list10nb[[1]], style = "B"))[1:10,1:10], 
  digits = 1
)



# ----------
# Now we can apply the function test.W() to the 10 neighbourhood matrices.
# There are no weights on the links.

mite.thresh.res <- 
  lapply(list10nb, 
         function(x) test.W(x, 
                            Y = mite.h.det, 
                            MEM.autocor = "positive")
  )



# ----------
# Lowest AICc, best model, threshold distance of best model
mite.thresh.minAIC <- 
  sapply(mite.thresh.res, 
         function(x) min(x$best$AIC$AICc, na.rm = TRUE))


# Smallest AICc (best model among the 10)
min(mite.thresh.minAIC)


# Number of the model among the 10
which.min(mite.thresh.minAIC)


# Truncation threshold (distance)
thresh10[which.min(mite.thresh.minAIC)]



# -->
# The result is more interesting than that of the weighted Delaunay MEM.
# The AICc of the best model, obtained with a threshold of 2 m, is -99.85 with a model consisting of 4 MEM variables.


