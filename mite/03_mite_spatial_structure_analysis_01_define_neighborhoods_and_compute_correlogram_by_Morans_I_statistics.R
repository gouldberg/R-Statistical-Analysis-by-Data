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
# Transform the data
# ------------------------------------------------------------------------------

# centering the location data
mite.xy.c <- scale(mite.xy, center = TRUE, scale = FALSE)



# ----------
# plot(mite.xy)




# ------------------------------------------------------------------------------
# Define neighbourhoods
# ------------------------------------------------------------------------------


# We first define neighbourhoods of size <= 0.7 m around the points using the function dnearnigh()
# Search for neighbours of all points within a radius of 0.7 m and multiples (i.e., 0 to 0.7 m, 0.7 to 1.4 m and so on).

nb1 <- spdep::dnearneigh(as.matrix(mite.xy), 0, 0.7)


summary(nb1)



# -->
# 4 resions with no links: 1, 7, 10, 70



# ----------
# visualize these links

graphics.off();  par(mfrow=c(1,1));

plot.links(mite.xy, thresh = 0.7)



# ------------------------------------------------------------------------------
# Correlogram (Moran's I vs. distance class) of substrate density
#   - correlogram:  plot of the spatial correlation values against the distance classes
# ------------------------------------------------------------------------------

# Find successive lag orders of contiguous neighbours and compute Moran's I for each of these lag orders
# A lag order is the number of links, or steps in the linkage graph, between 2 points
# For instance, if sites A and C are connected through site B, 2 links (A-B and B-C) are needed to connect A and C, with are then are connected at lag order 2.

subs.dens <- mite.env[ ,1]



# ----------
subs.correlog <- sp.correlogram(nb1, subs.dens, order = 14, method = "I", zero.policy = TRUE)


subs.correlog


# -->
# distance class at 1 and 4 seems to be significant




# ----------
print(subs.correlog, p.adj.method = "holm")



# -->
# print() allows for correction of the p-values for multiple testing.
# In a correlogram, a test is performed for each lag (distance class), so that without correction, the overall risk of type I error is greatly increased./
# The Holm correction is applied here.
# Now the distance class at 4 is NOT significant



# ----------
# plot with confidence interval
plot(subs.correlog)



# -->
# This correlogram has a single singnificant distance class:
# there is positive spatial correlation at distance class 1 (i.e., 0.0 m to 0.7 m)
# Negative spatial correlation at distance class 4 (2.1 m to 2.8 m) is hinted at,
# but the coefficient is not significant after Holm correction for multiple testing.


# -->
# measurements taken more than 0.7 m or (conservatively) 2.8 m apart (the upper limit of class 4)
# can be considered as spatially independent with respect to substrate density.



