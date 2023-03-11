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
# Trend-surface analysis:  example
#   - The crudest way to model the spatial structure of the response data is to regress the response data on the X-Y coordinates of the sampling sites.
#     A way of allowing curvilinear structures to be modelled is to add polynomial terms of the coordinates to the explanatory data.
#     Second- and third-degree terms are often applied.
#     It is better to centre (but not standardize, lest one distort the aspect-ratio of the sampling design) the X and Y coordinates
#     before computing the polynomial terms, to make at least the second-degree terms less correlated.
# ------------------------------------------------------------------------------

# Simple models on a square, regularly sampled surface

# Construct and plot a 10 by 10 grid
xygrid <- expand.grid(1:10, 1:10)

plot(xygrid)



# ----------
# Centering
xygrid.c <- scale(xygrid, scale = FALSE)


# Create and plot some first, second and third-degree functions of X and Y
X <- xygrid.c[ ,1]

Y <- xygrid.c[ ,2]

XY <- X + Y
XY2 <- X ^ 2  +  Y ^ 2
XY3 <- X ^ 2 - X * Y - Y ^ 2
XY4 <- X + Y  +  X ^ 2  +  X * Y  +  Y ^ 2
XY5 <- X ^ 3  +  Y ^ 3
XY6 <- X ^ 3  +  X ^ 2 * Y  +  X * Y ^ 2  +  Y ^ 3
XY7 <- X  +  Y  +  X ^ 2  +  X * Y  +  Y ^ 2  +  X ^ 3  +  X ^ 2 * Y  +  X * Y ^ 2  +  Y ^ 3

xy3deg <- cbind(X, Y, XY, XY2, XY3, XY4, XY5, XY6, XY7)


s.value(xygrid, xy3deg, symbol = "circle")



# ----------
# Try other combinations, for instance with minus signs or with coefficients not equal to 1.
XY <- X + Y
XY2 <- X - Y
XY3 <- X + 2 * Y
XY4 <- X - 2 * Y
XY5 <- X + 4 * Y
XY6 <- X - 4 * Y
XY7 <- X * Y

xy3deg <- cbind(X, Y, XY, XY2, XY3, XY4, XY5, XY6, XY7)

s.value(xygrid, xy3deg, symbol = "circle")

