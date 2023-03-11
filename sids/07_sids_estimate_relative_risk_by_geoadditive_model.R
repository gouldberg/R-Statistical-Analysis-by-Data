setwd("//media//kswada//MyFiles//R//sids")

packages <- c("dplyr", "maptools", "spdep")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  sids
# ------------------------------------------------------------------------------
# read the SIDS data, boundaries of North Carolina and the adjacency structure of the North Carolina countries in GAL format.
nc_file <- system.file("shapes/sids.shp", package="spData")[1]

llCRS <- CRS("+proj=longlat +datum=NAD27")
nc <- readShapePoly(nc_file, ID="FIPSNO", proj4string=llCRS)

str(nc)


# ----------
# By using the argument region.id we made sure that the order of the list of neighbours ncCR85 is the same as the areas in the
# SpatialPolygonDataFrame object nc
( rn <- sapply(slot(nc, "polygons"), function(x) slot(x, "ID")) )

gal_file <- system.file("weights/ncCR85.gal", package="spData")[1]
ncCR85 <- read.gal(gal_file, region.id = rn)

ncCR85



# ------------------------------------------------------------------------------
# Calculate standardized mortality ratio (SMR) = Observed / Expected
# ------------------------------------------------------------------------------
# the population at risk is the number of births
nc$Observed <- nc$SID74

nc$Population <- nc$BIR74

r <- sum(nc$Observed)/sum(nc$Population)

( nc$Expected <- nc$Population * r )

( nc$SMR <- nc$Observed/nc$Expected )



# ------------------------------------------------------------------------------
# set colors
# ------------------------------------------------------------------------------
library(RColorBrewer)

logSMR <- log(nc$SMR[nc$SMR > 0])
nsteps <- 7
step <- (max(logSMR) - min(logSMR)) / nsteps
brks <- exp(min(logSMR) + (0:nsteps) * step)
brks[1] <- 0
cols <- c(rev(brewer.pal(3, "Blues")), brewer.pal(4, "Reds"))



# ------------------------------------------------------------------------------
# Important covariate
# ------------------------------------------------------------------------------
nc$nwprop <- nc$NWBIR74/nc$BIR74



# ------------------------------------------------------------------------------
# Geoadditive model by R2BayesX:  P-splines
#
#   - BayesX places particular emphasis on the use of geoadditive models and the use of non-linear terms in the linear predictor.
#     BayesX allows the specification of different types of smoothing Penalized splines in the linear predictor by means and are very popular in Biostatistics.
#   - In general, P-splines can be expressed as a mixed-effects model.
#     Developing this approach can be done with WinBUGS and INLA, but may be cumbersome and more difficult than by using BayesX
# ------------------------------------------------------------------------------
library(R2BayesX) 


# ----------
# P-splies (with 10 knots) to fit a non-linear term on a covariate (the proportion of non-white births)
bayesxps <- bayesx(Observed ~ sx(nwprop, bs="ps", knots=10),
                   offset= log(nc$Expected),
                   family="poisson", data=as(nc, "data.frame"))

summary(bayesxps)



# ----------
plot(bayesxps)

# -->
# The non-linear term on the covariate shows that it is probably a good idea to use a simple linear term.



# ------------------------------------------------------------------------------
# Geoadditive model by R2BayesX:  2-dimensional P-splines
# ------------------------------------------------------------------------------
# 2-dimensional P-spline to model spatial variation using the centroid coordinates of each area
nc$long <- coordinates(nc)[,1]
nc$lat <- coordinates(nc)[,2]

bayesxte <- bayesx(Observed ~ sx(long, lat, bs="te"),
                   offset= log(nc$Expected),
                   family="poisson", data=as(nc, "data.frame"))


# ----------
# Note how the areas of high risk (to the sourth and north-east) are captured by the spatial spline
plot(bayesxte, image=TRUE)

