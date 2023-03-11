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
# Marshall's spatially structured statistical model
#   - Marshall proposed another estimator that only requires local information to be computed.
#     For each region, a set of neighbours is defined and local means, variances and shrinkage factors are defined in a similar way
#     as in the global estimator, but considering only the areas in the neighbourhood.
#     This produces a local shrinkage for each area.
# ------------------------------------------------------------------------------

( nc$EBMrshloc <- EBlocal(nc$Observed, nc$Expected, ncCR85)$est )



# ----------
# 2 Marshall's EB estimator using local and global information
print(spplot(nc, c("EBMarshall", "EBMrshloc"), col.regions=cols, at=brks))


# -->
# The shrinkage produced by the local estimator is in general lower than for the gloabl estimator.
# Marshall's local estimator also shows a general shift towards the global mean, but it is less severe than for the others
# because only local information is employed.

# In general, Empirical Bayes smoothed estimators have venn criticised because they fail to cope with the uncertainty of the parameters of the model
# and to produce an overshrinkage since the parameters of the prior distributions are estimated from the data and remain fixed.




# ------------------------------------------------------------------------------
# Compare models
# ------------------------------------------------------------------------------
boxplot(as(nc, "data.frame")[,c("SMR", "EBPG", "EBLN", "EBMarshall", "EBMrshloc")], cex.lab=.5, las=1, horizontal=TRUE)



