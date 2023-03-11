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
# Moran's I Test of spatial autocorrelation
#   - apply Moran's I statistic to the SMR to account for the spatial distribution of the population
# ------------------------------------------------------------------------------
# library(DCluster)

# binary weights are used depending on whether two regions share a common boundary or not.

col.W <- nb2listw(ncCR85, zero.policy=TRUE)

moranI.test(Observed ~ offset(log(Expected)), as(nc, "data.frame"), 
            "negbin", 999, listw = col.W, n = length(ncCR85), S0 = Szero(col.W))


# -->
# Spatial autocorrelation is still found even after accouning for over-dispersion.



