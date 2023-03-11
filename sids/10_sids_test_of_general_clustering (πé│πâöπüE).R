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
# Tango's test of general clustering
#   - Tango pointed out that different types of interactions between neighbouring regions can be considered 
#     and proposed a measure of strength based on a decaying function of the distance between two regions. 
# ------------------------------------------------------------------------------
# we have taken approximate location of the county seats from nc.sids (columns x and y), which are in UTM (zone 18) projection.
# Note that using the centroids as the county seats as obtained by coordinates(nc) may lead to slightly different coordinates
# and this may have an impact on the results of this and other tests.
data(nc.sids)
str(nc.sids)


idx <- match(nc$NAME, rownames(nc.sids))
nc$x <- nc.sids$x[idx]
nc$y <- nc.sids$y[idx]
coords <- cbind(nc$x, nc$y)


dlist <- dnearneigh(coords, 0, Inf)
dlist <- include.self(dlist)
dlist.d <- nbdists(dlist, coords)



# ----------
# phi is a positive constant that reflects the strength of the dependence between areas
# and the scale at which the interaction occurs.
phi <- 100

# adjacency matrix
col.W.tango <- nb2listw(dlist, glist=lapply(dlist.d, 
                                            function(x, phi) {exp(-x/phi)}, phi=phi), style="C")


# ----------
set.seed(1)

tango.test(Observed~offset(log(Expected)), as(nc, "data.frame"), "negbin", 999, 
           listw=col.W.tango, zero.policy=TRUE)


#-->
# The test points out the presence of global clustering.


