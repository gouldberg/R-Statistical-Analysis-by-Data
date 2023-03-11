setwd("//media//kswada//MyFiles//R//japanesepines")

packages <- c("dplyr", "spatstat", "maptools")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  japanesepines
# ------------------------------------------------------------------------------

data("japanesepines", package = "spatstat")

class(japanesepines)

str(japanesepines)



# ----------
# convert to Spatial Points class
spjpines <- as(japanesepines, "SpatialPoints")

str(spjpines)

# Convert to unit square using the elide methods
spjpines1 <- elide(spjpines, scale=TRUE, unitsq=TRUE)



# ----------
data(redwoodfull, package = "spatstat")

class(redwoodfull)

spred <- as(redwoodfull, "SpatialPoints")



# ----------
data(cells, package = "spatstat")

class(cells)

spcells <- as(cells, "SpatialPoints")



# ----------
summary(spjpines1)

summary(spred)

summary(spcells)



# ------------------------------------------------------------------------------
# F Function:  Distance from a point to the nearest event
#   - The F function measures the distribution of all distances from an arbitrary point of the plane to its nearest event.
#     This function is often called the empty space function because it is a measure of the average space left between events.
#   - Under CSR (Complete Spatial Randomness), F(r) = 1 - exp{ - lambda * pi * r^2 }, where lambda represents the mean number of events per unit area (or intensity)
# ------------------------------------------------------------------------------

# r <- seq(0, sqrt(2)/6, by = 0.005)
( r <- seq(0, sqrt(2)/6, by = 0.001) )



# ----------
set.seed(30)

Fenvjap <- envelope(as(spjpines1, "ppp"), fun=Fest, r=r, nrank=2, nsim=99)

Fenvred <- envelope(as(spred, "ppp"), fun=Fest, r=r, nrank=2, nsim=99)

Fenvcells <- envelope(as(spcells, "ppp"), fun=Fest, r=r, nrank=2, nsim=99)



# ----------
Fresults <- rbind(Fenvjap, Fenvred, Fenvcells)

Fresults <- cbind(Fresults, y=rep(c("JAPANESE", "REDWOOD", "CELLS"), each=length(r)))



# ------------------------------------------------------------------------------
# The empirical F functions and their associated 96% envelopes (nrank = 2)
# ------------------------------------------------------------------------------

print(xyplot(obs ~ theo | y , data=Fresults, type="l", 
             xlab = "theoretical", ylab = "observed", 
             panel=function(x, y, subscripts) {
               lpolygon(c(x, rev(x)), 
                        c(Fresults$lo[subscripts], rev(Fresults$hi[subscripts])),
                        border="gray", col="gray"
               )
               llines(x, y, col="black", lwd=2)
             }))



# -->
# The Japanese data are compatible with the CSR hypothesis, whereas the cells point pattern shows a regular pattern and the redwood points seem to
# be clustered.

