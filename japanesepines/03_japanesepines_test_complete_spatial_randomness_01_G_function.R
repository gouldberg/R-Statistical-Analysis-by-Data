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
# Spatial Distribution follows CSR (Complete Spatial Randomness) ?
#   - The compatibility with CSR of the point pattern can be assessed by plotting the empirical function G-hat(d) against the theoretical expectation.
#
# G Function:  Distance to the Nearest Event
#   - The G function measures the distribution of the distances from an arbitrary event to its nearest event.
#     If these distances are defined as d(i) = min(j){ d(ij) }, i = 1,...,n, then the G function can be estimated as G-hat(r) = #{d(i): d(i) <= r} / n
#     where the numerator is the number of elements in the set of distances that are lower than or equal to d and n is the total number of points.
#   - Under CSR (Complete Spatial Randomness), G(r) = 1 - exp{ - lambda * pi * r^2 }, where lambda represents the mean number of events per unit area (or intensity)
# ------------------------------------------------------------------------------

# r <- seq(0, sqrt(2)/6, by = 0.005)
( r <- seq(0, sqrt(2)/6, by = 0.001) )



# ----------
# soatstat::envelope()
# nrank = 2:  96% poiintwise envelopes of the same point pattern examined using the G function
# envelope() is a very flexible function that can be used to compute Monte Carlo envelopes of a certain type of functions.
# Basically, it works by randomly simulating a number of point patterns so that the summary function is computed for all of them.
# The resulting values are then used to compute point-wise or global Monte Carlo envelopes.

set.seed(120109)

envjap <- envelope(as(spjpines1, "ppp"), fun=Gest, r=r, nrank=2, nsim=99)

envred <- envelope(as(spred, "ppp"), fun=Gest, r=r, nrank=2, nsim=99)

envcells <- envelope(as(spcells, "ppp"), fun=Gest, r=r, nrank=2, nsim=99)



# ----------
Gresults <- rbind(envjap, envred, envcells) 

Gresults <- cbind(Gresults, y=rep(c("JAPANESE", "REDWOOD", "CELLS"), each=length(r)))

Gresults



# ------------------------------------------------------------------------------
# The empirical G functions and their associated 96% envelopes (nrank = 2)
# ------------------------------------------------------------------------------

print(xyplot(obs ~ theo | y , data=Gresults, type="l", 
             xlab = "theoretical", ylab = "observed", 
             panel=function(x, y, subscripts) {
               lpolygon(c(x, rev(x)), 
                        c(Gresults$lo[subscripts], rev(Gresults$hi[subscripts])),
                        border="gray", col="gray"
               )
               llines(x, y, col="black", lwd=2)
             }))


# -->
# The results show that only the Japanese trees seem to be homogeneously distributed, whilest the redwood seeds show a clustered pattern
# and the location of the cells shows a more regular pattern.


