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
# Measuring second-order intensity
#  - second-order properties of two points x and y reflects the probability of any pair of events occuring in the vicinities of x and y, respectively
#  - One way of measuring second-order properties when the spatial process is HPP is by means of the K-function
# 
#  - K-function measures the number of events found up to a given distance of any particular event
#      K(s) = E[N0(s)] / lambda
#      N0: the number of further events up to a distance s around an arbitrary event.
#    Ripley (1976) proposed an unbiased estimate for K(s)
#
#  - The value of the K-function for an homogeneous poisson process (HPP) is K(s) = pi * s^2
# ------------------------------------------------------------------------------

set.seed(30)


Kenvjap <- envelope(as(spjpines1, "ppp"), fun=Kest, r=r, nrank=2, nsim=99)

Kenvred <- envelope(as(spred, "ppp"), fun=Kest, r=r, nrank=2, nsim=99)

Kenvcells <- envelope(as(spcells, "ppp"), fun=Kest, r=r, nrank=2, nsim=99)



# ----------
Kresults <- rbind(Kenvjap, Kenvred, Kenvcells)

Kresults <- cbind(Kresults, y=rep(c("JAPANESE", "REDWOOD", "CELLS"), each=length(r)))



# ------------------------------------------------------------------------------
# Estimated K-function minus the theoretical value under CSR (Complete Spatial Randomness)
# ------------------------------------------------------------------------------

print(xyplot((obs - theo) ~ r | y , data=Kresults, type="l",
             ylim= c(-.06, .06), ylab=expression(hat(K) (r)  - pi * r^2),
             panel=function(x, y, subscripts) {
               Ktheo<- Kresults$theo[subscripts]
               lpolygon(c(r, rev(r)),
                        c(Kresults$lo[subscripts]-Ktheo, rev(Kresults$hi[subscripts]-Ktheo)),
                        border="gray", col="gray"
               )
               llines(r, Kresults$obs[subscripts]-Ktheo, lty=2, lwd=1.5, col="black")
             }))


# -->
# Value of K-hat(s) higher than pi * s^2 are characteristic of clustered processes,
# whilst values smaller than that are found when there exists competition between events (regular pattern)

# Note that the biological interpretations must be made cautiously because the underlying mechanisms are quite different
# and the scale of the interactions (if any) will probably be different for each point pattern.

# The Japanese trees point pattern is compatible with CSR because the estimated K-function is contained within the envelopes.

