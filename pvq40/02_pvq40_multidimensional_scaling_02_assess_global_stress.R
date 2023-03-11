setwd("//media//kswada//MyFiles//R//pvq40")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  PVQ40
# ------------------------------------------------------------------------------

data(PVQ40, package = "smacof")


str(PVQ40)


attributes(PVQ40)


car::some(PVQ40)



# ------------------------------------------------------------------------------
# Global Stress of MDS Solution:  Shepard Diagram
#   - Configuration Distances versus Proximities
# ------------------------------------------------------------------------------

# Configuration distances
res$confdist

# Disparities (transformed proximities, approximated distances, d-hats)
res$dhat



# ----------
graphics.off()

par(mfrow=c(1,1))

plot(res, plot.type = "Shepard")
# plot(res, plot.type = "Shepard", shepard.x = diss)


# -->
# each gray point is pair-wise proximities
# It represents the correlations among the items rather precisely, as can be seen by the relatively small scatter about the regression line.


res$stress



# ----------
# By hand-making

# Configuration distances
dist <- as.vector(res$confdist)

# Disparities (transformed proximities, approximated distances, d-hats)
dhat <- as.vector(res$dhat)


data0 <- as.matrix(diss)

dat <- data0[lower.tri(data0)]


par(mfrow = c(1,1))

# Configuration distances
plot(dat, dist, pch=21, cex=0.5, cex.axis=1 , cex.lab=1, col = gray(0.9),
     xlab="Similarity ratings (data)", ylab="Distances in MDS space" , lwd=1.8 )

# Disparities (transformed proximities)
points(dat, dhat, pch=16, col="blue", cex=0.5)

dat2 <- dat[order(dat, -dhat)]

dhat2 <- dhat[order(dat, -dhat)]

lines(dat2, dhat2, col="red", lwd=2)

for (i in 1:length(dat)){ segments(dat[i], dist[i], dat[i], dhat[i], col="blue", lwd=1.5, lty=2) }



# ------------------------------------------------------------------------------
# Global Stress of MDS Solution:  Stress-1 value
# ------------------------------------------------------------------------------

# NOT RUN:  THIS TAKES TIME !!!!!
# ex <- randomstress(n = dim(diss)[1], ndim = 2, nrep = 100, type = "ordinal")



# ----------
# quantile(ex, c(0.05, 0.95))

res$stress


# -->
# The stress value is smaller than these benchmark values.



# ----------
# graphics.off()
# hist(ex)
# psych::describe(ex)



# ----------
# The stress value is significant at the 5% level by 100 permutation test (stricter than the classical random Stress norms)
ex <- permtest(res)

graphics.off()
par(mfrow=c(1,1), mar = c(2,2,2,2))

hist(ex$stressvec, xlim = c(ex$stress.obs - 0.05, max(ex$stressvec) + 0.05))
abline(v = ex$stress.obs, col = "red")
points(ex$stress.obs, 0, cex = 2, pch = 16, col = "red")



# -->
# Evaluating a given Stress value is a complex matter. It involves a number of different parameters and considerations:

# The number of points (n):
#  - The greater n, the larger the expected Stress
#    (because the number of distances in an MDS solution grows almost quadratically as a function of n).

# The dimensionality of the MDS solution (m):
#  - The greater m, the smaller the expected Stress (because higher-dimensional spaces offer more freedom for an optimal positioning of points).

# The error component of the data:
#  - The greater the noise in the data, the larger the expected Stress (random data require maximal dimensionality).

# The MDS model:
#  - Stronger MDS models lead to higher Stress values than weaker MDS models,
#    because they leave less freedom for choosing optimal dhat values.

# The number of ties when using the primary approach to ties in ordinal MDS:
#  - The more ties (= equal values) in the proximities, the smaller the expected Stress.
#    The reason is that the primary approach to ties does not require that ties be mapped into equal distances,
#    so MDS has more freedom to find an optimal solution.

# The proportion of missing proximities (missing data):
#  - The more data are missing, the easier it is to find an MDS solution with small Stress.

# Outliers and other special cases:
#  - Different points contribute differently to the total Stress; eliminating particular points or setting certain data as missing
#    (e.g., because they are errors), can reduce the total Stress considerably.


