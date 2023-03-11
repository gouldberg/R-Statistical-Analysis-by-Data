setwd("//media//kswada//MyFiles//R//jo")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  JO
# ------------------------------------------------------------------------------

data("JO", package = "FactoMineR")

str(JO)

dim(JO)


head(JO)



# ------------------------------------------------------------------------------
# Global Stress of MDS Solution:  Shepard Diagram
#   - Configuration Distances versus Dissimilarities
# ------------------------------------------------------------------------------

# Configuration distances
fit_1o$confdist


# Disparities (transformed proximities, approximated distances, d-hats)
fit_1o$dhat



# ----------
graphics.off()

par(mfrow=c(2,2))

plot(fit_1i, plot.type = "Shepard", main = "1-interval")
plot(fit_1o, plot.type = "Shepard", main = "1-ordinal")
plot(fit_2i, plot.type = "Shepard", main = "2-interval")
plot(fit_2o, plot.type = "Shepard", main = "2-ordinal")



# -->
# Note that interval based MDS (left panel) is more stronger model and will have higher stress-1 values

fit_1i$stress
fit_1o$stress
fit_2i$stress
fit_2o$stress


# -->
# distmat2 and type = "ordinal" has lowest stress



# ------------------------------------------------------------------------------
# Global Stress of MDS Solution:  Shepard Diagram
#   - Configuration Distances versus Similarities (Proximities)
# ------------------------------------------------------------------------------

# graphics.off()

# par(mfrow=c(1,2))

# plot(fit_1i, plot.type = "Shepard", shepard.x = JO)

# plot(fit_2o, plot.type = "Shepard", shepard.x = JO)



# -->
# each gray point is pair-wise proximities out of XX countries



# ------------------------------------------------------------------------------
# Global Stress of MDS Solution:  Stress-1 value
# Stress calculation for random dissimilarities
# ------------------------------------------------------------------------------

ex <- randomstress(n = dim(distmat)[1], ndim = 2, nrep = 500, type = "interval")

ex2 <- randomstress(n = dim(distmat2)[1], ndim = 2, nrep = 500, type = "ordinal")



# ----------
quantile(ex, c(0.05, 0.95))
fit_1i$stress


quantile(ex2, c(0.05, 0.95))
fit_2o$stress


# -->
# The stress value is smaller than these benchmark values.



# ----------
graphics.off()
par(mfrow=c(2,1), mar = c(2,2,2,2))

hist(ex)
hist(ex2)

psych::describe(ex)
psych::describe(ex2)



# ----------
# The stress value is significant at the 5% level by 100 permutation test (stricter than the classical random Stress norms)
ex <- permtest(fit_1i)
ex2 <- permtest(fit_2o)

graphics.off()
par(mfrow=c(2,1), mar = c(2,2,2,2))

hist(ex$stressvec, xlim = c(ex$stress.obs - 0.05, max(ex$stressvec) + 0.05))
abline(v = ex$stress.obs, col = "red")
points(ex$stress.obs, 0, cex = 2, pch = 16, col = "red")

hist(ex2$stressvec, xlim = c(ex2$stress.obs - 0.05, max(ex2$stressvec) + 0.05))
abline(v = ex2$stress.obs, col = "red")
points(ex2$stress.obs, 0, cex = 2, pch = 16, col = "red")



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

