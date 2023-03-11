setwd("//media//kswada//MyFiles//R//temperature")

packages <- c("dplyr", "MPsychoR", "FactoMineR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  temperature
# ------------------------------------------------------------------------------

temperature <- read.table("temperature.csv", header = TRUE, sep = ";", dec = ".", row.names = 1)

str(temperature)

dim(temperature)


temperature



# ------------------------------------------------------------------------------
# Global Stress of MDS Solution:  Shepard Diagram
#   - Configuration Distances versus Dissimilarities
# ------------------------------------------------------------------------------

# Configuration distances
fitv_i$confdist

# Disparities (transformed proximities, approximated distances, d-hats)
fitv_i$dhat



# ----------
graphics.off()

par(mfrow=c(2,2))

plot(fitv_i, plot.type = "Shepard")
plot(fitv_o, plot.type = "Shepard")
plot(fiti_i, plot.type = "Shepard")
plot(fiti_o, plot.type = "Shepard")


# -->
# This is rare case ??:  ordinal model's stress is less than that of interval model

fitv_i$stress
fitv_o$stress
fiti_i$stress
fiti_o$stress



# ------------------------------------------------------------------------------
# Global Stress of MDS Solution:  Shepard Diagram
#   - Configuration Distances versus Similarities (Proximities)
# ------------------------------------------------------------------------------

graphics.off()

par(mfrow=c(1,2))

plot(fitv_i, plot.type = "Shepard", shepard.x = cormat_v)

plot(fiti_i, plot.type = "Shepard", shepard.x = cormat_i)



# -->
# each gray point is pair-wise proximities



# ------------------------------------------------------------------------------
# Global Stress of MDS Solution:  Stress-1 value
# Stress calculation for random dissimilarities
# ------------------------------------------------------------------------------

ex_v <- randomstress(n = dim(cormat_v)[1], ndim = 2, nrep = 500, type = "ordinal")

ex_i <- randomstress(n = dim(cormat_i)[1], ndim = 2, nrep = 500, type = "ordinal")



# ----------
quantile(ex_v, c(0.05, 0.95))
quantile(ex_i, c(0.05, 0.95))

fitv_o$stress
fiti_o$stress



# -->
# The stress value is smaller than these benchmark values.



# ----------
graphics.off()
par(mfrow=c(2,1), mar = c(2,2,2,2))

hist(ex_v)
hist(ex_i)

psych::describe(ex_v)
psych::describe(ex_i)



# ----------
# The stress value is significant at the 5% level by 100 permutation test (stricter than the classical random Stress norms)
ex_v <- permtest(fitv_o)
ex_i <- permtest(fiti_o)

graphics.off()
par(mfrow=c(2,1), mar = c(2,2,2,2))

hist(ex_v$stressvec, xlim = c(ex_v$stress.obs - 0.05, max(ex_v$stressvec) + 0.05))
abline(v = ex_v$stress.obs, col = "red")
points(ex_v$stress.obs, 0, cex = 2, pch = 16, col = "red")

hist(ex_i$stressvec, xlim = c(ex_i$stress.obs - 0.05, max(ex_i$stressvec) + 0.05))
abline(v = ex_i$stress.obs, col = "red")
points(ex_i$stress.obs, 0, cex = 2, pch = 16, col = "red")



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

