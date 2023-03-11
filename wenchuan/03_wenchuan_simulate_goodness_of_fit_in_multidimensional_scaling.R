setwd("//media//kswada//MyFiles//R//wenchuan")

packages <- c("dplyr", "MPsychoR")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Wenchuan
# ------------------------------------------------------------------------------

data("Wenchuan", package = "MPsychoR")

str(Wenchuan)



# ------------------------------------------------------------------------------
# Simulate Goodness-of-fit in MDS by randdomstress()
#   - A common mistake is to interpret stress value too mechanically by relying on Kruskal's rules of thumb
#     (0.20 poor, 0.10 fair, 0.05 good, 0.025 excellent and 0 perfect)
#     This is problematic due to the fact that the magnitude of the stress depends on the number of objects n (the larger n, the larger the stress).
#     In modern MDS applications, n can be fairly large.
#     Instead of using these rules of thumb, we can consider simulation approaches.
# ------------------------------------------------------------------------------

library(smacof)


set.seed(123)

rsvec <- randomstress(n = attr(Wdelta, "Size"), ndim = 2, nrep = 500, type = "ordinal")



# ----------
psych::describe(rsvec)

mean(rsvec) - 2 * sd(rsvec)



# -->
# Often in the literature, an observed stress values is considered "significant" if it is smaller than the lower 2 * 2d random stress boundary,
# which is clearly the case in our example.  (the stress was 0.133).



# ------------------------------------------------------------------------------
# Simulate Goodness-of-fit in MDS by permutation test
#   - The random stress criterion is not very shapr, as Cliff (1973) puts it, it tests the "nullest of all null hypotheses".
#     A sharper approach is to use a permutation test as described in Mair et al. (2016)
#     H0: "stress/configuration is obtained from a random permutation of dissimilarities"
# ------------------------------------------------------------------------------

set.seed(123)

permmds <- permtest(fit.wenchuan1, data = Wenchuan, method.dat = "euclidean", nrep = 500, verbose = FALSE)

permmds



# -->
# Since p < 0.05, we reject the H0 and conclude that stress/configuration is obtained from something other than a random permutation of dissimilarities.



# ------------------------------------------------------------------------------
# Scree plot with stress-1 values with varying dimensions
# ------------------------------------------------------------------------------

n <- attr(Wdelta, "Size")

svec <- NULL

for(i in 1:(n - 1)){ svec[i] <- mds(Wdelta, ndim = i, type = "ordinal")$stress }



# ----------
# scree plot
plot(1:(n-1), svec, type = "b", main = "MDS Scree Plot", pch = 20, xlab = "Number of Dimensions", ylab = "Stress")


# -->
# Based solely on the scree plot, we would probably pick a 3D solution, but the stress of the 2D solution is not too bad either.
# Compared to PCA and EFA, in MDS we aim for p = 2 or 3 since the main output is the configuration plot.



# ------------------------------------------------------------------------------
# Simulate with different random starts
#   - The stress target function is known to be bumpy. Thus it can easily happen that we end up in a local minimum.
#     By default, the functions in the smacof package use a classical scaling solution as starting configuration.
#     Here we use a simple ad hoc strategy by trying out different random starts and check whether the vest random start solution
#     leads to a lower stress value than the default setup.
# ------------------------------------------------------------------------------

# fit MDS with different random starts
set.seed(123)

fit.wenchuan <- NULL

for(i in 1:100){ fit.wenchuan[[i]] <- mds(Wdelta, type = "ordinal", init = "random") }



# ----------
# extract the best solution
ind <- which.min(sapply(fit.wenchuan, function(x) x$stress))

fit.wenchuan2 <- fit.wenchuan[[ind]]


# lowest stress (random start) and classical scaling start
fit.wenchuan2$stress
fit.wenchuan1$stress



# -->
# a particular random start provided a slightly better stress than our original solution.
# Since the stress difference is so minimal, we could pick either solution.





