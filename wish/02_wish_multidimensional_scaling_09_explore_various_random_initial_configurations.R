setwd("//media//kswada//MyFiles//R//wish")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  wish
# ------------------------------------------------------------------------------

data(wish, package = "smacof")


wish


car::some(wish)



# ------------------------------------------------------------------------------
# Explore inter-correlations of point coordinates of many MDS solutions
# ------------------------------------------------------------------------------

# method = 7 is converting "cooccurence" similarities into dissimilarities
diss_7 <- sim2diss(wish, method = 7)


res <- mds(diss_7, type = "ordinal")



# ----------
# icExplore() generates a large set of MDS solutions using random initial configurations, matches them all by Procrustean fittings,
# computes the inter-correlations of their point coordinates, and finally runs an (interval) MDS of these inter-correlations
set.seed(3)

solutions <- icExplore(diss_7, type = "ordinal", nrep = 75, returnfit = TRUE)


solutions



# ----------
graphics.off()

par(mfrow = c(1,1))
plot(solutions)


# -->
# The numbers in the plot represent the MDS configurations,
# and the size of the numbers corresponds to the Stress of the solution (solution #64, thus, has a poor fit to the data).

# The distances among the points represent the similarities of the configurations.
# The plot thus shows that there are many different local minima solutions when random initial configurations are used.

# Many of these solutions have a poor fit, but there are two clusters of  highly similar configurations on the right-hand side
# that all have relatively low Stress.



# ------------------------------------------------------------------------------
# plot interesting solutions
# ------------------------------------------------------------------------------

par(mfrow=c(1,2))

plot(solutions$mdsfit[[9]], main = "solution 9")
plot(solutions$mdsfit[[25]], main = "solution 25")



