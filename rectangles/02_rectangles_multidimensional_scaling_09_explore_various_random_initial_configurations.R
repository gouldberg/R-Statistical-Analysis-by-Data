setwd("//media//kswada//MyFiles//R//rectangles")

packages <- c("dplyr", "smacof")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  rectangles
# ------------------------------------------------------------------------------

data(rectangles, package = "smacof")


str(rectangles)


car::some(rectangles)



# ------------------------------------------------------------------------------
# Explore inter-correlations of point coordinates of many MDS solutions
# ------------------------------------------------------------------------------

res <- mds(rectangles, type = "ordinal")



# ----------
# icExplore() generates a large set of MDS solutions using random initial configurations, matches them all by Procrustean fittings,
# computes the inter-correlations of their point coordinates, and finally runs an (interval) MDS of these inter-correlations
set.seed(3)

solutions <- icExplore(rectangles, type = "ordinal", nrep = 75, returnfit = TRUE)


solutions



# ----------
graphics.off()

par(mfrow = c(1,1))
plot(solutions)


# -->
# The numbers in the plot represent the MDS configurations,
# and the size of the numbers corresponds to the Stresso f the solution (solution #68, #35, thus, have a poor fit to the data).

# The distances among the points represent the similarities of the configurations.
# The plot thus shows that there are many different local minima solutions when random initial configurations are used.



# ------------------------------------------------------------------------------
# plot interesting solutions
# ------------------------------------------------------------------------------

par(mfrow=c(1,2))

plot(solutions$mdsfit[[35]], main = "solution 35")
plot(solutions$mdsfit[[68]], main = "solution 68")



