setwd("//media//kswada//MyFiles//R//crimes")

packages <- c("dplyr", "smacof", "lattice")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  crimes
# ------------------------------------------------------------------------------
data("crimes", package = "smacof")

str(crimes)

car::some(crimes)



# ------------------------------------------------------------------------------
# Multidimensional Scaling
# ------------------------------------------------------------------------------

# Note that smacof always requires that the data either come as dissimilarities,
# convert correlations to dissimilarities
diss <- sim2diss(crimes, method = "corr")



# ----------
# Multidimensional Scaling
result <- mds(diss, type = "interval")

result



# ----------
# plot MDS configuration
plot(result)



# -->
# Crimes where psersons come to harm emerge in one such neighborhood,
# and property crimes from another neighborhood.
# Hence, if the Murder rate is high in a state, then Assault and Rape also tend to be relatively frequent.

# Robberty lies between these neighborhoods, possibly because RObbery may not only damage the victims' possessions but also their bodies.

# The second principal axis is difficult to interpret.
# On this axis, Larceny and Robbery are farthest apart.
# Hence, these two crimes might lead to a meaningful interpretation of the second dimension,
# but no compelling interpretation seems to offer itself.
# This dimension may simply represent the noise of the data.
# So one can ask whether it suffices to represent in a one-dimensional MDS space.

