# setwd("//media//kswada//MyFiles//R//mite")
setwd("//media//kswada//MyFiles//R//Spatial_data_analysis//mite")

packages <- c("dplyr", "ape", "spdep", "ade4", "adegraphics", "adespatial", "vegan")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  mite
#  - Substrate (7 classes),  Shrubs (3 classes), and Microtopography (2 classes)
# ------------------------------------------------------------------------------

load("./data/mite.RData")


dim(mite)
car::some(mite)


dim(mite.env)
car::some(mite.env)


dim(mite.xy)
car::some(mite.xy)



source("./functions/plot.links.R")
source("./functions/sr.value.R")
source("./functions/quickMEM.R")
source("./functions/scalog.R")



# ------------------------------------------------------------------------------
# Mite - environment - trend - dbMEM variation partitioning
# ------------------------------------------------------------------------------

( mite.varpart <- 
   varpart(mite.h, env.red, mite.xy, dbmem.broad, dbmem.fine) )



# ----------
# Show the symbols of the fractions and plot their values
par(mfrow = c(1,2))

showvarparts(4, bg = c("red", "blue", "yellow", "green"))

plot(mite.varpart, 
     digits = 2, 
     bg = c("red", "blue", "yellow", "green")
) 



# -->
# Keep in mind that the R2 adjusttment is done for the fractions that are directly fitted by a linear EDA model,
# without resorting to partial RDA or multiple regression
# The individual fractions [a] to [p] are then computed by subtraction.

# Small negative R2 adjusted values frequently appear in this process.
# Small negative R2 adjusted values geenrally correspond to explanatory components that explain less of the response variables' variation
# than would be expected by chance for random normal deviates;
# so for all practical purposes, they can be interpreted as zeros and neglected during interpretation, although they must be taken into account
# when computing sums of subsets, or sums of all fractions (the latter is equal to 1)


# ----------
# The whole set of environmental and spatial variables explaines 52.5% of the variation of the undetrended mite data
# (see the R2 adjusted for "All" fractions)

# ----------
# The environmental variables alone (matrix X1 in the partitioning results) explain 40.8% of the variation,
# of which a mere 7.3% is not spatially structured (fraction [a]).
# This fraction represents species-environment relationships associated with local environmental conditions.

# ----------
# Fraction [e] (27.4%) is common to the environmental data and the linear (X-Y) gradient.
# This is a typical case of "induced spatial dependence",
# where the spatial structure of environmenttal factors produces a similar spatial structure in the response data.

# ----------
# Fraction [g] (8.0%) and fraction [h] (3.4%):
# common to the environmental and, respectively, the broad-scale and fine-scale dbMEM variables.
# These fractions might also be interpreted as signatures of induced spatial dependence,
# but with spatial patterns more complex than linear gradients.

# However, when some variation is explained jointly by environmental and spatial variables,
# one should be careful when inferring causal species-environment relationships:
# the correlations may be dues to a direct influence of the environmental variables on the species (direct induced spatial dependence),
# or to some unmeasured underlying process that is spatially structured and is influencing both the mite community
# and the environmental variables (e.g., spatial variation induced by a historical causal factor)

# ----------
# the variation explained by spatial variables independently of the environment is represented by fractions
# [b], [c], [d], [f], [i], [j] and [m] = 11.6%

# Broad and medium scales could be explained by unmeasured environmental variables,
# although one cannot exclude the influence of past events that could still show their marks in the mite community.

# Fine-scale structures are more likely explainable by spatial correlation produced by neutral biotic processes.
# Neutral processes include ecological drift (variation in species demography) due to random reproduction and random survival of individuals due to competition,
# predator-prey interactions, etc.) and random dispersal (migration in animals, propagule dispersion in plants).




