setwd("//media//kswada//MyFiles//R//doubs")

packages <- c("dplyr", "vegan", "cluster", "dendextend", "pvclust", "labdsv", "RColorBrewer")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Doubs
# ------------------------------------------------------------------------------

load("./data/Doubs.RData")


dim(spe)
car::some(spe)


dim(env)
car::some(env)


dim(spa)
car::some(spa)


dim(fishtraits)
car::some(fishtraits)


dim(latlong)
car::some(latlong)



# ----------
# remove site 8, which have zero species
spe <- spe[-8,]
env <- env[-8,]
spa <- spa[-8,]
latlong <- latlong[-8,]



# ------------------------------------------------------------------------------
# Correspondence Analysis (CA)
#  - CA of the raw species dataset (original species abundances)
#
#  - Chi-square is not influenced by double zeros; it is an asymmetrical D function. Therefore, CA is a method adapted to the analysis of species abundance data
#   without pre-transformation.
#  - Note that the data submitted to CA must be frequencies or frequency-like, dimensionally homogenous and non-negative;
#   that is the case of species counts, biomasses, or presence-absence data.
#  - In CA both the objects and the species are generally represented as points in the same joint plot --> most useful in ecology.
# 
#  - CA scaling 1:  rows (sites) are at the centroids of columns (species), appropriate if primarily interested in the ordination of OBJECTS (SITES)
#      (1) OBJECT (SITES) points that are close to one another are likely to be fairly similar in their species relative frequencies
#      (2) Any object found near the point representing a species is likely to contain a high contribution of that species.
#          For presence-absence data, the object is more likely to possess the state "1" for that species.
#  - CA scaling 2:  columns (species) are at the centroids of rows (sites), appropriate if primarily interested in the ordination of SPECIES
#      (1) SPECIES points that are close to one another are likely to be fairly similar relative frequencies along the objects (sites)
#      (2) Any species found near the point representing an object (site) is more likely to be found in that object (site) or
#          to have a higher frequency there than in objects that are further away in the joint plot.
#
#  - In ecology, almost exclusively used to analyze community composition data.
# ------------------------------------------------------------------------------

# Compute CA:  NOTE that we do not need to pre-transform species abundance data
( spe.ca <- vegan::cca(spe) )



# ----------
# site and sopecies scores
# default is scaling 2
summary(spe.ca)

summary(spe.ca, scaling = 1)



# ------------------------------------------------------------------------------
# Scree plot and broken stick model using vegan's screeplot.cca()
# ------------------------------------------------------------------------------

graphics.off();  par(mfrow=c(1,1));

screeplot(spe.ca, bstick = TRUE, npcs = length(spe.ca$CA$eig))



# -->
# 1st axis has a large eigenvalue.  In CA, values over 0.6 indicate a very strong gradient in the data.
# The eigenvalues are the same in both scalings.
# The scaling affects the eigenvectors to be drawn but not the eigenvalues.
# the 1st axis is extremely dominant.
