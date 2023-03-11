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
# Reordering the data table on the basis of an ordination axis
# ------------------------------------------------------------------------------

# compact form of ordered table
vegan::vegemite(spe, spe.ca)



# ----------
# ordered abundance table as a heat map
# Ordering is not optimal since it is done only on the basis of the 1st CA axis.
# Therefore sites 1-10 and 11-18 (separated along axis 2) and their corresponding characteristic species are interspersed.

vegan::tabasco(spe, spe.ca)

