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
# Compare classifications by construcing contingency table
# ------------------------------------------------------------------------------

# Single linkage and complete linkage is totally different
# The 26 sites of the 2nd group of the single linkage clustering are distributed over the 4 groups of the Ward clustering 
table(spech.single.g, spech.complete.g)

table(spech.single.g, spech.ward.g)


table(spech.UPGMA.g, spech.ward.g)



# The classifications of Ward and complete linkage are close to each other
# Ward and beta-flexible clustering, the classifications completely same
table(spech.complete.g, spech.ward.g)

table(spech.ward.g, spech.beta2.g)


