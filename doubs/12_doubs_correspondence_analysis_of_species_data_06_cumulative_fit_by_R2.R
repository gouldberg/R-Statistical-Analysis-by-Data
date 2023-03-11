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
# For quick assessment
# ------------------------------------------------------------------------------

source("./functions/CA.newr.R")


spe.CA.PL <- CA.newr(spe)

par(mfrow = c(1,2))

biplot.CA(spe.CA.PL, scaling = 1, cex = 1)

biplot.CA(spe.CA.PL, scaling = 2, cex = 1)



# ----------
# ordering of the data table following the 1st CA axis
# the table is transposed, as in the vegemite() output

summary(spe.CA.PL)  ## ??

t(spe[order(as.vector(spe.CA.PL$scaling1$sites[,1])), 
      order(as.vector(spe.CA.PL$scaling1$species[,1]))])



# ------------------------------------------------------------------------------
# Cumulative fit of species and sites in terms of R^2
# ------------------------------------------------------------------------------

# cumulative fit of species
head(spe.CA.PL$fit$cumulfit.spe)


# cumulative fit of sites
head(spe.CA.PL$fit$cumulfit.obj)



# ----------
spe.CA.PL$fit$cumulfit.spe["Phph",]

spe.CA.PL$fit$cumulfit.spe["Cogo",]

spe.CA.PL$fit$cumulfit.spe["Lele",]


# -->
# Phph is well fitted by axis 1 only (0.865)
# whereas Cogo is very well fitted by axes 1 and 2 together (0.914).
# On the other hand, Lele has no contribution to axes 1 adn 2 but gets half of its fit (0.494) on axis 3.


# -->
# This information could be used for graphical purposes, e.g. to display only the sites and species
# whose cumulative fit is at least 0.5 on axes 2.
# Another application would involve the selection of several axes for a further analysis,
# retaining only enough to reach a cumulative fit of 0.8 for 80% of sites



