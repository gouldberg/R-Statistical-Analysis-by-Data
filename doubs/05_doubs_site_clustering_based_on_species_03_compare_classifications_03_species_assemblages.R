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
# Species Assemblages:  Simple statistics on group contents
# ------------------------------------------------------------------------------

# compute mean species abundances in the 4 groups from the optimized Ward clustering

groups <- as.factor(spech.ward.gk)

spe.means <- matrix(0, ncol(spe), length(levels(groups)))

row.names(spe.means) <- colnames(spe)

for (i in 1:ncol(spe)) spe.means[i, ] <- tapply(spe[, i], spech.ward.gk, mean)



# ----------
# Mean species abundances of the four groups
group1 <- round(sort(spe.means[, 1], decreasing = TRUE), 2)

group2 <- round(sort(spe.means[, 2], decreasing = TRUE), 2)

group3 <- round(sort(spe.means[, 3], decreasing = TRUE), 2)

group4 <- round(sort(spe.means[, 4], decreasing = TRUE), 2)



# ----------
# Species with abundances greater than group mean species abundance
group1.domin <- which(group1 > mean(group1))

group1

group1.domin


#... same for other groups
