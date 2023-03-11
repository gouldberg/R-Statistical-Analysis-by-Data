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
# Comparing 2 typologies (contingency table approach)
# ------------------------------------------------------------------------------

env2 <- env[, -1]


# Dissimilarity indices for community ecologists
env.de <- vegdist(scale(env2), "euc")

env.kmeans <- kmeans(env.de, centers = 4, nstart = 100)

env.kmeans.g <- env.kmeans$cluster



# ----------
# Table crossing the species and environment 4-group typologies
table(spe.kmeans.g, env.kmeans.g)



# ------------------------------------------------------------------------------
# Test the relationship using a chi-square test
# ------------------------------------------------------------------------------

# Change the testing procedure to a permutation test
#    chisq.test(table(spe.kmeans.g, env.kmeans.g), 
#               simulate.p.value = TRUE)
# Test the relationship using a Fisher's exact test

fisher.test(table(spe.kmeans.g, env.kmeans.g))

