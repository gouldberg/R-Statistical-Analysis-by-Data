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
# latlong <- latlong[-8,]


# ----------
# data preparation
dfs <- env[, 1]
env2 <- env[, -1]
slo2 <- rep(".very_steep", nrow(env))
slo2[env$slo <= quantile(env$slo)[4]] <- ".steep"
slo2[env$slo <= quantile(env$slo)[3]] <- ".moderate"
slo2[env$slo <= quantile(env$slo)[2]] <- ".low"
slo2 <- factor(slo2, levels = c(".low", ".moderate", ".steep", ".very_steep"))
env3 <- env2
env3$slo <- slo2
envtopo <- env2[, c(1 : 3)]
envchem <- env2[, c(4 : 10)]



# ------------------------------------------------------------------------------
# 3. Variation partitioning without the 'nit' variable
# ------------------------------------------------------------------------------

# "nit" is strongly correlated to "ele" (r = -0.75).
# This means that in the RDA with the chemical variables, nit has explained some of the same structures as ele was explaining
# in the physiography RDA.
# We remove "nit" and check

envchem.pars2 <- envchem[, c(6, 7)]

( spe.part2 <- varpart(spe.hel, envchem.pars2, envtopo.pars) )

plot(spe.part2, digits = 2)



# -->
# overall amount of variation explained is about 0.595 (instead of 0.590)
# The [b] fraction has dropped from 0.196 to 0.088 and the fraction (elevation + slope) explained uniquely
# by physiography has absorbed the difference, rising from 0.142 to 0.249.
# This does not mena that elevation is a better causal candidate than nitrates to explain fish communities.

# Comparison of the two analyses rather indicates that nitrate content is related to elevation, just as the fish communities are,
# and that the interpretation of variables elevation and nitrates must be done with causation since theri causal link to the fish communities
# cannot be untangled.

# On the other hand, elevation is certainly related to other, unmeasured environmental variables that have na effect on the communiteis,
# making it a good proxy for them.




