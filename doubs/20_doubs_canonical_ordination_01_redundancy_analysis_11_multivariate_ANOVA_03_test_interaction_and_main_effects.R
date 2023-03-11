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
# Test the interaction
# ------------------------------------------------------------------------------

# Factors ele and pH (columns 1-4)  
interaction.rda <- rda(spe.hel[1:27, ], ele.pH.helm[, 5:8], ele.pH.helm[, 1:4])


anova(interaction.rda, permutations = how(nperm = 999))


# -->
# Interaction is NOT significant.



# ------------------------------------------------------------------------------
# Test the main effect
# ------------------------------------------------------------------------------

# The test the main factor ele.
# The factor pH and the interaction are assempled to form the matrix of covariables.
factor.ele.rda <- rda(spe.hel[1:27, ], ele.pH.helm[, 1:2], ele.pH.helm[, 3:8])


anova(factor.ele.rda, permutations = how(nperm = 999), strata = pH.fac)


# -->
# factor ele is significant



# ----------
# The test the main factor pH.
# The factor ele and the interaction are assempled to form the matrix of covariables.

factor.pH.rda <- rda(spe.hel[1:27, ], ele.pH.helm[, 3:4], ele.pH.helm[, c(1:2, 5:8)]) 


anova(factor.pH.rda, permutations = how(nperm = 999), strata = ele.fac)


# -->
# factor pH is NOT significant

