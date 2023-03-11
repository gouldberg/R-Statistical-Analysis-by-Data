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
# Permutation Tests of RDA Results:  global test
#
#  - Due to widespread problems of non-normal distributions in ecologicasl data, classical parametric tests are often not appropriate in this field.
# ------------------------------------------------------------------------------

# Global test of the RDA result
# The test function is called anova() because it relies on a ratio between two measures of variation.
# DO NOT CONFUSE IT WITH CLASSICAL ANOVA TEST !!

anova(spe.rda, permutations = how(nperm = 999))



# ------------------------------------------------------------------------------
# Permutation Tests of all canonical axes
# ------------------------------------------------------------------------------

anova(spe.rda, by = "axis", permutations = how(nperm = 999))


# -->
# only 2 canonical axes are significant at the alpha = 0.05



# ------------------------------------------------------------------------------
# Apply Kaiser-Guttman criterion (generally quite liveral) to residual axes
# ------------------------------------------------------------------------------

# The eigenvalues are compared with the mean of the residual eigenvalues only

spe.rda$CA$eig

mean(spe.rda$CA$eig)

spe.rda$CA$eig[spe.rda$CA$eig > mean(spe.rda$CA$eig)]



# -->
# There may still some interesting variation in these data that has not been explained by our set of environmental variables.



