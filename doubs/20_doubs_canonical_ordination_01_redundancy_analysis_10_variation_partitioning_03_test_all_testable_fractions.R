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
# Tests of all testable fractions
# ------------------------------------------------------------------------------

# Test of fraction [a+b]
anova(rda(spe.hel, envchem.pars), permutations = how(nperm = 999))


# Test of fraction [b+c]
anova(rda(spe.hel, envtopo.pars), permutations = how(nperm = 999))


# Test of fraction [a+b+c]
env.pars <- cbind(envchem.pars, envtopo.pars)
anova(rda(spe.hel, env.pars), permutations = how(nperm = 999))


# Test of fraction [a]
anova(rda(spe.hel, envchem.pars, envtopo.pars), permutations = how(nperm = 999))


# Test of fraction [c]
anova(rda(spe.hel, envtopo.pars, envchem.pars), permutations = how(nperm = 999))



# -->
# As expected, forward-selecting the explanatory variables independently ineach subset (chemistry and physiography)
# does nothing to prevent inter-set correlations:
# some of the variables retained in each set are correlated with those of the other set.

# Therefore, fraction [b] remains important.
