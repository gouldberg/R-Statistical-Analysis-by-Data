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
# CCA-based forward selection using vegan's ordistep()
#   - ordiR2step() and adespatial::forward.sel() can only compute RDA.
#   - ordistep() allows the use of factors like "slo" in env3
# ------------------------------------------------------------------------------


cca.step.forward <- ordistep(cca(spe ~ 1, data = env3), scope = formula(spe.cca),
                             direction = "forward",
                             permutations = how(nperm = 199))


cca.step.forward




# ------------------------------------------------------------------------------
# Parsimonious CCA
# ------------------------------------------------------------------------------

spe.cca.pars <- cca(spe ~ ele + oxy + bod, data = env3)



# ----------
anova(spe.cca.pars, permutations = how(nperm = 999))


anova(spe.cca.pars, permutations = how(nperm = 999), by = "axis")



# ----------
# R-square

RsquareAdj(spe.cca.pars)

RsquareAdj(spe.cca)



# ------------------------------------------------------------------------------
# Compare vairance inflation factors
# ------------------------------------------------------------------------------

# Note that vif.cca() takes into account the weights of the rows to compute the VIFs of the explanatory variables.
# If factors are present among the RDA or CCA explanatory variables, they are decomposed into binary variables before computation.

vif.cca(spe.cca)


vif.cca(spe.cca.pars)









