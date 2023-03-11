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
# Redundancy analysis (RDA):  Selection of Explanatory Variables
#  - Seach for parsimony
#  - Seach for possible strong linear dependencies (correlations) among the explanatory variables in the RDA model, which could render the regression coeffs
#   of the explanatory variables in the model unstable.
#
#
# In RDA, forward selection is the method most often applied because it works even in cases where the number of explanatory variables is larger than (n-1).
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Forward selection of explanatory variables:  forward.sel()
#    - forward.sel() requires a response data matrix and an explanatory data matrix which, unfortunately, must contain quantitative variables only.
#      Factors must be recoded in the form of dummy variables.
# ------------------------------------------------------------------------------

# RDA with all explanatory variables except dfs (only quantitative variables)
spe.rda.all <- rda(spe.hel ~ ., data = env2)


# Global adjusted R^2
( R2a.all <- RsquareAdj(spe.rda.all)$adj.r.squared )


# Forward selection using forward.sel()
adespatial::forward.sel(spe.hel, env2, adjR2thresh = R2a.all)



# ------------------------------------------------------------------------------
# Forward selection using vegan's ordistep()
#    - This function allows the use of factors, and allows forward, backward and stepwise (a combination) selection.
#      It can be applied to RDA, CCA and db-RDA.
#      This is based on AIC-like criterion, but according to Oksanen, it may not be completely trustwarthy, and furthermore, experience shows that it tends to be very liberal.
# ------------------------------------------------------------------------------

mod0 <- rda(spe.hel ~ 1, data = env2)

step.forward <- vegan::ordistep(mod0, scope = formula(spe.rda.all), direction = "forward", permutations = how(nperm = 499))



# ----------
step.forward


RsquareAdj(step.forward)



# ------------------------------------------------------------------------------
# Backward elimination using vegan's ordistep()
# ------------------------------------------------------------------------------

step.backward <- vegan::ordistep(spe.rda.all, permutations = how(nperm = 499))


# With redundant argument direction = "backward":
# step.backward <- vegan::ordistep(spe.rda.all, direction = "backward", permutations = how(nperm = 499)


# ----------
step.backward


RsquareAdj(step.backward)



# ------------------------------------------------------------------------------
# Forward selection using vegan's ordiR2step()
# using a double stopping criterion (Blanchet et al. 2008a) and object env containing only quantitative variables.
# ------------------------------------------------------------------------------

step2.forward <- vegan::ordiR2step(mod0, scope = formula(spe.rda.all), direction = "forward", R2scope = TRUE, permutations = how(nperm = 199))



# ----------
step2.forward


RsquareAdj(step2.forward)



# ------------------------------------------------------------------------------
# Forward selection using vegan's ordiR2step()
# using a double stopping criterion (Blanchet et al. 2008a) and object env3 containing a factor.
# ------------------------------------------------------------------------------

mod00 <- rda(spe.hel ~ 1, data = env3)

spe.rda2.all <- rda(spe.hel ~ ., data = env3)


# ----------
step3.forward <- vegan::ordiR2step(mod00, scope = formula(spe.rda2.all), direction = "forward", permutations = how(nperm = 199))



# ----------
step3.forward


RsquareAdj(step3.forward)


# -->
# Note that the adjusted R^2 of the complete model is smaller than that of the complete RDA with only quantitative variables.
# Some information has been lost when transforming the quantitative slo variable into a factor with 4 levels.



# ------------------------------------------------------------------------------
# Partial forward selection with variable slo held constant
# ------------------------------------------------------------------------------

mod0p <- rda(spe.hel ~ Condition(slo), data = env2)

mod1p <- rda(spe.hel ~ . + Condition(slo), data = env2)

step.p.forward <- ordiR2step(mod0p,  scope = formula(mod1p), direction = "forward", permutations = how(nperm = 199))


# ----------
step.p.forward


RsquareAdj(step.p.forward)
