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
# Redundancy analysis (RDA):
# - Partial RDA: whether water chemistry significantly explains the fish species patterns when the effect of the topographic gradient is held constant
# ------------------------------------------------------------------------------

# Simple syntax; X and W may be in separate tables of quantitative variables

( spechem.physio <- rda(spe.hel, envchem, envtopo) )



# ----------
# Formula interface; X and W variables must be in the same data frame
# The results of the two analyses are identical.
( spechem.physio2 <- 
    rda(spe.hel ~ pH + har + pho + nit + amm + oxy + bod 
        + Condition(ele + slo + dis), data = env2) )



# ----------
summary(spechem.physio, axes = 0)



# -->
# Partitioning of variance:
#  - Conditioned:  the amount of variance that has been explained by the covariables and removed.
#  - Constrained:  the amount of variance uniquely explained by the explanatory variables.
#  - Unconstrained:  the residual variance.
# BEAWARE !!!:  proportions are unadjusted and therefore biased.

# Eigenvalues, and their contribution to the variance after removing the contributions of conditioning variables
#  - These values and proportions are partial in the sense that the effects of the covariables have been removed. 
#  - The sum of all these eigenvalues corresponds therefore to the sum of the constrained and nconstrained (residual) variances, excluding the conditioned (i.e. removed) variance.



# ------------------------------------------------------------------------------
# Test of the partial RDA, using the results with the formula interface to allow the tests of the axes to be run
# ------------------------------------------------------------------------------

anova(spechem.physio2, permutations = how(nperm = 999))

anova(spechem.physio2, permutations = how(nperm = 999), by = "axis")



# ----------------------------------------------
# Partial RDA triplots (with fitted site scores) with function triplot.rda
# ----------------------------------------------

# Scaling 1
par(mfrow = c(2, 1))

triplot.rda(spechem.physio, site.sc = "lc", scaling = 1, cex.char2 = 0.8, pos.env = 3, mar.percent = 0)

text(-0.58, 0.64, "a", cex = 2)



# -->
# The sites are not as cleanly ordered by their succession along the river. 
# This indicates that the chemical variables that are important for the fishes do not necessarily follow that order and 
# that the fish community responds significantly to these chemical constraints irrespective of their locations along the river.
# The hardness (har) and nitrates (nit) are less important to explain the fish community structure.
# These two variables are well correlated with the positions of the sites along the river, and therefore their apparent effect on the fish community may have been spurious and 
# has been removed by the analysis, which controlled for the effect of the physiographic variables.



# ----------
# Scaling 2
triplot.rda(spechem.physio, site.sc = "lc", scaling = 2, cex.char2 = 0.8, pos.env = 3, mult.spe = 1.1, mar.percent = 0.04)

text(-3.34, 3.64, "b", cex = 2)




# ----------------------------------------------
# Alternative plot using plot.cca
# ----------------------------------------------

# Partial RDA triplots (with fitted site scores) with function triplot.rda


# ----------
# Scaling 1
plot(spechem.physio, scaling = 1, display = c("sp", "lc", "cn"), main = "Triplot RDA spe.hel ~ chem | Topo - scaling 1 - lc scores")

spe3.sc <- scores(spechem.physio, choices = 1:2, scaling = 1, display = "sp")

arrows(0, 0, spe3.sc[, 1] * 0.92, spe3.sc[, 2] * 0.92, length = 0, lty = 1, col = "red")



# ----------
# Scaling 2
plot(spechem.physio, display = c("sp", "lc", "cn"), main = "Triplot RDA spe.hel ~ chem | Topo - scaling 2 - lc scores")

spe4.sc <- scores(spechem.physio, choices = 1:2, display = "sp")

arrows(0, 0, spe4.sc[, 1] * 0.88, spe4.sc[, 2] * 0.88, length = 0, lty = 1, col = "red")

