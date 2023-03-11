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
# Parsimonious RDA
# ------------------------------------------------------------------------------

( spe.rda.pars <- rda(spe.hel ~ ele + oxy + bod, data = env2) )


summary(spe.rda.pars)



# ------------------------------------------------------------------------------
# Test for significance by permutation test
# ------------------------------------------------------------------------------

# Global test of the RDA result
# The test function is called anova() because it relies on a ratio between two measures of variation.
# DO NOT CONFUSE IT WITH CLASSICAL ANOVA TEST !!

anova(spe.rda.pars, permutations = how(nperm = 999))



# ----------
# Permutation Tests of all canonical axes

anova(spe.rda.pars, by = "axis", permutations = how(nperm = 999))



# -->
# all axes are significant at the alpha = 0.05



# ----------
# Apply Kaiser-Guttman criterion (generally quite liveral) to residual axes

# The eigenvalues are compared with the mean of the residual eigenvalues only

spe.rda.pars$CA$eig

mean(spe.rda.pars$CA$eig)

spe.rda.pars$CA$eig[spe.rda.pars$CA$eig > mean(spe.rda.pars$CA$eig)]



# ------------------------------------------------------------------------------
# Compare the variance inflation factors
# ------------------------------------------------------------------------------

vif.cca(spe.rda.all)

vif.cca(spe.rda.pars)



# ------------------------------------------------------------------------------
# Triplots of the parsimonious RDA (with fitted site scores)
# ------------------------------------------------------------------------------

# Since there is now a third significant canonical axis, you could plot other combinations, axes 1 and 3, 2 and 3

par(mfrow = c(2, 1))


# Scaling 1
triplot.rda(spe.rda.pars, site.sc = "lc", scaling = 1, cex.char2 = 0.8, pos.env = 2, mult.spe = 0.9, mult.arrow = 0.92, mar.percent = 0.01)


# Scaling 2
triplot.rda(spe.rda.pars, site.sc = "lc", scaling = 2, cex.char2 = 0.8, pos.env = 2, mult.spe = 1.1, mar.percent = -0.02)



# -->
# This triplot indeed presents the same structures as the one produced with all explanatory variables.
# The sites and species show the same relationships.




# ------------------------------------------------------------------------------
# Alternate code using plot.cca
# ------------------------------------------------------------------------------

par(mfrow = c(1, 2))


# Scaling 1
plot(spe.rda.pars, scaling = 1, display = c("sp", "lc", "cn"), main = "Triplot RDA spe.hel ~ ele+oxy+bod - scaling 1 - lc scores")
spe4.sc <- scores(spe.rda.pars, choices = 1:2, scaling = 1, display = "sp")
arrows(0, 0, spe4.sc[, 1] * 0.92, spe4.sc[, 2] * 0.92, length = 0, lty = 1, col = "red")


# Scaling 2
plot(spe.rda.pars, display = c("sp", "lc", "cn"), main = "Triplot RDA spe.hel ~ ele+oxy+bod - scaling 2 - lc scores")
spe5.sc <- scores(spe.rda.pars, choices = 1:2, display = "sp")
arrows(0, 0, spe5.sc[, 1] * 0.9, spe5.sc[, 2] * 0.9, length = 0, lty = 1, col = "red")


