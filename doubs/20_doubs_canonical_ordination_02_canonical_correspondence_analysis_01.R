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
# Canonical Correspondence Analysis (CCA)
#   - Basically, CCA is a weighted form of RDA applied to the same matrix Q of contributions to the X^2 statistic as used in CA.
#
#   - One particularly attractive feature of a CCA triplot is that species are ordered along the canonical axes following their ecological optima.
#     This allows a relatively easy ecological interpretation of species assemblages.
#     Also, species scores can be used as synthetic descriptors in a clustering procedure to produce a typology of the species in assemblages.
#
#   - The drawbacks is related to the mathematical properties of the X^2 distance.
#     A difference between abundance values for a common species contributes less to the distance than the same difference for a rare species,
#     so that rare species may have an unduly large influence on the analysis.
#     Its use should be limited to situations where rare species are well sampled and are seen as potential indicators of particular characteristics
#     of an ecosystem; the alternative is to eliminate rare species from the data table before CCA.
#
#   - Differences with RDA output
#        - variation expressed as mean squared contingency coefficient
#        - the species scores are represented by points in the triplot
#        - site scores are weighted averages (instead of weighted sums) of species scores
# ------------------------------------------------------------------------------


# the species data are the raw, untransformed abundances and the explanatory variables are all the ones in object env3
# DO NOT USE Hellinger, log-chord, or chord-transformed data, which are meant to be used with RDA.
# preserved distance would no longer be the X^2 distance and the results could not be interpreted.
# Furthermore, the row sums of the data table, which are used as weights in the CCA regressions,
# have no identifiable meanings for such transformed data.

( spe.cca <- vegan::cca(spe ~ ., env3) )



# default scaling = 2
summary(spe.cca)



# ----------
# unadjusted and adjusted R^2

RsquareAdj(spe.cca)



