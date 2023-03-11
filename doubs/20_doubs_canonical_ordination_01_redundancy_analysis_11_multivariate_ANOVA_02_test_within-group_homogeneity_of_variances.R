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
# Verify multivariate homogeneity of within-cell dispersions
# ------------------------------------------------------------------------------

# betadisper() function (vegan package) can test homogeneity of variance-covariance matrices
# implemented based onMarti Anderson's testing method (Anderson 2006)

# To avoid the risk of heterogeneity of variances with respect to one factor
# because of the dispersion in the other (in case of interaction), 
# creation of a factor crossing the two factors, i.e. defining the cell-by-cell attribution of the data

# The test becomes a test of homogeneity of within-cell dispersions

( cell.fac <- gl(9, 3) )

spe.hel.d1 <- dist(spe.hel[1:27, ])



# ----------
# Test of homogeneity of within-cell dispersions

( spe.hel.cell.MHV <- betadisper(spe.hel.d1, cell.fac) )



# ----------
anova(spe.hel.cell.MHV)     # Parametric test (not recommended here)

permutest(spe.hel.cell.MHV)



# -->
# The within-group dispersions are homogeneous


# ------------------------------------------------------------------------------
# Alternatively, test homogeneity of dispersions within each factor
# ------------------------------------------------------------------------------
# These tests ore more robust with this small example because there are now 9 observations per group instead of 3. 
# Factor "elevation"

( spe.hel.ele.MHV <- betadisper(spe.hel.d1, ele.fac) )

anova(spe.hel.ele.MHV)     # Parametric test (not recommended here)

permutest(spe.hel.ele.MHV) # Permutation test



# ----------
# Factor "pH"
( spe.hel.pH.MHV <- betadisper(spe.hel.d1, pH.fac) )

anova(spe.hel.pH.MHV)

permutest(spe.hel.pH.MHV) # Permutation test


