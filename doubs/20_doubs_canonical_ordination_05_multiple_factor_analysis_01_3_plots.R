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
# Multiple factor analysis (MFA)
#  - Useful to explore the complex relationships among several ecologically meaningful groups of descriptors, whatever their number and type.
#  - This analysis is correlative:  it does not involve any hypothesis of causal influence of a data set on another.
#  - The variables must belong to the same mathematical type (quantitative or qualitative) within each subset.
#  - If all variables are quantitative, the nMFA is basically a PCA applied to the whole set of variables in which each subset is weighted.
#
#  - DO NOT CONFUSE it with multiple correspondence analysis (MCA) where a single matrix of qualitative variables is submitted to ordination and other matrices
#    may be added as supplementary (passive) information.
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Regroup the 3 tables
# ------------------------------------------------------------------------------

# Hellinger-transform the species dataset
spe.hel <- decostand(spe, "hellinger")



# MFA on 3 groups of variables: Regroup the 3 tables (Hellinger-transformed species, physiographic variables, chemical variables) 
tab3 <- data.frame(spe.hel, envtopo, envchem)

dim(tab3)


# Number of variables in each group
( grn <- c(ncol(spe), ncol(envtopo), ncol(envchem)) )



# ------------------------------------------------------------------------------
# Compute the MFA without multiple plots
# ------------------------------------------------------------------------------

graphics.off()

t3.mfa <- FactoMineR::MFA(tab3, group = grn, 
                          type = c("c", "s", "s"), 
                          ncp = 2, 
                          name.group = c("Fish community", "Physiography", "Water quality"),
                          graph = FALSE)


t3.mfa



summary(t3.mfa)


t3.mfa$ind



# ------------------------------------------------------------------------------
# Plot the results
# Partial axes:  represents the projection of the principal components of each separate PCA on the global PCA
# ------------------------------------------------------------------------------

plot(t3.mfa, choix = "axes", habillage = "group", shadowtext = TRUE)



# -->
# the first 2 axes represent more than 63% of the total variance



# ------------------------------------------------------------------------------
# Plot the results
# Individual Factor Map
# ------------------------------------------------------------------------------

plot(t3.mfa, choix = "ind", partial = "all", habillage = "group")



# -->
# the labelled black points represent the MFA site scores (centroids of the site scores of the 3 separate PCAs)
# they are connected by coloured lines to the points representing their scores in the 3 separate PCAs



# ------------------------------------------------------------------------------
# Plot the results
# Correlation circle:  correlations between the quantitative variabels of each subset on MFA axes 1 and 2
#                      the normalized vectors of all quantitative variables
# ------------------------------------------------------------------------------

plot(t3.mfa, choix = "var", habillage = "group", shadowtext = TRUE)




# -->
# If we examine Individual Factor Map and Correlation Circle together,
# we can recognize the main upstream-downstream gradient along the first axis
# and the gradient of water quality along a combination of the 1st and 2nd axes (from upper left to lower right)

# For example, the scores of sites 1, 2 and 3 correspond to high elevation and strong slope, as well as high oxygen concentration.
# Here, close to the source, the ecological conditions are dominated by physiography.
# The relatively poor fish community is characterized by Satr, Phph and Babl.

# On the opposite side, sites 23, 24 and 25 show the highest concentrations in phosphates, ammonium and nitrates, and a high biological oxygen demand.
# These three sites are heavily polluted and their community is characterized by another set of 3 species: Alal, Ruru and Sqce.



# ----------
plot(t3.mfa, choix = "group")


